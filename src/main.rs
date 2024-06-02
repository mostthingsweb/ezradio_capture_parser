use std::{fmt, fs};
use std::collections::HashMap;
use std::io::Read;
use bitvec::order::Msb0;
use bitvec::vec::BitVec;

use clap::Parser;
use itertools::Itertools;
use scraper::{Element, ElementRef, Html, Selector};
use tabled::builder::Builder;
use tabled::settings::{Alignment, Span, Style};
use tabled::settings::object::Cell;
use tabled::settings::style::HorizontalLine;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    file: String,
}


macro_rules! selector {
    ($x:literal) => {
        &Selector::parse($x).unwrap()
    };
}

struct Packet {
    timestamp: f32,
    number: Option<u64>,
    mosi_data: u8,
    miso_data: u8,
}

impl fmt::Debug for Packet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Packet")
            .field("timestamp", &self.timestamp)
            .field("number", &self.number)
            .field("mosi_data", &format_args!("0x{:02X}", self.mosi_data))
            .field("miso_data", &format_args!("0x{:02X}", self.miso_data))
            .finish()
    }
}

fn parse_hex_string<S: AsRef<str>>(s: S) -> u8 {
    u8::from_str_radix(s.as_ref().trim_start_matches("0x"), 16).unwrap()
}

#[derive(Debug)]
struct DontCare;

#[derive(Debug)]
struct FixedField {
    name: String,
    width: u8,
}

#[derive(Debug)]
enum Field {
    DontCare,
    Constant(bool),
    FixedWidth(FixedField),
}

impl Field {
    fn name(&self) -> Option<&str> {
        match self {
            Self::DontCare => None,
            Self::FixedWidth(f) => Some(f.name.as_ref()),
            Self::Constant(_) => None,
        }
    }

    fn width(&self) -> u8 {
        match self {
            Self::DontCare | Self::Constant(..) => 1,
            Self::FixedWidth(f) => f.width,
        }
    }
}

#[derive(Debug)]
struct Register {
    name: String,
    fields: Vec<Field>,
}

#[derive(Debug)]
struct RegisterTable(Vec<Register>);


fn parse_register_table(e: ElementRef) -> RegisterTable {
    let mut registers = vec![];

    for tbody in e.select(selector!("tbody")) {
        if !tbody.attr("id").unwrap_or_default().starts_with("sum_cmd_") {
            continue;
        }

        let name2;
        if let Some(name) = tbody.select(selector!(".reg-view-name")).next() {
            name2 = name.text().collect_vec().join(" ");
        } else {
            name2 = String::new();
        }

        for tr in tbody.select(selector!("tr")) {
            let mut argument = vec![];
            let mut width: u8 = 0;

            let skip;
            let mut is_numbers = false;
            if tr.select(selector!(".reg-view-name")).next().is_some() {
                skip = 3;
            } else if tr.select(selector!("td.numbers")).next().is_some() {
                skip = 3;
                is_numbers = true;
            } else {
                skip = 1;
            }

            for td in tr.select(selector!("td")).skip(skip) {
                let c = td.attr("class").unwrap_or_default().split_whitespace().collect_vec();
                let field_width = td.attr("colspan").unwrap().parse::<u8>().unwrap();

                if c.contains(&"fixed-width-bit") {
                    let field_name: String = td.text().join(" ");

                    let constant = if field_name.len() == 1 {
                        field_name.parse::<u8>().ok().filter(|&c2| c2 <= 1)
                    } else {
                        None
                    };

                    if let Some(constant) = constant {
                        argument.push(Field::Constant(constant == 1));
                    } else {
                        argument.push(Field::FixedWidth(FixedField {
                            name: field_name,
                            width: field_width,
                        }));
                    }
                }

                width += field_width;
            }

            assert_eq!(width, 8);

            if argument.len() > 1 {
                let prefix = name2.clone();
                for arg in argument.iter() {
                    //assert_eq!(&prefix, arg.name().unwrap());
                }
            }

            registers.push(Register {
                fields: argument,
                name: name2.clone(),
            });
        }
    }

    RegisterTable(registers)
}

#[derive(Debug)]
pub struct Command {
    name: String,
    code: u8,
    summary: String,
    register_table: RegisterTable,
    reply_stream: Option<RegisterTable>,
}

fn main() {
    let args = Args::parse();

    let api_html = include_str!("../resources/EZRadioPRO_REVC2_API/EZRadioPRO_REVC2/Si4463/revC2A/index_all.html");
    let doc = Html::parse_document(api_html);

    let summary_selector = Selector::parse(".sum-sum").unwrap();
    let number_selector = Selector::parse(".sum-number").unwrap();
    let href_selector = Selector::parse(".sum-number + td a[href]").unwrap();

    let command_rows = Selector::parse(".sum-table tbody[id^=sum_cmd_]").unwrap();

    let li_selector = Selector::parse("li").unwrap();
    let table_selector = Selector::parse("table").unwrap();

    let mut commands: HashMap<u8, Command> = HashMap::new();
    for c in doc.select(&command_rows) {
        if c.attr("id").unwrap().starts_with("sum_cmd_set_") {
            continue;
        }

        let command_summary = c.select(&summary_selector).next().unwrap().text().join(" ");
        let command_number = parse_hex_string(c.select(&number_selector).next().unwrap().text().join(" "));

        let command_name_link = c.select(&href_selector).next().unwrap();
        let command_name = command_name_link.text().join(" ");
        let command_href = command_name_link.attr("href").unwrap().trim_start_matches('#');

        let command_details_selector = Selector::parse(&format!("[title={}]", command_href)).unwrap();
        let details = doc.select(&command_details_selector).next().unwrap();
        let details_parent = details.parent_element().unwrap();
        let ul = details_parent.next_sibling_element().unwrap();
        let mut register_table = None;
        let mut reply_table = None;
        for li in ul.select(&li_selector) {
            let li_text = li.text().collect_vec().join(" ");
            if li_text.starts_with("Argument Stream:") {
                let table_element = li.select(&table_selector).next().unwrap();
                assert!(register_table.is_none());
                register_table = Some(parse_register_table(table_element));
            } else if li_text.starts_with("Reply Stream:") {
                let table_element = li.select(&table_selector).next().unwrap();
                assert!(reply_table.is_none());
                reply_table = Some(parse_register_table(table_element));
            }
        }

        commands.insert(command_number, Command {
            name: command_name,
            code: command_number,
            summary: command_summary,
            register_table: register_table.unwrap(),
            reply_stream: reply_table,
        });
    }

    let packets = fs::read_to_string(args.file)
        .unwrap()
        .lines()
        .skip(1)
        .map(|line| {
            let mut parts = line.split(',');
            Packet {
                timestamp: parts.next().unwrap().parse().unwrap(),
                number: parts.next().and_then(|s| {
                    if s.is_empty() {
                        None
                    } else {
                        Some(s.parse().unwrap())
                    }
                }),
                mosi_data: u8::from_str_radix(parts.next().map(|s| {
                    s.trim_start_matches("0x")
                }).unwrap(), 16).unwrap(),
                miso_data: u8::from_str_radix(parts.next().map(|s| {
                    s.trim_start_matches("0x")
                }).unwrap(), 16).unwrap(),
            }
        }).collect::<Vec<_>>();
    //
    // let transactions = packets.into_iter().chunk_by(|p| p.number);
    //
    // let transactions = transactions
    //     .into_iter()
    //     .map(|t| t.1.collect::<Vec<_>>())
    //     .collect::<Vec<_>>();

    #[derive(Debug)]
    struct Transaction<'a> {
        command: u8,
        arguments: Vec<&'a Packet>,
        reply: Vec<&'a Packet>,
    }

    impl<'a> Transaction<'a> {
        fn new(command: u8) -> Transaction<'a> {
            Transaction {
                command,
                arguments: vec![],
                reply: vec![],
            }
        }
    }

    let mut transactions: Vec<Transaction> = vec![];
    let mut current_transaction: Option<Transaction> = None;

    #[derive(Debug, Copy, Clone)]
    enum State {
        Idle,
        FastCommandSent, // TODO
        CommandSent,
        ReadCmdBuffSent,
        ReadingResponse,
    }

    let mut state = State::Idle;

    for packet in &packets {
        let old_state = state;
        eprintln!("{:#?}", packet);
        match state {
            State::Idle => {
                assert_ne!(packet.mosi_data, 0x44);
                current_transaction = Some(Transaction::new(packet.mosi_data));
                state = State::CommandSent;
            }
            State::CommandSent => {
                if packet.mosi_data == 0x44 {
                    state = State::ReadCmdBuffSent;
                } else {
                    // This is a command arg
                    current_transaction.as_mut().unwrap().arguments.push(packet);
                }
            }
            State::ReadCmdBuffSent => {
                if packet.miso_data == 0xFF {
                    state = State::ReadingResponse;
                } else {
                    state = State::CommandSent;
                }
            }
            State::ReadingResponse => {
                match packet.mosi_data {
                    0xFF => current_transaction.as_mut().unwrap().reply.push(packet),
                    0x44 => state = State::ReadCmdBuffSent,
                    _ => {
                        transactions.push(current_transaction.take().unwrap());
                        current_transaction = Some(Transaction::new(packet.mosi_data));
                        state = State::CommandSent;
                    }
                }
            }
            _ => unreachable!()
        }

        eprintln!("  state: {:?} => {:?}", old_state, state);
    }

    if let Some(current_transaction) = current_transaction {
        transactions.push(current_transaction);
    }

    eprintln!("{:#?}", transactions);

    for transaction in &transactions {
        eprintln!("{:#?}", transaction);

        let command_code = transaction.command;
        // Ignore internal commands (https://github.com/astuder/Inside-EZRadioPRO/blob/master/docs/api-cmd.md)
        match command_code {
            0x4 => {
                // TODO: PATCH_IMAGE
                continue;
            }
            0x5 => {
                // TODO: PATCH_ARGS
                continue;
            }
            c if c & 0xE0 == 0xE0 => {
                // TODO: PATCH_DATA
                continue;
            }
            _ => {}
        }

        let command = commands.get(&command_code).unwrap();

        let mut builder = Builder::default();
        builder.push_record(vec![format!("{} Arguments", command.name)]);
        builder.push_record(vec!["Index", "Name", "7", "6", "5", "4", "3", "2", "1", "0"]);

        builder.push_record(vec!["0x00", "CMD", &format!("0x{:02X}", command.code)]);

        let mut modifications = vec![];
        let mut i = 3;

        let arg_stream = command.register_table.0.iter().skip(1);
        for (packet, register) in transaction.arguments.iter().zip(arg_stream) {
            let mut cell_i = 2;

            let mut v: BitVec<_, Msb0> = BitVec::from_element(packet.mosi_data);
            eprintln!("0x{:02X} => {:?}", packet.mosi_data, v);

            let mut record = vec![String::from("0x01"), register.name.clone()];

            for field in &register.fields {
                let remainder = v.split_off(field.width() as usize);

                match field {
                    Field::Constant(c) => {
                        let c = if *c {String::from("1") } else{ String::from("0") };
                        record.push(c);
                    }
                    Field::DontCare => record.push(String::from("X")),
                    Field::FixedWidth(f) => {
                        record.push(format!("{}: 0x{:02X}", f.name, v.into_vec()[0]));
                    }
                }

                v = remainder;
                modifications.push((Cell::new(i, cell_i), Span::column(field.width() as usize)));
                cell_i += field.width() as usize;
            }

            builder.push_record(record);
            i += 1;
        }

        let mut table = builder.build();
        table.modify((0, 0), Span::column(10)).with(Alignment::center());
        table.modify(Cell::new(2, 2), Span::column(8)).with(Alignment::center());

        for m in modifications.drain(..) {
            table.modify(m.0, m.1).with(Alignment::center());
        }

        eprintln!("{}", table.with(Style::modern()));


        if let Some(reply_stream) = command.reply_stream.as_ref() {
            let mut builder = Builder::default();
            builder.push_record(vec![format!("{} Reply", command.name)]);
            builder.push_record(vec!["Index", "Name", "7", "6", "5", "4", "3", "2", "1", "0"]);

            let mut modifications = vec![];
            let mut i = 2;

            // Skip CTS
            for zipped in transaction.reply.iter().zip_longest(reply_stream.0.iter().skip(1)) {
                eprintln!("{:#?}", zipped);

                let (packet, register) = zipped.left_and_right();
                let packet = packet.unwrap();
                let register = register.unwrap_or(reply_stream.0.last().unwrap());
                let mut cell_i = 2;

                let mut v: BitVec<_, Msb0> = BitVec::from_element(packet.mosi_data);
                eprintln!("0x{:02X} => {:?}", packet.mosi_data, v);

                let mut record = vec![String::from("0x01"), register.name.clone()];

                for field in &register.fields {
                    let remainder = v.split_off(field.width() as usize);

                    match field {
                        Field::Constant(c) => {
                            let c = if *c {String::from("1") } else{ String::from("0") };
                            record.push(c);
                        }
                        Field::DontCare => record.push(String::from("X")),
                        Field::FixedWidth(f) => {
                            record.push(format!("{}: 0x{:02X}", f.name, v.into_vec()[0]));
                        }
                    }

                    v = remainder;
                    modifications.push((Cell::new(i, cell_i), Span::column(field.width() as usize)));
                    cell_i += field.width() as usize;
                }

                builder.push_record(record);
                i += 1;
            }

            let mut table = builder.build();
            table.modify((0, 0), Span::column(10)).with(Alignment::center());

            for m in modifications.drain(..) {
                table.modify(m.0, m.1).with(Alignment::center());
            }

            eprintln!("{}", table.with(Style::modern()));
        }
    }

    //eprintln!("{:#?}", transactions);
}
