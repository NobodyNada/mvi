use std::sync::Arc;

use crate::core::trace;

use super::Disassembler;

pub struct SnesDisassembler {
    fields: Option<Fields>,
}

struct Fields {
    _arc: Arc<trace::Fields>,
    p: *const trace::Field,
}

impl Fields {
    fn new(entry: trace::Entry<'_>) -> Self {
        Fields {
            _arc: entry.fields.clone(),
            p: entry
                .fields
                .fields
                .iter()
                .find(|f| f.name == "P")
                .expect("Missing trace field for processor status register"),
        }
    }

    fn p(&self) -> &trace::Field {
        // SAFETY: p is owned by _arc
        unsafe { &*self.p }
    }
}

impl SnesDisassembler {
    pub fn new() -> Self {
        SnesDisassembler { fields: None }
    }
}

impl Disassembler for SnesDisassembler {
    fn disassemble(&mut self, entry: crate::core::trace::Entry<'_>) -> String {
        let instr = entry.instruction_bytes();
        let Instruction {
            mnemonic: op,
            addressing_mode,
        } = INSTRUCTIONS[instr[0] as usize];

        let flags = entry
            .get_field(self.fields.get_or_insert_with(|| Fields::new(entry)).p())
            .data;

        let byte = || instr[1];
        let word = || u16::from_le_bytes([instr[1], instr[2]]);
        let long = || u32::from_le_bytes([instr[1], instr[2], instr[3], 0]);
        let imm = |byte_flag| {
            if byte_flag {
                format!("{op} #${:02x}", byte())
            } else {
                format!("{op} #${:04x}", word())
            }
        };

        match addressing_mode {
            AddressingMode::Implied => op.to_string(),
            AddressingMode::Immediate8 => imm(true),
            AddressingMode::ImmediateA => imm(flags & 0x20 != 0),
            AddressingMode::ImmediateXY => imm(flags & 0x10 != 0),
            AddressingMode::Direct => format!("{op} ${:02x}", byte()),
            AddressingMode::DirectX => format!("{op} ${:02x}, x", byte()),
            AddressingMode::DirectY => format!("{op} ${:02x}, y", byte()),
            AddressingMode::Indirect => format!("{op} (${:02x})", byte()),
            AddressingMode::IndirectX => format!("{op} (${:02x}, x)", byte()),
            AddressingMode::IndirectY => format!("{op} (${:02x}), y", byte()),
            AddressingMode::IndirectLong => format!("{op} [${:02x}]", byte()),
            AddressingMode::IndirectLongY => format!("{op} [${:02x}], y", byte()),
            AddressingMode::IndirectStackY => format!("{op} (${:02x}, s), y", byte()),
            AddressingMode::Abs => format!("{op} ${:04x}", word()),
            AddressingMode::AbsX => format!("{op} ${:04x}, x", word()),
            AddressingMode::AbsY => format!("{op} ${:04x}, y", word()),
            AddressingMode::AbsLong => format!("{op} ${:06x}", long()),
            AddressingMode::AbsLongX => format!("{op} ${:06x}", long()),
            AddressingMode::IndirectAbs => format!("{op} (${:04x})", word()),
            AddressingMode::IndirectAbsX => format!("{op} (${:04x}), x", word()),
            AddressingMode::IndirectAbsLong => format!("{op} [${:04x}]", word()),
            AddressingMode::Rel8 => format!("{op} .+${:02x}", byte()),
            AddressingMode::Rel16 => format!("{op} .+${:04x}", word()),
            AddressingMode::Stack => format!("{op} ${:02x}, s", byte()),
            AddressingMode::BlockMove => format!("{op} ${:02x}, ${:02x}", instr[2], instr[1]),
        }
    }
}

#[derive(Clone, Copy)]
struct Instruction {
    mnemonic: &'static str,
    addressing_mode: AddressingMode,
}

#[derive(Clone, Copy)]
enum AddressingMode {
    Implied,
    Immediate8,
    ImmediateA,
    ImmediateXY,
    Direct,
    DirectX,
    DirectY,
    Indirect,
    IndirectX,
    IndirectY,
    IndirectLong,
    IndirectLongY,
    IndirectStackY,
    Abs,
    AbsX,
    AbsY,
    AbsLong,
    AbsLongX,
    IndirectAbs,
    IndirectAbsX,
    IndirectAbsLong,
    Rel8,
    Rel16,
    Stack,
    BlockMove,
}

macro_rules! i {
    ($op:ident, $mode:ident) => {
        Instruction {
            mnemonic: stringify!($op),
            addressing_mode: AddressingMode::$mode,
        }
    };

    ($op:ident) => {
        i!($op, Implied)
    };
    ($op:ident #imm8) => {
        i!($op, Immediate8)
    };
    ($op:ident #immA) => {
        i!($op, ImmediateA)
    };
    ($op:ident #immXY) => {
        i!($op, ImmediateXY)
    };
    ($op:ident dd) => {
        i!($op, Direct)
    };
    ($op:ident dd, x) => {
        i!($op, DirectX)
    };
    ($op:ident dd, y) => {
        i!($op, DirectY)
    };
    ($op:ident (dd)) => {
        i!($op, Indirect)
    };
    ($op:ident (dd, x)) => {
        i!($op, IndirectX)
    };
    ($op:ident (dd), y) => {
        i!($op, IndirectY)
    };
    ($op:ident [dd]) => {
        i!($op, IndirectLong)
    };
    ($op:ident [dd], y) => {
        i!($op, IndirectLongY)
    };
    ($op:ident (dd, s), y) => {
        i!($op, IndirectStackY)
    };
    ($op:ident aaaa) => {
        i!($op, Abs)
    };
    ($op:ident aaaa, x) => {
        i!($op, AbsX)
    };
    ($op:ident aaaa, y) => {
        i!($op, AbsY)
    };
    ($op:ident aaaaaa) => {
        i!($op, AbsLong)
    };
    ($op:ident aaaaaa, x) => {
        i!($op, AbsLongX)
    };
    ($op:ident aaaaaa, y) => {
        i!($op, AbsLongY)
    };
    ($op:ident (aaaa)) => {
        i!($op, IndirectAbs)
    };
    ($op:ident (aaaa, x)) => {
        i!($op, IndirectAbsX)
    };
    ($op:ident [aaaa]) => {
        i!($op, IndirectAbsLong)
    };
    ($op:ident rr) => {
        i!($op, Rel8)
    };
    ($op:ident rrrr) => {
        i!($op, Rel16)
    };
    ($op:ident dd, s) => {
        i!($op, Stack)
    };
    ($op:ident mv) => {
        i!($op, BlockMove)
    };
}

#[rustfmt::skip]
static INSTRUCTIONS: [Instruction; 256] = [
    // 0x00
    i!{BRK},            i!{ORA (dd, x)},    i!{COP #imm8},      i!{ORA dd, s},
    i!{TSB dd},         i!{ORA dd},         i!{ASL dd},         i!{ORA [dd]},
    i!{PHP},            i!{ORA #immA},      i!{ASL},            i!{PHD},
    i!{TSB dd},         i!{ORA aaaa},       i!{ASL aaaa},       i!{ORA aaaaaa},

    // 0x10
    i!{BPL rr},         i!{ORA (dd), y},    i!{ORA (dd)},       i!{ORA (dd,s), y},
    i!{TRB dd},         i!{ORA dd, x},      i!{ASL dd, x},      i!{ORA [dd], y},
    i!{CLC},            i!{ORA aaaa, y},    i!{INC},            i!{TCS},
    i!{TRB aaaa},       i!{ORA aaaa, x},    i!{ASL aaaa, x},    i!{ORA aaaaaa, x},

    // 0x20
    i!{JSR aaaa},       i!{AND (dd, x)},    i!{JSL aaaaaa},     i!{AND dd, s},
    i!{BIT dd},         i!{AND dd},         i!{ROL dd},         i!{AND [dd]},
    i!{PLP},            i!{AND #immA},      i!{ROL},            i!{PLD}, 
    i!{BIT aaaa},       i!{AND aaaa},       i!{ROL aaaa},       i!{AND aaaaaa},

    // 0x30
    i!{BMI rr},         i!{AND (dd), y},    i!{AND (dd)},       i!{AND (dd, s), y},
    i!{BIT dd, x},      i!{AND dd, x},      i!{ROL dd, x},      i!{AND [dd], y},
    i!{SEC},            i!{AND aaaa, y},    i!{DEC},            i!{TSC},
    i!{BIT aaaa, x},    i!{AND aaaa, x},    i!{ROL aaaa, x},    i!{AND aaaaaa, x},

    // 0x40
    i!{RTI},            i!{EOR (dd, x)},    i!{WDM},            i!{EOR dd, s},
    i!{MVP mv},         i!{EOR dd},         i!{LSR dd},         i!{EOR [dd]},
    i!{PHA},            i!{EOR #immA},      i!{LSR},            i!{PHK}, 
    i!{JMP aaaa},       i!{EOR aaaa},       i!{LSR aaaa},       i!{EOR aaaaaa},

    // 0x50
    i!{BVC rr},         i!{EOR (dd), y},    i!{EOR (dd)},       i!{EOR (dd, s), y},
    i!{MVN mv},         i!{EOR dd, x},      i!{LSR dd, x},      i!{EOR [dd], y},
    i!{CLI},            i!{EOR aaaa, y},    i!{PHY},            i!{TCD},
    i!{JML aaaaaa},     i!{EOR aaaa, x},    i!{LSR aaaa, x},    i!{EOR aaaaaa, x},

    // 0x60
    i!{RTS},            i!{ADC (dd, x)},    i!{PER rrrr},       i!{ADC dd, s},
    i!{STZ dd},         i!{ADC dd},         i!{ROR dd},         i!{ADC [dd]},
    i!{PLA},            i!{ADC #immA},      i!{ROR},            i!{RTL}, 
    i!{JMP (aaaa)},     i!{ADC aaaa},       i!{ROR aaaa},       i!{ADC aaaaaa},

    // 0x70
    i!{BVS rr},         i!{ADC (dd), y},    i!{ADC (dd)},       i!{ADC (dd, s), y},
    i!{STZ dd, x},      i!{ADC dd, x},      i!{ROR dd, x},      i!{ADC [dd], y},
    i!{SEI},            i!{ADC aaaa, y},    i!{PLY},            i!{TDC},
    i!{JMP [aaaa]},   i!{ADC aaaa, x},    i!{ROR aaaa, x},    i!{ADC aaaaaa, x},

    // 0x80
    i!{BRA rr},         i!{STA (dd, x)},    i!{BRL rrrr},       i!{STA dd, s},
    i!{STY dd},         i!{STA dd},         i!{STX dd},         i!{STA [dd]},
    i!{DEY},            i!{BIT #immA},      i!{TXA},            i!{PHB}, 
    i!{STY aaaa},       i!{STA aaaa},       i!{STX aaaa},       i!{STA aaaaaa},

    // 0x90
    i!{BCC rr},         i!{STA (dd), y},    i!{STA (dd)},       i!{STA (dd, s), y},
    i!{STY dd, x},      i!{STA dd, x},      i!{STX dd, y},      i!{STA [dd], y},
    i!{TYA},            i!{STA aaaa, y},    i!{TXS},            i!{TXY},
    i!{STZ aaaa},       i!{STA aaaa, x},    i!{STZ aaaa, x},    i!{STA aaaaaa, x},

    // 0xA0
    i!{LDY #immXY},     i!{LDA (dd, x)},    i!{LDX #immXY},     i!{LDA dd, s},
    i!{LDY dd},         i!{LDA dd},         i!{LDX dd},         i!{LDA [dd]},
    i!{TAY},            i!{LDA #immA},      i!{TAX},            i!{PLB}, 
    i!{LDY aaaa},       i!{LDA aaaa},       i!{LDX aaaa},       i!{LDA aaaaaa},

    // 0xB0
    i!{BCS rr},         i!{LDA (dd), y},    i!{LDA (dd)},       i!{LDA (dd, s), y},
    i!{LDY dd, x},      i!{LDA dd, x},      i!{LDX dd, y},      i!{LDA [dd], y},
    i!{CLV},            i!{LDA aaaa, y},    i!{TSX},            i!{TYX},
    i!{LDY aaaa, x},    i!{LDA aaaa, x},    i!{LDX aaaa, y},    i!{LDA aaaaaa, x},

    // 0xC0
    i!{CPY #immXY},     i!{CMP (dd, x)},    i!{REP #imm8},      i!{CMP dd, s},
    i!{CPY dd},         i!{CMP dd},         i!{DEC dd},         i!{CMP [dd]},
    i!{INY},            i!{CMP #immA},      i!{DEX},            i!{WAI}, 
    i!{CPY aaaa},       i!{CMP aaaa},       i!{DEC aaaa},       i!{CMP aaaaaa},

    // 0xD0
    i!{BNE rr},         i!{CMP (dd), y},    i!{CMP (dd)},       i!{CMP (dd, s), y},
    i!{PEI (dd)},       i!{CMP dd, x},      i!{DEC dd, x},      i!{CMP [dd], y},
    i!{CLD},            i!{CMP aaaa, y},    i!{PHX},            i!{STP},
    i!{JMP (aaaa, x)},  i!{CMP aaaa, x},    i!{DEC aaaa, x},    i!{CMP aaaaaa, x},

    // 0xE0
    i!{CPX #immXY},     i!{SBC (dd, x)},    i!{SEP #imm8},      i!{SBC dd, s},
    i!{CPX dd},         i!{SBC dd},         i!{INC dd},         i!{SBC [dd]},
    i!{INX},            i!{SBC #immA},      i!{NOP},            i!{XBA}, 
    i!{CPX aaaa},       i!{SBC aaaa},       i!{INC aaaa},       i!{SBC aaaaaa},

    // 0xF0
    i!{BEQ rr},         i!{SBC (dd), y},    i!{SBC (dd)},       i!{SBC (dd, s), y},
    i!{PEA #imm8},      i!{SBC dd, x},      i!{INC dd, x},      i!{SBC [dd], y},
    i!{SED},            i!{SBC aaaa, y},    i!{PLX},            i!{XCE},
    i!{JSR (aaaa, x)},  i!{SBC aaaa, x},    i!{INC aaaa, x},    i!{SBC aaaaaa, x},
];
