use imgui::{ListClipper, TableColumnFlags, TableColumnSetup, TableFlags};

use crate::tas::Tas;

use crate::core::trace;

use super::disasm::{self, Disassembler};

pub struct TraceDebugger {
    focus: bool,
    system_id: Option<String>,
    disassembler: Option<Box<dyn Disassembler>>,
}

impl TraceDebugger {
    pub fn new() -> Self {
        TraceDebugger {
            focus: false,
            system_id: None,
            disassembler: None,
        }
    }

    pub fn focus(&mut self) {
        self.focus = true;
    }

    pub fn draw(&mut self, ui: &imgui::Ui, tas: &mut Tas) -> bool {
        if self.system_id != tas.movie().system_id {
            self.system_id = tas.movie().system_id.clone();
            self.disassembler = self
                .system_id
                .as_deref()
                .and_then(|s| disasm::get_disassembler(s));
        }

        let mut opened = true;
        ui.window("Trace Debugger")
            .focused(std::mem::take(&mut self.focus))
            .size([512., 448.], imgui::Condition::FirstUseEver)
            .opened(&mut opened)
            .build(|| {
                let fields = tas.trace_fields().unwrap();
                let pc = fields
                    .iter()
                    .position(|f| f.field_type == trace::FieldType::ProgramCounter);

                if let Some(_t) = ui.begin_table_with_flags(
                    "Instructions",
                    fields.len() + 2,
                    TableFlags::SCROLL_Y,
                ) {
                    let zero_width = ui.calc_text_size("0")[0];
                    let header_column = |field: &trace::Field| {
                        let width = zero_width * field.size as f32 * 2. + 1.;
                        ui.table_setup_column_with(TableColumnSetup {
                            init_width_or_weight: width,
                            flags: TableColumnFlags::WIDTH_FIXED,
                            ..TableColumnSetup::new(&field.name)
                        });
                    };

                    ui.table_setup_column_with(TableColumnSetup {
                        init_width_or_weight: zero_width * 10.,
                        flags: TableColumnFlags::WIDTH_FIXED,
                        ..TableColumnSetup::new("Cycle")
                    });
                    if let Some(pc) = pc {
                        header_column(&fields[pc]);
                    }
                    ui.table_setup_column("Instruction");
                    for (i, field) in fields.iter().enumerate() {
                        if Some(i) == pc {
                            continue;
                        }
                        header_column(field);
                    }

                    ui.table_setup_scroll_freeze(1, 1);
                    ui.table_headers_row();

                    if let Some(trace) = tas.trace() {
                        let count = trace.into_iter().count();

                        let clipper = ListClipper::new(count as i32)
                            .items_height(ui.text_line_height_with_spacing())
                            .begin(ui);
                        let mut cursor = trace.into_iter().enumerate();

                        for i in clipper.iter() {
                            let i = i as usize;
                            let (mut j, mut entry) = cursor.next().unwrap();
                            if j > i {
                                cursor = trace.into_iter().enumerate();
                                (j, entry) = cursor.next().unwrap();
                            }
                            while j < i {
                                (j, entry) = cursor.next().unwrap();
                            }

                            let column = |idx| {
                                let field = entry.get(idx);
                                ui.table_next_column();
                                ui.text(format!(
                                    "{data:0width$x}",
                                    data = field.data,
                                    width = field.field.size * 2
                                ));
                            };

                            // Draw entry
                            ui.table_next_column();
                            ui.text(format!("{}", entry.cycle()));
                            if let Some(pc) = pc {
                                column(pc);
                            }

                            ui.table_next_column();

                            if let Some(disasm) = self.disassembler.as_mut() {
                                ui.text(disasm.disassemble(entry));
                            } else {
                                let mut instr_bytes = String::new();
                                for byte in entry.instruction_bytes() {
                                    instr_bytes.push_str(&format!("{byte:02x} "));
                                }
                                ui.text(&instr_bytes);
                            }

                            for field in 0..fields.len() {
                                if Some(field) != pc {
                                    column(field);
                                }
                            }

                            ui.table_next_row();
                        }
                    }
                }
            });

        opened
    }
}
