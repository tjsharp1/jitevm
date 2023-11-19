use revm::{
    interpreter::{
        instructions::opcode::{JUMP, JUMPI, PUSH0, PUSH32},
        InstructionResult, Interpreter, OpCode,
    },
    primitives::{db::Database, U256},
    EVMData, Inspector,
};

#[derive(Debug)]
pub struct EVMState {
    pub stack: Vec<U256>,
    pub memory: Vec<u8>,
    pub ip: isize,
}

#[derive(Debug)]
pub struct CycleCountInspector {
    cycle_stop: u64,
    pub final_state: Option<EVMState>,
}

impl CycleCountInspector {
    pub fn new(cycle_stop: u64) -> CycleCountInspector {
        CycleCountInspector {
            cycle_stop,
            final_state: None,
        }
    }
}

impl<DB: Database> Inspector<DB> for CycleCountInspector {
    fn step(&mut self, interp: &mut Interpreter, _context: &mut EVMData<'_, DB>) {
        if self.cycle_stop == 0 {
            let state = EVMState {
                stack: interp.stack.data().clone(),
                memory: interp.shared_memory.context_memory().to_vec(),
                ip: unsafe {
                    interp
                        .instruction_pointer
                        .offset_from(interp.contract.bytecode.as_ptr())
                },
            };
            self.final_state = Some(state);

            interp.instruction_result = InstructionResult::Stop;
        } else {
            // The JIT has fused push-jump instructions, so handle similarly to get the same breakpoint.
            if let Some(op) = OpCode::new(interp.current_opcode()) {
                let op = op.get();
                if (PUSH0..=PUSH32).contains(&op) {
                    let n = (op - PUSH0) + 1;
                    let next_ip = interp.instruction_pointer.wrapping_add(n as usize);

                    let next_op = unsafe { *next_ip };

                    if next_op == JUMP || next_op == JUMPI {
                        interp.instruction_result = InstructionResult::Continue;
                        return;
                    }
                }
            }
            self.cycle_stop -= 1;
            interp.instruction_result = InstructionResult::Continue;
        }
    }
}
