use revm::{
    interpreter::{
        instructions::opcode::JUMPDEST,
        InstructionResult, Interpreter, OpCode,
    },
    primitives::{db::Database, HashMap, U256},
    EVMData, Inspector,
};


#[derive(Debug)]
pub struct JumpdestInspector {
    pub dests: HashMap<String, usize>,
}

impl JumpdestInspector {
    pub fn new() -> JumpdestInspector {
        JumpdestInspector {
            dests: HashMap::new(),
        }
    }
}

impl<DB: Database> Inspector<DB> for JumpdestInspector {
    fn step(&mut self, interp: &mut Interpreter, _context: &mut EVMData<'_, DB>) {
        if interp.current_opcode() == JUMPDEST {
            let base = interp.contract.bytecode.as_ptr();
            let index = unsafe { interp.instruction_pointer.offset_from(base) } as usize;
            let key = format!("0x{:x}", index);
            let count = self.dests.entry(key).or_default();
            *count += 1;
        }

        interp.instruction_result = InstructionResult::Continue;
    }
}
