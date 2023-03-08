use super::InstructionData;

pub trait InstructionsProcessor {
    fn handle_push(&self, i: InstructionData);
    fn handle_identifier(&self, i: InstructionData);
    fn handle_vardef(&self, i: InstructionData);
    fn handle_procdef(&self, i: InstructionData);
    fn handle_if(&self, i: InstructionData);
    fn handle_while(&self, i: InstructionData);
    fn handle_for(&self, i: InstructionData);
    fn handle_syscall(&self, i: InstructionData);
    fn handle_print(&self, i: InstructionData);
    fn handle_clear(&self, i: InstructionData);
    fn handle_take(&self, i: InstructionData);
    fn handle_argv(&self, i: InstructionData);
    fn handle_get(&self, i: InstructionData);
    fn handle_ptr(&self, i: InstructionData);
    fn handle_pop(&self, i: InstructionData);
    fn handle_rot(&self, i: InstructionData);
    fn handle_dup(&self, i: InstructionData);
    fn handle_swap(&self, i: InstructionData);
    fn handle_over(&self, i: InstructionData);
    fn handle_sub(&self, i: InstructionData);
    fn handle_add(&self, i: InstructionData);
    fn handle_mul(&self, i: InstructionData);
    fn handle_div(&self, i: InstructionData);
    fn handle_mod(&self, i: InstructionData);
    fn handle_not(&self, i: InstructionData);
    fn handle_eq(&self, i: InstructionData);
    fn handle_gt(&self, i: InstructionData);
    fn handle_lt(&self, i: InstructionData);

    fn handle_instructions(&self, instructions: Vec<InstructionData>) {
        for instruction_data in instructions {
            let instruction = instruction_data.instruction.clone();

            match instruction {
                super::Instruction::Pass => continue,
                super::Instruction::Push(_) => self.handle_push(instruction_data),
                super::Instruction::Take => self.handle_take(instruction_data),
                super::Instruction::Get => self.handle_get(instruction_data),
                super::Instruction::Identifier(_) => self.handle_identifier(instruction_data),
                super::Instruction::VarDef(_, _) => self.handle_vardef(instruction_data),
                super::Instruction::ProcDef(_, _) => self.handle_procdef(instruction_data),
                super::Instruction::If(_) => self.handle_if(instruction_data),
                super::Instruction::While(_) => self.handle_while(instruction_data),
                super::Instruction::For(_) => self.handle_for(instruction_data),
                super::Instruction::Ptr => self.handle_ptr(instruction_data),
                super::Instruction::Argv => self.handle_argv(instruction_data),
                super::Instruction::Print => self.handle_print(instruction_data),
                super::Instruction::Syscall => self.handle_syscall(instruction_data),
                super::Instruction::Pop => self.handle_pop(instruction_data),
                super::Instruction::Rot => self.handle_rot(instruction_data),
                super::Instruction::Dup => self.handle_dup(instruction_data),
                super::Instruction::Swap => self.handle_swap(instruction_data),
                super::Instruction::Clear => self.handle_clear(instruction_data),
                super::Instruction::Over => self.handle_over(instruction_data),
                super::Instruction::Sub => self.handle_sub(instruction_data),
                super::Instruction::Add => self.handle_add(instruction_data),
                super::Instruction::Mul => self.handle_mul(instruction_data),
                super::Instruction::Mod => self.handle_mod(instruction_data),
                super::Instruction::Div => self.handle_div(instruction_data),
                super::Instruction::Not => self.handle_not(instruction_data),
                super::Instruction::Eq => self.handle_eq(instruction_data),
                super::Instruction::Gt => self.handle_gt(instruction_data),
                super::Instruction::Lt => self.handle_lt(instruction_data),
            }
        }
    }
}
