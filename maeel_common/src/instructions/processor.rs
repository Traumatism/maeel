use super::InstructionData;

pub trait InstructionsProcessor {
    fn handle_push(&mut self, i: InstructionData);

    fn handle_identifier(&mut self, i: InstructionData);

    fn handle_vardef(&mut self, i: InstructionData);

    fn handle_procdef(&mut self, i: InstructionData);

    fn handle_if(&mut self, i: InstructionData);

    fn handle_while(&mut self, i: InstructionData);

    fn handle_for(&mut self, i: InstructionData);

    fn handle_syscall(&mut self, i: InstructionData);

    fn handle_print(&mut self, i: InstructionData);

    fn handle_clear(&mut self, i: InstructionData);

    fn handle_take(&mut self, i: InstructionData);

    fn handle_argv(&mut self, i: InstructionData);

    fn handle_get(&mut self, i: InstructionData);

    fn handle_ptr(&mut self, i: InstructionData);

    fn handle_pop(&mut self, i: InstructionData);

    fn handle_rot(&mut self, i: InstructionData);

    fn handle_dup(&mut self, i: InstructionData);

    fn handle_swap(&mut self, i: InstructionData);

    fn handle_over(&mut self, i: InstructionData);

    fn handle_sub(&mut self, i: InstructionData);

    fn handle_add(&mut self, i: InstructionData);

    fn handle_mul(&mut self, i: InstructionData);

    fn handle_div(&mut self, i: InstructionData);

    fn handle_mod(&mut self, i: InstructionData);

    fn handle_not(&mut self, i: InstructionData);

    fn handle_eq(&mut self, i: InstructionData);

    fn handle_gt(&mut self, i: InstructionData);

    fn handle_lt(&mut self, i: InstructionData);

    fn handle_instructions(&mut self, instructions: Vec<InstructionData>) {
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
