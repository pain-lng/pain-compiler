// IR Optimizations module

use crate::ir::*;
use std::collections::{HashMap, HashSet};

/// Optimization pass manager
pub struct Optimizer;

impl Optimizer {
    /// Run all optimization passes on IR program
    pub fn optimize(ir: IrProgram) -> IrProgram {
        let mut ir = ir;
        
        // Run optimization passes in order
        ir = Self::constant_folding(&ir);
        ir = Self::common_subexpression_elimination(&ir);
        ir = Self::dead_code_elimination(&ir);
        
        ir
    }
    
    /// Constant folding: evaluate constant expressions at compile time
    fn constant_folding(ir: &IrProgram) -> IrProgram {
        let mut new_ir = ir.clone();
        
        for func in &mut new_ir.functions {
            let mut const_map: HashMap<ValueId, Instruction> = HashMap::new();
            let mut changed = true;
            
            // Iterate until no more constants can be folded
            while changed {
                changed = false;
                
                // First pass: identify constant values and fold them
                for block in &func.blocks {
                    for (value_id, instr) in &block.instructions {
                        if const_map.contains_key(value_id) {
                            continue;
                        }
                        
                        // Check if this is already a constant
                        if Self::is_immediate_constant(instr) {
                            const_map.insert(*value_id, instr.clone());
                            changed = true;
                            continue;
                        }
                        
                        // Try to fold if all operands are constants
                        if Self::is_constant_instruction(instr, &const_map) {
                            if let Some(const_instr) = Self::fold_instruction(instr, &const_map) {
                                const_map.insert(*value_id, const_instr);
                                changed = true;
                            }
                        }
                    }
                }
            }
            
            // Second pass: replace folded instructions with constants
            for block in &mut func.blocks {
                let mut new_instructions = Vec::new();
                
                for (value_id, instr) in &block.instructions {
                    if let Some(const_instr) = const_map.get(value_id) {
                        // Replace with constant
                        new_instructions.push((*value_id, const_instr.clone()));
                    } else {
                        // Keep original instruction, but update operands if they were folded
                        let new_instr = Self::replace_with_constants(instr, &const_map);
                        new_instructions.push((*value_id, new_instr));
                    }
                }
                
                block.instructions = new_instructions;
            }
        }
        
        new_ir
    }
    
    /// Check if instruction is an immediate constant
    fn is_immediate_constant(instr: &Instruction) -> bool {
        matches!(
            instr,
            Instruction::ConstInt { .. }
                | Instruction::ConstFloat { .. }
                | Instruction::ConstBool { .. }
                | Instruction::ConstString { .. }
                | Instruction::ConstUnit
        )
    }
    
    /// Check if instruction is a constant or can be folded
    fn is_constant_instruction(instr: &Instruction, const_map: &HashMap<ValueId, Instruction>) -> bool {
        match instr {
            Instruction::ConstInt { .. } 
            | Instruction::ConstFloat { .. }
            | Instruction::ConstBool { .. }
            | Instruction::ConstString { .. }
            | Instruction::ConstUnit => true,
            
            Instruction::Add { lhs, rhs }
            | Instruction::Sub { lhs, rhs }
            | Instruction::Mul { lhs, rhs }
            | Instruction::Div { lhs, rhs }
            | Instruction::Mod { lhs, rhs }
            | Instruction::Eq { lhs, rhs }
            | Instruction::Ne { lhs, rhs }
            | Instruction::Lt { lhs, rhs }
            | Instruction::Gt { lhs, rhs }
            | Instruction::Le { lhs, rhs }
            | Instruction::Ge { lhs, rhs }
            | Instruction::And { lhs, rhs }
            | Instruction::Or { lhs, rhs } => {
                const_map.contains_key(lhs) && const_map.contains_key(rhs)
            }
            
            Instruction::Not { operand }
            | Instruction::Neg { operand } => {
                const_map.contains_key(operand)
            }
            
            _ => false,
        }
    }
    
    /// Fold a constant instruction into a constant
    fn fold_instruction(instr: &Instruction, const_map: &HashMap<ValueId, Instruction>) -> Option<Instruction> {
        match instr {
            Instruction::Add { lhs, rhs } => {
                let lhs_const = const_map.get(lhs)?;
                let rhs_const = const_map.get(rhs)?;
                Self::fold_add(lhs_const, rhs_const)
            }
            Instruction::Sub { lhs, rhs } => {
                let lhs_const = const_map.get(lhs)?;
                let rhs_const = const_map.get(rhs)?;
                Self::fold_sub(lhs_const, rhs_const)
            }
            Instruction::Mul { lhs, rhs } => {
                let lhs_const = const_map.get(lhs)?;
                let rhs_const = const_map.get(rhs)?;
                Self::fold_mul(lhs_const, rhs_const)
            }
            Instruction::Div { lhs, rhs } => {
                let lhs_const = const_map.get(lhs)?;
                let rhs_const = const_map.get(rhs)?;
                Self::fold_div(lhs_const, rhs_const)
            }
            Instruction::Eq { lhs, rhs } => {
                let lhs_const = const_map.get(lhs)?;
                let rhs_const = const_map.get(rhs)?;
                Self::fold_eq(lhs_const, rhs_const)
            }
            Instruction::Lt { lhs, rhs } => {
                let lhs_const = const_map.get(lhs)?;
                let rhs_const = const_map.get(rhs)?;
                Self::fold_lt(lhs_const, rhs_const)
            }
            Instruction::Not { operand } => {
                let op_const = const_map.get(operand)?;
                Self::fold_not(op_const)
            }
            Instruction::Neg { operand } => {
                let op_const = const_map.get(operand)?;
                Self::fold_neg(op_const)
            }
            _ => None,
        }
    }
    
    /// Fold addition
    fn fold_add(lhs: &Instruction, rhs: &Instruction) -> Option<Instruction> {
        match (lhs, rhs) {
            (Instruction::ConstInt { value: a }, Instruction::ConstInt { value: b }) => {
                Some(Instruction::ConstInt { value: a + b })
            }
            (Instruction::ConstFloat { value: a }, Instruction::ConstFloat { value: b }) => {
                Some(Instruction::ConstFloat { value: a + b })
            }
            _ => None,
        }
    }
    
    /// Fold subtraction
    fn fold_sub(lhs: &Instruction, rhs: &Instruction) -> Option<Instruction> {
        match (lhs, rhs) {
            (Instruction::ConstInt { value: a }, Instruction::ConstInt { value: b }) => {
                Some(Instruction::ConstInt { value: a - b })
            }
            (Instruction::ConstFloat { value: a }, Instruction::ConstFloat { value: b }) => {
                Some(Instruction::ConstFloat { value: a - b })
            }
            _ => None,
        }
    }
    
    /// Fold multiplication
    fn fold_mul(lhs: &Instruction, rhs: &Instruction) -> Option<Instruction> {
        match (lhs, rhs) {
            (Instruction::ConstInt { value: a }, Instruction::ConstInt { value: b }) => {
                Some(Instruction::ConstInt { value: a * b })
            }
            (Instruction::ConstFloat { value: a }, Instruction::ConstFloat { value: b }) => {
                Some(Instruction::ConstFloat { value: a * b })
            }
            _ => None,
        }
    }
    
    /// Fold division
    fn fold_div(lhs: &Instruction, rhs: &Instruction) -> Option<Instruction> {
        match (lhs, rhs) {
            (Instruction::ConstInt { value: a }, Instruction::ConstInt { value: b }) if *b != 0 => {
                Some(Instruction::ConstInt { value: a / b })
            }
            (Instruction::ConstFloat { value: a }, Instruction::ConstFloat { value: b }) if *b != 0.0 => {
                Some(Instruction::ConstFloat { value: a / b })
            }
            _ => None,
        }
    }
    
    /// Fold equality comparison
    fn fold_eq(lhs: &Instruction, rhs: &Instruction) -> Option<Instruction> {
        match (lhs, rhs) {
            (Instruction::ConstInt { value: a }, Instruction::ConstInt { value: b }) => {
                Some(Instruction::ConstBool { value: a == b })
            }
            (Instruction::ConstFloat { value: a }, Instruction::ConstFloat { value: b }) => {
                Some(Instruction::ConstBool { value: a == b })
            }
            (Instruction::ConstBool { value: a }, Instruction::ConstBool { value: b }) => {
                Some(Instruction::ConstBool { value: a == b })
            }
            _ => None,
        }
    }
    
    /// Fold less-than comparison
    fn fold_lt(lhs: &Instruction, rhs: &Instruction) -> Option<Instruction> {
        match (lhs, rhs) {
            (Instruction::ConstInt { value: a }, Instruction::ConstInt { value: b }) => {
                Some(Instruction::ConstBool { value: a < b })
            }
            (Instruction::ConstFloat { value: a }, Instruction::ConstFloat { value: b }) => {
                Some(Instruction::ConstBool { value: a < b })
            }
            _ => None,
        }
    }
    
    /// Fold logical NOT
    fn fold_not(operand: &Instruction) -> Option<Instruction> {
        match operand {
            Instruction::ConstBool { value } => {
                Some(Instruction::ConstBool { value: !value })
            }
            _ => None,
        }
    }
    
    /// Fold unary negation
    fn fold_neg(operand: &Instruction) -> Option<Instruction> {
        match operand {
            Instruction::ConstInt { value } => {
                Some(Instruction::ConstInt { value: -value })
            }
            Instruction::ConstFloat { value } => {
                Some(Instruction::ConstFloat { value: -value })
            }
            _ => None,
        }
    }
    
    /// Replace instruction operands with constants
    fn replace_with_constants(instr: &Instruction, const_map: &HashMap<ValueId, Instruction>) -> Instruction {
        match instr {
            Instruction::Add { lhs, rhs } => {
                if let (Some(lhs_const), Some(rhs_const)) = (const_map.get(lhs), const_map.get(rhs)) {
                    if let Some(folded) = Self::fold_add(lhs_const, rhs_const) {
                        return folded;
                    }
                }
                instr.clone()
            }
            Instruction::Sub { lhs, rhs } => {
                if let (Some(lhs_const), Some(rhs_const)) = (const_map.get(lhs), const_map.get(rhs)) {
                    if let Some(folded) = Self::fold_sub(lhs_const, rhs_const) {
                        return folded;
                    }
                }
                instr.clone()
            }
            Instruction::Mul { lhs, rhs } => {
                if let (Some(lhs_const), Some(rhs_const)) = (const_map.get(lhs), const_map.get(rhs)) {
                    if let Some(folded) = Self::fold_mul(lhs_const, rhs_const) {
                        return folded;
                    }
                }
                instr.clone()
            }
            Instruction::Div { lhs, rhs } => {
                if let (Some(lhs_const), Some(rhs_const)) = (const_map.get(lhs), const_map.get(rhs)) {
                    if let Some(folded) = Self::fold_div(lhs_const, rhs_const) {
                        return folded;
                    }
                }
                instr.clone()
            }
            _ => instr.clone(),
        }
    }
    
    /// Common Subexpression Elimination: replace duplicate expressions with single computation
    fn common_subexpression_elimination(ir: &IrProgram) -> IrProgram {
        let mut new_ir = ir.clone();
        
        for func in &mut new_ir.functions {
            // Map from instruction signature to value ID
            let mut expr_map: HashMap<String, ValueId> = HashMap::new();
            let mut value_replacements: HashMap<ValueId, ValueId> = HashMap::new();
            
            // First pass: identify common subexpressions
            for block in &func.blocks {
                for (value_id, instr) in &block.instructions {
                    // Skip constants and already replaced values
                    if Self::is_immediate_constant(instr) || value_replacements.contains_key(value_id) {
                        continue;
                    }
                    
                    // Create signature for this instruction
                    let signature = Self::instruction_signature(instr, &value_replacements);
                    
                    if let Some(existing_value) = expr_map.get(&signature) {
                        // Found duplicate expression, mark for replacement
                        value_replacements.insert(*value_id, *existing_value);
                    } else {
                        // First occurrence, record it
                        expr_map.insert(signature, *value_id);
                    }
                }
            }
            
            // Second pass: replace duplicate expressions
            for block in &mut func.blocks {
                // Replace in instructions
                for (_value_id, instr) in &mut block.instructions {
                    // Replace operands with their canonical values
                    *instr = Self::replace_value_ids(instr, &value_replacements);
                }
                
                // Remove instructions that are replaced
                block.instructions.retain(|(value_id, _)| {
                    !value_replacements.contains_key(value_id)
                });
                
                // Replace in terminator
                if let Some(terminator) = &mut block.terminator {
                    *terminator = Self::replace_value_ids_in_terminator(terminator, &value_replacements);
                }
            }
        }
        
        new_ir
    }
    
    /// Create a signature string for an instruction (for CSE matching)
    fn instruction_signature(instr: &Instruction, value_replacements: &HashMap<ValueId, ValueId>) -> String {
        match instr {
            Instruction::Add { lhs, rhs } => {
                let lhs = Self::resolve_value(*lhs, value_replacements);
                let rhs = Self::resolve_value(*rhs, value_replacements);
                format!("Add({},{})", lhs.0, rhs.0)
            }
            Instruction::Sub { lhs, rhs } => {
                let lhs = Self::resolve_value(*lhs, value_replacements);
                let rhs = Self::resolve_value(*rhs, value_replacements);
                format!("Sub({},{})", lhs.0, rhs.0)
            }
            Instruction::Mul { lhs, rhs } => {
                let lhs = Self::resolve_value(*lhs, value_replacements);
                let rhs = Self::resolve_value(*rhs, value_replacements);
                format!("Mul({},{})", lhs.0, rhs.0)
            }
            Instruction::Div { lhs, rhs } => {
                let lhs = Self::resolve_value(*lhs, value_replacements);
                let rhs = Self::resolve_value(*rhs, value_replacements);
                format!("Div({},{})", lhs.0, rhs.0)
            }
            Instruction::Eq { lhs, rhs } => {
                let lhs = Self::resolve_value(*lhs, value_replacements);
                let rhs = Self::resolve_value(*rhs, value_replacements);
                format!("Eq({},{})", lhs.0, rhs.0)
            }
            Instruction::Lt { lhs, rhs } => {
                let lhs = Self::resolve_value(*lhs, value_replacements);
                let rhs = Self::resolve_value(*rhs, value_replacements);
                format!("Lt({},{})", lhs.0, rhs.0)
            }
            Instruction::Not { operand } => {
                let op = Self::resolve_value(*operand, value_replacements);
                format!("Not({})", op.0)
            }
            Instruction::Neg { operand } => {
                let op = Self::resolve_value(*operand, value_replacements);
                format!("Neg({})", op.0)
            }
            _ => format!("{:?}", instr), // Fallback for other instructions
        }
    }
    
    /// Resolve a value ID through replacement chain
    fn resolve_value(mut value: ValueId, value_replacements: &HashMap<ValueId, ValueId>) -> ValueId {
        while let Some(&replacement) = value_replacements.get(&value) {
            value = replacement;
        }
        value
    }
    
    /// Replace value IDs in an instruction
    fn replace_value_ids(instr: &Instruction, value_replacements: &HashMap<ValueId, ValueId>) -> Instruction {
        match instr {
            Instruction::Add { lhs, rhs } => {
                Instruction::Add {
                    lhs: Self::resolve_value(*lhs, value_replacements),
                    rhs: Self::resolve_value(*rhs, value_replacements),
                }
            }
            Instruction::Sub { lhs, rhs } => {
                Instruction::Sub {
                    lhs: Self::resolve_value(*lhs, value_replacements),
                    rhs: Self::resolve_value(*rhs, value_replacements),
                }
            }
            Instruction::Mul { lhs, rhs } => {
                Instruction::Mul {
                    lhs: Self::resolve_value(*lhs, value_replacements),
                    rhs: Self::resolve_value(*rhs, value_replacements),
                }
            }
            Instruction::Div { lhs, rhs } => {
                Instruction::Div {
                    lhs: Self::resolve_value(*lhs, value_replacements),
                    rhs: Self::resolve_value(*rhs, value_replacements),
                }
            }
            Instruction::Eq { lhs, rhs } => {
                Instruction::Eq {
                    lhs: Self::resolve_value(*lhs, value_replacements),
                    rhs: Self::resolve_value(*rhs, value_replacements),
                }
            }
            Instruction::Lt { lhs, rhs } => {
                Instruction::Lt {
                    lhs: Self::resolve_value(*lhs, value_replacements),
                    rhs: Self::resolve_value(*rhs, value_replacements),
                }
            }
            Instruction::Not { operand } => {
                Instruction::Not {
                    operand: Self::resolve_value(*operand, value_replacements),
                }
            }
            Instruction::Neg { operand } => {
                Instruction::Neg {
                    operand: Self::resolve_value(*operand, value_replacements),
                }
            }
            _ => instr.clone(),
        }
    }
    
    /// Replace value IDs in terminator instruction
    fn replace_value_ids_in_terminator(terminator: &Instruction, value_replacements: &HashMap<ValueId, ValueId>) -> Instruction {
        match terminator {
            Instruction::Return { value } => {
                Instruction::Return {
                    value: value.map(|v| Self::resolve_value(v, value_replacements)),
                }
            }
            Instruction::Branch { cond, then_block, else_block } => {
                Instruction::Branch {
                    cond: Self::resolve_value(*cond, value_replacements),
                    then_block: *then_block,
                    else_block: *else_block,
                }
            }
            _ => terminator.clone(),
        }
    }
    
    /// Dead Code Elimination: remove unused instructions
    fn dead_code_elimination(ir: &IrProgram) -> IrProgram {
        let mut new_ir = ir.clone();
        
        for func in &mut new_ir.functions {
            // Find all used values (live values)
            let mut used_values: HashSet<ValueId> = HashSet::new();
            
            // Mark return values and branch conditions as used
            for block in &func.blocks {
                if let Some(terminator) = &block.terminator {
                    match terminator {
                        Instruction::Return { value } => {
                            if let Some(val) = value {
                                used_values.insert(*val);
                            }
                        }
                        Instruction::Branch { cond, .. } => {
                            used_values.insert(*cond);
                        }
                        _ => {}
                    }
                }
            }
            
            // Mark parameter values as used
            for (_, param_value, _) in &func.params {
                used_values.insert(*param_value);
            }
            
            // Mark instructions with side effects as used (and their operands)
            for block in &func.blocks {
                for (value_id, instr) in &block.instructions {
                    if Self::has_side_effects(instr) {
                        used_values.insert(*value_id);
                        // Mark all operands of instructions with side effects as used
                        let operands = Self::get_instruction_operands(instr);
                        for operand in operands {
                            used_values.insert(operand);
                        }
                    }
                }
            }
            
            // Propagate usage backwards
            let mut changed = true;
            while changed {
                changed = false;
                for block in &func.blocks {
                    for (value_id, instr) in &block.instructions {
                        if used_values.contains(value_id) {
                            // Mark all operands of this instruction as used
                            let operands = Self::get_instruction_operands(instr);
                            for operand in operands {
                                if used_values.insert(operand) {
                                    changed = true;
                                }
                            }
                        }
                    }
                }
            }
            
            // Remove unused instructions (but keep instructions with side effects)
            for block in &mut func.blocks {
                block.instructions.retain(|(value_id, instr)| {
                    used_values.contains(value_id) || Self::has_side_effects(instr)
                });
            }
        }
        
        new_ir
    }
    
    /// Check if an instruction has side effects (e.g., memory operations, function calls)
    fn has_side_effects(instr: &Instruction) -> bool {
        match instr {
            Instruction::Store { .. }
            | Instruction::Alloc { .. }
            | Instruction::Free { .. }
            | Instruction::Call { effect: MemoryEffect::Write, .. }
            | Instruction::Call { effect: MemoryEffect::ReadWrite, .. }
            | Instruction::Intrinsic { effect: MemoryEffect::Write, .. }
            | Instruction::Intrinsic { effect: MemoryEffect::ReadWrite, .. } => true,
            Instruction::Intrinsic { name, .. } => {
                // print always has side effects
                name == "print"
            }
            _ => false,
        }
    }
    
    /// Get all value operands from an instruction
    fn get_instruction_operands(instr: &Instruction) -> Vec<ValueId> {
        match instr {
            Instruction::Add { lhs, rhs }
            | Instruction::Sub { lhs, rhs }
            | Instruction::Mul { lhs, rhs }
            | Instruction::Div { lhs, rhs }
            | Instruction::Mod { lhs, rhs }
            | Instruction::Eq { lhs, rhs }
            | Instruction::Ne { lhs, rhs }
            | Instruction::Lt { lhs, rhs }
            | Instruction::Gt { lhs, rhs }
            | Instruction::Le { lhs, rhs }
            | Instruction::Ge { lhs, rhs }
            | Instruction::And { lhs, rhs }
            | Instruction::Or { lhs, rhs } => vec![*lhs, *rhs],
            
            Instruction::Not { operand }
            | Instruction::Neg { operand } => vec![*operand],
            
            Instruction::Load { ptr, .. } => vec![*ptr],
            Instruction::Store { ptr, value, .. } => vec![*ptr, *value],
            Instruction::Alloc { size, .. } => vec![*size],
            Instruction::Free { ptr, .. } => vec![*ptr],
            
            Instruction::Call { callee, args, effect: _, function_name: _ } => {
                let mut operands = vec![*callee];
                operands.extend(args);
                operands
            }
            
            Instruction::Phi { incoming } => {
                incoming.iter().map(|(_, val)| *val).collect()
            }
            
            Instruction::TensorOp { args, .. } => args.clone(),
            Instruction::Intrinsic { args, .. } => args.clone(),
            
            _ => Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_constant_folding_add() {
        let mut ir = IrProgram::new();
        let func_id = ir.new_function_id();
        let block_id = ir.new_block_id();
        
        let v1 = ir.new_value_id();
        let v2 = ir.new_value_id();
        let v3 = ir.new_value_id();
        
        let mut func = IrFunction {
            id: func_id,
            name: "test".to_string(),
            params: Vec::new(),
            return_type: IrType::Int64,
            blocks: vec![BasicBlock::new(block_id)],
            entry_block: block_id,
            attributes: Vec::new(),
        };
        
        func.blocks[0].instructions.push((
            v1,
            Instruction::ConstInt { value: 5 },
        ));
        func.blocks[0].instructions.push((
            v2,
            Instruction::ConstInt { value: 3 },
        ));
        func.blocks[0].instructions.push((
            v3,
            Instruction::Add { lhs: v1, rhs: v2 },
        ));
        func.blocks[0].terminator = Some(Instruction::Return { value: Some(v3) });
        
        ir.functions.push(func);
        
        let optimized = Optimizer::optimize(ir);
        let func = &optimized.functions[0];
        
        // Should have constant folded the addition
        assert!(func.blocks[0].instructions.len() <= 3);
        
        // Check that v3 is now a constant
        let v3_instr = func.blocks[0].instructions.iter()
            .find(|(vid, _)| *vid == v3)
            .map(|(_, instr)| instr);
        
        if let Some(Instruction::ConstInt { value }) = v3_instr {
            assert_eq!(*value, 8);
        } else {
            panic!("Expected v3 to be folded to ConstInt(8)");
        }
    }
    
    #[test]
    fn test_constant_folding_mul() {
        let mut ir = IrProgram::new();
        let func_id = ir.new_function_id();
        let block_id = ir.new_block_id();
        
        let v1 = ir.new_value_id();
        let v2 = ir.new_value_id();
        let v3 = ir.new_value_id();
        
        let mut func = IrFunction {
            id: func_id,
            name: "test".to_string(),
            params: Vec::new(),
            return_type: IrType::Int64,
            blocks: vec![BasicBlock::new(block_id)],
            entry_block: block_id,
            attributes: Vec::new(),
        };
        
        func.blocks[0].instructions.push((
            v1,
            Instruction::ConstInt { value: 4 },
        ));
        func.blocks[0].instructions.push((
            v2,
            Instruction::ConstInt { value: 7 },
        ));
        func.blocks[0].instructions.push((
            v3,
            Instruction::Mul { lhs: v1, rhs: v2 },
        ));
        func.blocks[0].terminator = Some(Instruction::Return { value: Some(v3) });
        
        ir.functions.push(func);
        
        let optimized = Optimizer::optimize(ir);
        let func = &optimized.functions[0];
        
        let v3_instr = func.blocks[0].instructions.iter()
            .find(|(vid, _)| *vid == v3)
            .map(|(_, instr)| instr);
        
        if let Some(Instruction::ConstInt { value }) = v3_instr {
            assert_eq!(*value, 28);
        } else {
            panic!("Expected v3 to be folded to ConstInt(28)");
        }
    }
    
    #[test]
    fn test_dead_code_elimination() {
        let mut ir = IrProgram::new();
        let func_id = ir.new_function_id();
        let block_id = ir.new_block_id();
        
        let v1 = ir.new_value_id();
        let v2 = ir.new_value_id();
        let v3 = ir.new_value_id();
        
        let mut func = IrFunction {
            id: func_id,
            name: "test".to_string(),
            params: Vec::new(),
            return_type: IrType::Int64,
            blocks: vec![BasicBlock::new(block_id)],
            entry_block: block_id,
            attributes: Vec::new(),
        };
        
        // v1 is used in return
        func.blocks[0].instructions.push((
            v1,
            Instruction::ConstInt { value: 5 },
        ));
        // v2 is dead code (never used)
        func.blocks[0].instructions.push((
            v2,
            Instruction::ConstInt { value: 10 },
        ));
        // v3 is dead code (never used)
        func.blocks[0].instructions.push((
            v3,
            Instruction::Add { lhs: v1, rhs: v2 },
        ));
        func.blocks[0].terminator = Some(Instruction::Return { value: Some(v1) });
        
        ir.functions.push(func);
        
        let optimized = Optimizer::optimize(ir);
        let func = &optimized.functions[0];
        
        // v2 and v3 should be eliminated
        let remaining_values: Vec<ValueId> = func.blocks[0].instructions.iter()
            .map(|(vid, _)| *vid)
            .collect();
        
        assert!(remaining_values.contains(&v1), "v1 should remain (used in return)");
        assert!(!remaining_values.contains(&v2), "v2 should be eliminated (dead code)");
        assert!(!remaining_values.contains(&v3), "v3 should be eliminated (dead code)");
    }
    
    #[test]
    fn test_common_subexpression_elimination() {
        let mut ir = IrProgram::new();
        let func_id = ir.new_function_id();
        let block_id = ir.new_block_id();
        
        let v1 = ir.new_value_id();
        let v2 = ir.new_value_id();
        let v3 = ir.new_value_id(); // x + y (first)
        let v4 = ir.new_value_id(); // x + y (duplicate)
        let v5 = ir.new_value_id(); // result
        
        let mut func = IrFunction {
            id: func_id,
            name: "test".to_string(),
            params: vec![
                ("x".to_string(), v1, IrType::Int64),
                ("y".to_string(), v2, IrType::Int64),
            ],
            return_type: IrType::Int64,
            blocks: vec![BasicBlock::new(block_id)],
            entry_block: block_id,
            attributes: Vec::new(),
        };
        
        // First x + y
        func.blocks[0].instructions.push((
            v3,
            Instruction::Add { lhs: v1, rhs: v2 },
        ));
        // Duplicate x + y
        func.blocks[0].instructions.push((
            v4,
            Instruction::Add { lhs: v1, rhs: v2 },
        ));
        // Use both (but they should be the same after CSE)
        func.blocks[0].instructions.push((
            v5,
            Instruction::Add { lhs: v3, rhs: v4 },
        ));
        func.blocks[0].terminator = Some(Instruction::Return { value: Some(v5) });
        
        ir.functions.push(func);
        
        let optimized = Optimizer::optimize(ir);
        let func = &optimized.functions[0];
        
        // After CSE, v4 should be eliminated and replaced with v3
        let remaining_values: Vec<ValueId> = func.blocks[0].instructions.iter()
            .map(|(vid, _)| *vid)
            .collect();
        
        // v3 should remain, v4 should be eliminated (replaced by v3)
        assert!(remaining_values.contains(&v3), "v3 should remain (first occurrence)");
        assert!(!remaining_values.contains(&v4), "v4 should be eliminated (duplicate of v3)");
    }
}

