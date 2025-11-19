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
        ir = Self::loop_invariant_code_motion(&ir);
        ir = Self::function_inlining(&ir);
        ir = Self::tail_call_optimization(&ir);
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
    fn is_constant_instruction(
        instr: &Instruction,
        const_map: &HashMap<ValueId, Instruction>,
    ) -> bool {
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

            Instruction::Not { operand } | Instruction::Neg { operand } => {
                const_map.contains_key(operand)
            }

            _ => false,
        }
    }

    /// Fold a constant instruction into a constant
    fn fold_instruction(
        instr: &Instruction,
        const_map: &HashMap<ValueId, Instruction>,
    ) -> Option<Instruction> {
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
            (Instruction::ConstFloat { value: a }, Instruction::ConstFloat { value: b })
                if *b != 0.0 =>
            {
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
            Instruction::ConstBool { value } => Some(Instruction::ConstBool { value: !value }),
            _ => None,
        }
    }

    /// Fold unary negation
    fn fold_neg(operand: &Instruction) -> Option<Instruction> {
        match operand {
            Instruction::ConstInt { value } => Some(Instruction::ConstInt { value: -value }),
            Instruction::ConstFloat { value } => Some(Instruction::ConstFloat { value: -value }),
            _ => None,
        }
    }

    /// Replace instruction operands with constants
    fn replace_with_constants(
        instr: &Instruction,
        const_map: &HashMap<ValueId, Instruction>,
    ) -> Instruction {
        match instr {
            Instruction::Add { lhs, rhs } => {
                if let (Some(lhs_const), Some(rhs_const)) = (const_map.get(lhs), const_map.get(rhs))
                {
                    if let Some(folded) = Self::fold_add(lhs_const, rhs_const) {
                        return folded;
                    }
                }
                instr.clone()
            }
            Instruction::Sub { lhs, rhs } => {
                if let (Some(lhs_const), Some(rhs_const)) = (const_map.get(lhs), const_map.get(rhs))
                {
                    if let Some(folded) = Self::fold_sub(lhs_const, rhs_const) {
                        return folded;
                    }
                }
                instr.clone()
            }
            Instruction::Mul { lhs, rhs } => {
                if let (Some(lhs_const), Some(rhs_const)) = (const_map.get(lhs), const_map.get(rhs))
                {
                    if let Some(folded) = Self::fold_mul(lhs_const, rhs_const) {
                        return folded;
                    }
                }
                instr.clone()
            }
            Instruction::Div { lhs, rhs } => {
                if let (Some(lhs_const), Some(rhs_const)) = (const_map.get(lhs), const_map.get(rhs))
                {
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
                    if Self::is_immediate_constant(instr)
                        || value_replacements.contains_key(value_id)
                    {
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
                block
                    .instructions
                    .retain(|(value_id, _)| !value_replacements.contains_key(value_id));

                // Replace in terminator
                if let Some(terminator) = &mut block.terminator {
                    *terminator =
                        Self::replace_value_ids_in_terminator(terminator, &value_replacements);
                }
            }
        }

        new_ir
    }

    /// Create a signature string for an instruction (for CSE matching)
    fn instruction_signature(
        instr: &Instruction,
        value_replacements: &HashMap<ValueId, ValueId>,
    ) -> String {
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
    fn resolve_value(
        mut value: ValueId,
        value_replacements: &HashMap<ValueId, ValueId>,
    ) -> ValueId {
        while let Some(&replacement) = value_replacements.get(&value) {
            value = replacement;
        }
        value
    }

    /// Replace value IDs in an instruction
    fn replace_value_ids(
        instr: &Instruction,
        value_replacements: &HashMap<ValueId, ValueId>,
    ) -> Instruction {
        match instr {
            Instruction::Add { lhs, rhs } => Instruction::Add {
                lhs: Self::resolve_value(*lhs, value_replacements),
                rhs: Self::resolve_value(*rhs, value_replacements),
            },
            Instruction::Sub { lhs, rhs } => Instruction::Sub {
                lhs: Self::resolve_value(*lhs, value_replacements),
                rhs: Self::resolve_value(*rhs, value_replacements),
            },
            Instruction::Mul { lhs, rhs } => Instruction::Mul {
                lhs: Self::resolve_value(*lhs, value_replacements),
                rhs: Self::resolve_value(*rhs, value_replacements),
            },
            Instruction::Div { lhs, rhs } => Instruction::Div {
                lhs: Self::resolve_value(*lhs, value_replacements),
                rhs: Self::resolve_value(*rhs, value_replacements),
            },
            Instruction::Eq { lhs, rhs } => Instruction::Eq {
                lhs: Self::resolve_value(*lhs, value_replacements),
                rhs: Self::resolve_value(*rhs, value_replacements),
            },
            Instruction::Lt { lhs, rhs } => Instruction::Lt {
                lhs: Self::resolve_value(*lhs, value_replacements),
                rhs: Self::resolve_value(*rhs, value_replacements),
            },
            Instruction::Not { operand } => Instruction::Not {
                operand: Self::resolve_value(*operand, value_replacements),
            },
            Instruction::Neg { operand } => Instruction::Neg {
                operand: Self::resolve_value(*operand, value_replacements),
            },
            Instruction::Call {
                callee,
                args,
                effect,
                function_name,
                is_tail_call,
            } => Instruction::Call {
                callee: Self::resolve_value(*callee, value_replacements),
                args: args
                    .iter()
                    .map(|a| Self::resolve_value(*a, value_replacements))
                    .collect(),
                effect: *effect,
                function_name: function_name.clone(),
                is_tail_call: *is_tail_call,
            },
            _ => instr.clone(),
        }
    }

    /// Replace value IDs in terminator instruction
    fn replace_value_ids_in_terminator(
        terminator: &Instruction,
        value_replacements: &HashMap<ValueId, ValueId>,
    ) -> Instruction {
        match terminator {
            Instruction::Return { value } => Instruction::Return {
                value: value.map(|v| Self::resolve_value(v, value_replacements)),
            },
            Instruction::Branch {
                cond,
                then_block,
                else_block,
            } => Instruction::Branch {
                cond: Self::resolve_value(*cond, value_replacements),
                then_block: *then_block,
                else_block: *else_block,
            },
            _ => terminator.clone(),
        }
    }

    /// Tail-call optimization: mark calls that are immediately returned as tail calls
    fn tail_call_optimization(ir: &IrProgram) -> IrProgram {
        let mut new_ir = ir.clone();

        for func in &mut new_ir.functions {
            for block in &mut func.blocks {
                // Check if block ends with a return that uses the result of the last instruction
                if let Some(Instruction::Return {
                    value: Some(return_value),
                }) = &block.terminator
                {
                    // Check if the last instruction is a Call that produces the return value
                    if let Some((last_value_id, last_instr)) = block.instructions.last() {
                        if *last_value_id == *return_value {
                            if let Instruction::Call {
                                callee,
                                args,
                                effect,
                                function_name,
                                ..
                            } = last_instr
                            {
                                // This is a tail call - mark it
                                let last_idx = block.instructions.len() - 1;
                                let new_call = Instruction::Call {
                                    callee: *callee,
                                    args: args.clone(),
                                    effect: *effect,
                                    function_name: function_name.clone(),
                                    is_tail_call: true,
                                };
                                block.instructions[last_idx] = (*last_value_id, new_call);
                            }
                        }
                    }
                }
            }
        }

        new_ir
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
                        Instruction::Return { value: Some(val) } => {
                            used_values.insert(*val);
                        }
                        Instruction::Return { value: None } => {}
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
            | Instruction::Call {
                effect: MemoryEffect::Write,
                ..
            }
            | Instruction::Call {
                effect: MemoryEffect::ReadWrite,
                ..
            }
            | Instruction::Intrinsic {
                effect: MemoryEffect::Write,
                ..
            }
            | Instruction::Intrinsic {
                effect: MemoryEffect::ReadWrite,
                ..
            } => true,
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

            Instruction::Not { operand } | Instruction::Neg { operand } => vec![*operand],

            Instruction::Load { ptr, .. } => vec![*ptr],
            Instruction::Store { ptr, value, .. } => vec![*ptr, *value],
            Instruction::Alloc { size, .. } => vec![*size],
            Instruction::Free { ptr, .. } => vec![*ptr],

            Instruction::Call {
                callee,
                args,
                effect: _,
                function_name: _,
                is_tail_call: _,
            } => {
                let mut operands = vec![*callee];
                operands.extend(args);
                operands
            }

            Instruction::Phi { incoming } => incoming.iter().map(|(_, val)| *val).collect(),

            Instruction::TensorOp { args, .. } => args.clone(),
            Instruction::Intrinsic { args, .. } => args.clone(),

            _ => Vec::new(),
        }
    }

    /// Loop Invariant Code Motion: move loop-invariant instructions out of loops
    fn loop_invariant_code_motion(ir: &IrProgram) -> IrProgram {
        let mut new_ir = ir.clone();

        for func in &mut new_ir.functions {
            // Find all loops in the function
            let loops = Self::find_loops(func);

            for (header_block_id, body_blocks) in loops {
                // Find loop-invariant instructions in body blocks
                let mut invariant_instructions: Vec<(BlockId, usize, (ValueId, Instruction))> =
                    Vec::new();
                let mut loop_defined_values: HashSet<ValueId> = HashSet::new();

                // First pass: identify values defined in the loop
                for &body_block_id in &body_blocks {
                    if let Some(body_block) = func.blocks.iter().find(|b| b.id == body_block_id) {
                        for (value_id, _) in &body_block.instructions {
                            loop_defined_values.insert(*value_id);
                        }
                    }
                }

                // Also include values from header block
                if let Some(header_block) = func.blocks.iter().find(|b| b.id == header_block_id) {
                    for (value_id, _) in &header_block.instructions {
                        loop_defined_values.insert(*value_id);
                    }
                }

                // Second pass: find invariant instructions
                for &body_block_id in &body_blocks {
                    if let Some(body_block) = func.blocks.iter().find(|b| b.id == body_block_id) {
                        for (idx, (value_id, instr)) in body_block.instructions.iter().enumerate() {
                            // Check if instruction is loop-invariant
                            if Self::is_loop_invariant(instr, &loop_defined_values) {
                                invariant_instructions.push((
                                    body_block_id,
                                    idx,
                                    (*value_id, instr.clone()),
                                ));
                            } else {
                                // This instruction uses loop-defined values, so mark its result as loop-defined
                                loop_defined_values.insert(*value_id);
                            }
                        }
                    }
                }

                // Third pass: move invariant instructions to pre-header block
                if !invariant_instructions.is_empty() {
                    // Find or create pre-header block (block that jumps to header)
                    let pre_header_id = Self::find_or_create_pre_header(func, header_block_id);

                    // Move invariant instructions to pre-header
                    if let Some(pre_header) = func.blocks.iter_mut().find(|b| b.id == pre_header_id)
                    {
                        // Collect instructions to move (in reverse order to maintain dependencies)
                        let mut to_move: Vec<(ValueId, Instruction)> = Vec::new();
                        for (_, _, (value_id, instr)) in invariant_instructions.iter().rev() {
                            to_move.push((*value_id, instr.clone()));
                        }

                        // Insert before terminator
                        let terminator = pre_header.terminator.take();
                        for (value_id, instr) in to_move {
                            pre_header.instructions.push((value_id, instr));
                        }
                        pre_header.terminator = terminator;
                    }

                    // Remove moved instructions from body blocks
                    for (body_block_id, idx, _) in invariant_instructions.iter().rev() {
                        if let Some(body_block) =
                            func.blocks.iter_mut().find(|b| b.id == *body_block_id)
                        {
                            if *idx < body_block.instructions.len() {
                                body_block.instructions.remove(*idx);
                            }
                        }
                    }
                }
            }
        }

        new_ir
    }

    /// Find all loops in a function (returns (header_block_id, body_block_ids))
    fn find_loops(func: &IrFunction) -> Vec<(BlockId, Vec<BlockId>)> {
        let mut loops = Vec::new();
        let mut visited = HashSet::new();

        // Simple loop detection: a block is a loop header if it has a predecessor that is dominated by itself
        // For now, use a simpler heuristic: find blocks that are their own predecessors through a back edge
        for block in &func.blocks {
            // Check if any successor is also a predecessor (back edge)
            for &successor_id in &block.successors {
                if let Some(successor) = func.blocks.iter().find(|b| b.id == successor_id) {
                    if successor.predecessors.contains(&block.id) {
                        // Found a back edge: block -> successor -> ... -> block
                        // The successor is the loop header
                        if !visited.contains(&successor_id) {
                            visited.insert(successor_id);

                            // Find all blocks in the loop (reachable from header without going through header's predecessors)
                            let body_blocks = Self::find_loop_body(func, successor_id, block.id);
                            loops.push((successor_id, body_blocks));
                        }
                    }
                }
            }
        }

        loops
    }

    /// Find all blocks in a loop body
    fn find_loop_body(
        func: &IrFunction,
        header_id: BlockId,
        back_edge_from: BlockId,
    ) -> Vec<BlockId> {
        let mut body_blocks = Vec::new();
        let mut to_visit = vec![back_edge_from];
        let mut visited = HashSet::new();
        visited.insert(header_id);

        // Get header's predecessors (excluding the back edge)
        if let Some(header) = func.blocks.iter().find(|b| b.id == header_id) {
            for &pred_id in &header.predecessors {
                if pred_id != back_edge_from {
                    visited.insert(pred_id);
                }
            }
        }

        while let Some(block_id) = to_visit.pop() {
            if visited.contains(&block_id) {
                continue;
            }
            visited.insert(block_id);
            body_blocks.push(block_id);

            if let Some(block) = func.blocks.iter().find(|b| b.id == block_id) {
                for &successor_id in &block.successors {
                    if !visited.contains(&successor_id) && successor_id != header_id {
                        to_visit.push(successor_id);
                    }
                }
            }
        }

        body_blocks
    }

    /// Check if an instruction is loop-invariant (doesn't depend on loop-defined values)
    fn is_loop_invariant(instr: &Instruction, loop_defined_values: &HashSet<ValueId>) -> bool {
        // Instructions with side effects are never invariant
        if Self::has_side_effects(instr) {
            return false;
        }

        // Check if all operands are loop-invariant
        let operands = Self::get_instruction_operands(instr);
        for operand in operands {
            if loop_defined_values.contains(&operand) {
                return false;
            }
        }

        true
    }

    /// Find or create a pre-header block for a loop header
    fn find_or_create_pre_header(func: &mut IrFunction, header_id: BlockId) -> BlockId {
        // Find the header block
        let header = func.blocks.iter().find(|b| b.id == header_id).unwrap();

        // If header has only one predecessor, use it as pre-header
        if header.predecessors.len() == 1 {
            return header.predecessors[0];
        }

        // Otherwise, create a new pre-header block
        let pre_header_id = BlockId(func.blocks.len() as u32 + 1000); // Temporary ID
        let mut pre_header = BasicBlock::new(pre_header_id);
        pre_header.terminator = Some(Instruction::Jump { target: header_id });
        pre_header.successors.push(header_id);

        // Update all predecessors of header to jump to pre-header instead
        let predecessors = header.predecessors.clone();
        for pred_id in &predecessors {
            if let Some(pred_block) = func.blocks.iter_mut().find(|b| b.id == *pred_id) {
                // Update terminator to jump to pre-header
                if let Some(Instruction::Jump { target }) = &mut pred_block.terminator {
                    if *target == header_id {
                        *target = pre_header_id;
                    }
                } else if let Some(Instruction::Branch {
                    then_block,
                    else_block,
                    ..
                }) = &mut pred_block.terminator
                {
                    if *then_block == header_id {
                        *then_block = pre_header_id;
                    }
                    if *else_block == header_id {
                        *else_block = pre_header_id;
                    }
                }

                // Update successors
                if let Some(pos) = pred_block.successors.iter().position(|&id| id == header_id) {
                    pred_block.successors[pos] = pre_header_id;
                }
            }
        }

        // Update header's predecessors
        if let Some(header_block) = func.blocks.iter_mut().find(|b| b.id == header_id) {
            header_block.predecessors = vec![pre_header_id];
        }

        pre_header.predecessors = predecessors;
        func.blocks.push(pre_header);

        pre_header_id
    }

    /// Function inlining: inline small functions marked with @inline attribute
    fn function_inlining(ir: &IrProgram) -> IrProgram {
        let mut new_ir = ir.clone();

        // Find functions marked for inlining and create a map of name -> function data (clone needed data)
        type InlineCandidate = (
            FunctionId,
            BlockId,
            Vec<BasicBlock>,
            Vec<(String, ValueId, IrType)>,
        );
        let mut inline_candidates: HashMap<String, InlineCandidate> = HashMap::new();
        for func in &new_ir.functions {
            if func
                .attributes
                .iter()
                .any(|a| a == "@inline" || a == "inline")
            {
                // Check if function is small enough to inline
                let total_instructions: usize =
                    func.blocks.iter().map(|b| b.instructions.len()).sum();

                if total_instructions < 10 {
                    // Clone the function's data for inlining
                    inline_candidates.insert(
                        func.name.clone(),
                        (
                            func.id,
                            func.entry_block,
                            func.blocks.clone(),
                            func.params.clone(),
                        ),
                    );
                }
            }
        }

        if inline_candidates.is_empty() {
            return new_ir;
        }

        // Inline small functions
        for func in &mut new_ir.functions {
            let mut changed = true;
            while changed {
                changed = false;

                for block in &mut func.blocks {
                    let mut new_instructions = Vec::new();
                    let mut block_changed = false;

                    for (value_id, instr) in &block.instructions {
                        if let Instruction::Call {
                            function_name: Some(callee_name),
                            args,
                            ..
                        } = instr
                        {
                            // Check if this is a call to an inline candidate
                            if let Some((
                                _callee_id,
                                entry_block_id,
                                callee_blocks,
                                callee_params,
                            )) = inline_candidates.get(callee_name)
                            {
                                // Inline the function
                                if let Some(inlined_code) = Self::inline_function(
                                    *entry_block_id,
                                    callee_blocks,
                                    callee_params,
                                    args,
                                    *value_id,
                                ) {
                                    new_instructions.extend(inlined_code);
                                    block_changed = true;
                                    changed = true;
                                    continue;
                                }
                            }
                        }

                        new_instructions.push((*value_id, instr.clone()));
                    }

                    if block_changed {
                        block.instructions = new_instructions;
                    }
                }
            }
        }

        new_ir
    }

    /// Inline a function call, returning the inlined instructions
    fn inline_function(
        entry_block_id: BlockId,
        callee_blocks: &[BasicBlock],
        callee_params: &[(String, ValueId, IrType)],
        args: &[ValueId],
        result_value: ValueId,
    ) -> Option<Vec<(ValueId, Instruction)>> {
        if callee_params.len() != args.len() {
            return None;
        }

        // Create parameter mapping
        let mut param_map: HashMap<ValueId, ValueId> = HashMap::new();
        for (i, (_, param_value, _)) in callee_params.iter().enumerate() {
            param_map.insert(*param_value, args[i]);
        }

        // Clone and remap instructions from callee's entry block
        let entry_block = callee_blocks.iter().find(|b| b.id == entry_block_id)?;
        let mut inlined_instructions = Vec::new();

        for (value_id, instr) in &entry_block.instructions {
            let new_value_id = if *value_id == result_value {
                result_value
            } else {
                // Use a new value ID for intermediate values
                ValueId(value_id.0 + 100000) // Offset to avoid conflicts
            };

            let new_instr = Self::remap_instruction(instr, &param_map);
            inlined_instructions.push((new_value_id, new_instr));
        }

        // Handle return value
        if let Some(Instruction::Return {
            value: Some(return_value),
        }) = &entry_block.terminator
        {
            // Check if return value is a parameter
            if param_map.contains_key(return_value) {
                // If return value is a parameter, just use the argument directly
                // This will be handled by the caller using the mapped value
            } else {
                // Find the instruction that produces the return value
                if let Some((_, return_instr)) = entry_block
                    .instructions
                    .iter()
                    .find(|(vid, _)| *vid == *return_value)
                {
                    let new_instr = Self::remap_instruction(return_instr, &param_map);
                    inlined_instructions.push((result_value, new_instr));
                }
            }
        }

        Some(inlined_instructions)
    }

    /// Remap value IDs in an instruction according to parameter mapping
    fn remap_instruction(
        instr: &Instruction,
        param_map: &HashMap<ValueId, ValueId>,
    ) -> Instruction {
        match instr {
            Instruction::Add { lhs, rhs } => Instruction::Add {
                lhs: *param_map.get(lhs).unwrap_or(lhs),
                rhs: *param_map.get(rhs).unwrap_or(rhs),
            },
            Instruction::Sub { lhs, rhs } => Instruction::Sub {
                lhs: *param_map.get(lhs).unwrap_or(lhs),
                rhs: *param_map.get(rhs).unwrap_or(rhs),
            },
            Instruction::Mul { lhs, rhs } => Instruction::Mul {
                lhs: *param_map.get(lhs).unwrap_or(lhs),
                rhs: *param_map.get(rhs).unwrap_or(rhs),
            },
            Instruction::Div { lhs, rhs } => Instruction::Div {
                lhs: *param_map.get(lhs).unwrap_or(lhs),
                rhs: *param_map.get(rhs).unwrap_or(rhs),
            },
            Instruction::Call {
                callee,
                args,
                effect,
                function_name,
                is_tail_call,
            } => Instruction::Call {
                callee: *param_map.get(callee).unwrap_or(callee),
                args: args
                    .iter()
                    .map(|a| *param_map.get(a).unwrap_or(a))
                    .collect(),
                effect: *effect,
                function_name: function_name.clone(),
                is_tail_call: *is_tail_call,
            },
            _ => instr.clone(), // For other instructions, just clone (they don't use parameters)
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

        func.blocks[0]
            .instructions
            .push((v1, Instruction::ConstInt { value: 5 }));
        func.blocks[0]
            .instructions
            .push((v2, Instruction::ConstInt { value: 3 }));
        func.blocks[0]
            .instructions
            .push((v3, Instruction::Add { lhs: v1, rhs: v2 }));
        func.blocks[0].terminator = Some(Instruction::Return { value: Some(v3) });

        ir.functions.push(func);

        let optimized = Optimizer::optimize(ir);
        let func = &optimized.functions[0];

        // Should have constant folded the addition
        assert!(func.blocks[0].instructions.len() <= 3);

        // Check that v3 is now a constant
        let v3_instr = func.blocks[0]
            .instructions
            .iter()
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

        func.blocks[0]
            .instructions
            .push((v1, Instruction::ConstInt { value: 4 }));
        func.blocks[0]
            .instructions
            .push((v2, Instruction::ConstInt { value: 7 }));
        func.blocks[0]
            .instructions
            .push((v3, Instruction::Mul { lhs: v1, rhs: v2 }));
        func.blocks[0].terminator = Some(Instruction::Return { value: Some(v3) });

        ir.functions.push(func);

        let optimized = Optimizer::optimize(ir);
        let func = &optimized.functions[0];

        let v3_instr = func.blocks[0]
            .instructions
            .iter()
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
        func.blocks[0]
            .instructions
            .push((v1, Instruction::ConstInt { value: 5 }));
        // v2 is dead code (never used)
        func.blocks[0]
            .instructions
            .push((v2, Instruction::ConstInt { value: 10 }));
        // v3 is dead code (never used)
        func.blocks[0]
            .instructions
            .push((v3, Instruction::Add { lhs: v1, rhs: v2 }));
        func.blocks[0].terminator = Some(Instruction::Return { value: Some(v1) });

        ir.functions.push(func);

        let optimized = Optimizer::optimize(ir);
        let func = &optimized.functions[0];

        // v2 and v3 should be eliminated
        let remaining_values: Vec<ValueId> = func.blocks[0]
            .instructions
            .iter()
            .map(|(vid, _)| *vid)
            .collect();

        assert!(
            remaining_values.contains(&v1),
            "v1 should remain (used in return)"
        );
        assert!(
            !remaining_values.contains(&v2),
            "v2 should be eliminated (dead code)"
        );
        assert!(
            !remaining_values.contains(&v3),
            "v3 should be eliminated (dead code)"
        );
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
        func.blocks[0]
            .instructions
            .push((v3, Instruction::Add { lhs: v1, rhs: v2 }));
        // Duplicate x + y
        func.blocks[0]
            .instructions
            .push((v4, Instruction::Add { lhs: v1, rhs: v2 }));
        // Use both (but they should be the same after CSE)
        func.blocks[0]
            .instructions
            .push((v5, Instruction::Add { lhs: v3, rhs: v4 }));
        func.blocks[0].terminator = Some(Instruction::Return { value: Some(v5) });

        ir.functions.push(func);

        let optimized = Optimizer::optimize(ir);
        let func = &optimized.functions[0];

        // After CSE, v4 should be eliminated and replaced with v3
        let remaining_values: Vec<ValueId> = func.blocks[0]
            .instructions
            .iter()
            .map(|(vid, _)| *vid)
            .collect();

        // v3 should remain, v4 should be eliminated (replaced by v3)
        assert!(
            remaining_values.contains(&v3),
            "v3 should remain (first occurrence)"
        );
        assert!(
            !remaining_values.contains(&v4),
            "v4 should be eliminated (duplicate of v3)"
        );
    }
}
