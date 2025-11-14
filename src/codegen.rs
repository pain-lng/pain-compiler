// Code generation - converts Pain IR to LLVM IR

use crate::ir::*;
use std::collections::HashMap;

pub struct CodeGenerator {
    ir: IrProgram,
    llvm_code: String,
    value_map: HashMap<ValueId, String>, // Maps IR ValueId to LLVM value name
    block_map: HashMap<BlockId, String>, // Maps IR BlockId to LLVM label name
    next_register: u32,
    next_label: u32,
}

impl CodeGenerator {
    pub fn new(ir: IrProgram) -> Self {
        Self {
            ir,
            llvm_code: String::new(),
            value_map: HashMap::new(),
            block_map: HashMap::new(),
            next_register: 0,
            next_label: 0,
        }
    }

    /// Generate LLVM IR from Pain IR
    pub fn generate(mut self) -> String {
        self.llvm_code.push_str("; LLVM IR generated from Pain\n");
        self.llvm_code.push_str("target datalayout = \"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128\"\n");
        self.llvm_code.push_str("target triple = \"x86_64-unknown-linux-gnu\"\n\n");

        let functions = self.ir.functions.clone();
        for func in &functions {
            self.generate_function(func);
        }

        self.llvm_code
    }

    fn generate_function(&mut self, func: &IrFunction) {
        // Generate function signature
        let ret_type = self.llvm_type(&func.return_type);
        let mut params = Vec::new();
        
        for (name, _value_id, param_type) in &func.params {
            let llvm_param_type = self.llvm_type(param_type);
            params.push(format!("{} %{}", llvm_param_type, name));
        }

        let params_str = params.join(", ");
        self.llvm_code.push_str(&format!(
            "define {} @{}({}) {{\n",
            ret_type, func.name, params_str
        ));

        // Map parameters to LLVM values
        for (name, value_id, _param_type) in &func.params {
            self.value_map.insert(*value_id, format!("%{}", name));
        }

        // Generate entry block
        let entry_label = self.new_label();
        self.block_map.insert(func.entry_block, entry_label.clone());
        self.llvm_code.push_str(&format!("{}:\n", entry_label));

        // Generate all blocks
        for block in &func.blocks {
            self.generate_block(func, block);
        }

        self.llvm_code.push_str("}\n\n");
        
        // Clear maps for next function
        self.value_map.clear();
        self.block_map.clear();
    }

    fn generate_block(&mut self, func: &IrFunction, block: &BasicBlock) {
        let label = self.block_map.get(&block.id)
            .cloned()
            .unwrap_or_else(|| {
                let new_label = self.new_label();
                self.block_map.insert(block.id, new_label.clone());
                new_label
            });

        // Only print label if it's not the entry block or if it has predecessors
        if !block.predecessors.is_empty() || block.id != func.entry_block {
            self.llvm_code.push_str(&format!("{}:\n", label));
        }

        // Generate instructions
        for (value_id, instruction) in &block.instructions {
            let llvm_value = self.generate_instruction(instruction, *value_id);
            self.value_map.insert(*value_id, llvm_value);
        }

        // Generate terminator
        if let Some(terminator) = &block.terminator {
            self.generate_terminator(terminator);
        }
    }

    fn generate_instruction(&mut self, inst: &Instruction, _result_id: ValueId) -> String {
        let result = self.new_register();
        match inst {
            Instruction::ConstInt { value } => {
                self.llvm_code.push_str(&format!("  {} = add i64 0, {}\n", result, value));
            }
            Instruction::ConstFloat { value } => {
                self.llvm_code.push_str(&format!("  {} = fadd double 0.0, {}\n", result, value));
            }
            Instruction::ConstBool { value } => {
                let val = if *value { 1 } else { 0 };
                self.llvm_code.push_str(&format!("  {} = add i1 0, {}\n", result, val));
            }
            Instruction::ConstString { value } => {
                // String constants need to be global
                let global_name = format!("@str.{}", self.next_register);
                self.llvm_code.push_str(&format!("  {} = getelementptr inbounds [{} x i8], [{} x i8]* {}, i64 0, i64 0\n",
                    result, value.len() + 1, value.len() + 1, global_name));
            }
            Instruction::ConstUnit => {
                // Unit type - void
                return "void".to_string();
            }
            Instruction::Add { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = add i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Sub { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = sub i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Mul { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = mul i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Div { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = sdiv i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Mod { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = srem i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Eq { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = icmp eq i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Ne { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = icmp ne i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Lt { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = icmp slt i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Gt { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = icmp sgt i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Le { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = icmp sle i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Ge { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = icmp sge i64 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::And { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = and i1 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Or { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!("  {} = or i1 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Not { operand } => {
                let op_val = self.get_value(*operand);
                self.llvm_code.push_str(&format!("  {} = xor i1 {}, 1\n", result, op_val));
            }
            Instruction::Neg { operand } => {
                let op_val = self.get_value(*operand);
                self.llvm_code.push_str(&format!("  {} = sub i64 0, {}\n", result, op_val));
            }
            Instruction::Return { value: _ } => {
                // Handled in generate_terminator
                return result;
            }
            Instruction::Branch { .. } | Instruction::Jump { .. } => {
                // Handled in generate_terminator
                return result;
            }
            _ => {
                // TODO: Implement remaining instructions
                self.llvm_code.push_str(&format!("  {} = add i64 0, 0  ; TODO: {}\n", result, format!("{:?}", inst)));
            }
        }
        result
    }

    fn generate_terminator(&mut self, terminator: &Instruction) {
        match terminator {
            Instruction::Return { value } => {
                if let Some(val) = value {
                    let val_str = self.get_value(*val);
                    // Get return type from current function context
                    // For now, assume i64 for integers
                    self.llvm_code.push_str(&format!("  ret i64 {}\n", val_str));
                } else {
                    self.llvm_code.push_str("  ret void\n");
                }
            }
            Instruction::Branch { cond, then_block, else_block } => {
                let cond_val = self.get_value(*cond);
                let then_label = self.block_map.get(then_block)
                    .cloned()
                    .unwrap_or_else(|| {
                        let new_label = self.new_label();
                        self.block_map.insert(*then_block, new_label.clone());
                        new_label
                    });
                let else_label = self.block_map.get(else_block)
                    .cloned()
                    .unwrap_or_else(|| {
                        let new_label = self.new_label();
                        self.block_map.insert(*else_block, new_label.clone());
                        new_label
                    });
                self.llvm_code.push_str(&format!("  br i1 {}, label %{}, label %{}\n", cond_val, then_label, else_label));
            }
            Instruction::Jump { target } => {
                let target_label = self.block_map.get(target)
                    .cloned()
                    .unwrap_or_else(|| {
                        let new_label = self.new_label();
                        self.block_map.insert(*target, new_label.clone());
                        new_label
                    });
                self.llvm_code.push_str(&format!("  br label %{}\n", target_label));
            }
            _ => {
                self.llvm_code.push_str(&format!("  ; TODO: terminator {:?}\n", terminator));
            }
        }
    }

    fn llvm_type(&self, ir_type: &IrType) -> String {
        match ir_type {
            IrType::Int | IrType::Int64 => "i64".to_string(),
            IrType::Int32 => "i32".to_string(),
            IrType::Float32 => "float".to_string(),
            IrType::Float64 => "double".to_string(),
            IrType::Bool => "i1".to_string(),
            IrType::Str => "i8*".to_string(),
            IrType::Unit => "void".to_string(),
            IrType::Pointer(element) => format!("{}*", self.llvm_type(element)),
            IrType::Array { element, size } => {
                if let Some(sz) = size {
                    format!("[{} x {}]", sz, self.llvm_type(element))
                } else {
                    format!("{}*", self.llvm_type(element))
                }
            }
            IrType::Tensor { element, dims: _ } => {
                // Tensors are represented as pointers for now
                format!("{}*", self.llvm_type(element))
            }
            IrType::Function { params, return_type } => {
                let param_types: Vec<String> = params.iter().map(|p| self.llvm_type(p)).collect();
                format!("{} ({})*", self.llvm_type(return_type), param_types.join(", "))
            }
            IrType::Named(_) => "i8*".to_string(), // Placeholder for named types
        }
    }

    fn get_value(&self, value_id: ValueId) -> String {
        self.value_map.get(&value_id)
            .cloned()
            .unwrap_or_else(|| format!("%{}", value_id.0))
    }

    fn new_register(&mut self) -> String {
        let reg = format!("%{}", self.next_register);
        self.next_register += 1;
        reg
    }

    fn new_label(&mut self) -> String {
        let label = format!("L{}", self.next_label);
        self.next_label += 1;
        label
    }
}

