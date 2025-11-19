// MLIR code generation - converts Pain IR to MLIR (Multi-Level IR)
// Supports GPU dialect for GPU kernel generation

use crate::ir::*;
use std::collections::HashMap;

pub struct MlirCodeGenerator {
    ir: IrProgram,
    mlir_code: String,
    value_map: HashMap<ValueId, String>,
    block_map: HashMap<BlockId, String>,
    next_register: u32,
    next_label: u32,
    current_function: Option<FunctionId>,
}

impl MlirCodeGenerator {
    pub fn new(ir: IrProgram) -> Self {
        Self {
            ir,
            mlir_code: String::new(),
            value_map: HashMap::new(),
            block_map: HashMap::new(),
            next_register: 0,
            next_label: 0,
            current_function: None,
        }
    }

    /// Generate MLIR from Pain IR
    pub fn generate(mut self) -> String {
        self.mlir_code.push_str("// MLIR generated from Pain\n");
        self.mlir_code.push_str("module {\n");

        // Generate struct types as memref types
        self.generate_struct_types();

        // Generate user-defined functions
        let functions = self.ir.functions.clone();
        for (idx, func) in functions.iter().enumerate() {
            self.current_function = Some(FunctionId(idx as u32));
            self.generate_function(func);
        }

        self.mlir_code.push_str("}\n");
        self.mlir_code
    }

    fn generate_struct_types(&mut self) {
        // MLIR uses memref for structured data
        // Struct types are represented as memref or custom types
        // For now, we'll use memref for structs
        // TODO: Add proper struct type support
        let _ = &self.ir.structs;
    }

    fn generate_function(&mut self, func: &IrFunction) {
        // Check if this is a GPU kernel
        let is_gpu_kernel = func
            .attributes
            .iter()
            .any(|attr| attr == "@kernel" || attr == "kernel");

        if is_gpu_kernel {
            self.generate_gpu_kernel(func);
        } else {
            self.generate_regular_function(func);
        }
    }

    fn generate_gpu_kernel(&mut self, func: &IrFunction) {
        // Generate GPU kernel using GPU dialect
        let ret_type = self.mlir_type(&func.return_type);
        let mut params = Vec::new();

        for (name, _value_id, param_type) in &func.params {
            let mlir_param_type = self.mlir_type(param_type);
            params.push(format!("{} %{}", mlir_param_type, name));
        }

        let params_str = params.join(", ");
        self.mlir_code.push_str(&format!(
            "  gpu.func @{} kernel({}) -> {} {{\n",
            func.name, params_str, ret_type
        ));

        // Map parameters
        for (name, value_id, _param_type) in &func.params {
            self.value_map.insert(*value_id, format!("%{}", name));
        }

        // Generate entry block
        let entry_label = self.new_label();
        self.block_map.insert(func.entry_block, entry_label.clone());
        self.mlir_code.push_str(&format!("    ^{}:\n", entry_label));

        // Generate all blocks
        for block in &func.blocks {
            self.generate_block(func, block);
        }

        self.mlir_code.push_str("  }\n\n");
        self.value_map.clear();
        self.block_map.clear();
    }

    fn generate_regular_function(&mut self, func: &IrFunction) {
        // Generate regular function using func dialect
        let ret_type = self.mlir_type(&func.return_type);
        let mut params = Vec::new();

        for (name, _value_id, param_type) in &func.params {
            let mlir_param_type = self.mlir_type(param_type);
            params.push(format!("{} %{}", mlir_param_type, name));
        }

        let params_str = params.join(", ");
        self.mlir_code.push_str(&format!(
            "  func.func @{}({}) -> {} {{\n",
            func.name, params_str, ret_type
        ));

        // Map parameters
        for (name, value_id, _param_type) in &func.params {
            self.value_map.insert(*value_id, format!("%{}", name));
        }

        // Generate entry block
        let entry_label = self.new_label();
        self.block_map.insert(func.entry_block, entry_label.clone());
        self.mlir_code.push_str(&format!("    ^{}:\n", entry_label));

        // Generate all blocks
        for block in &func.blocks {
            self.generate_block(func, block);
        }

        self.mlir_code.push_str("  }\n\n");
        self.value_map.clear();
        self.block_map.clear();
    }

    fn generate_block(&mut self, func: &IrFunction, block: &BasicBlock) {
        let label = self.block_map.get(&block.id).cloned().unwrap_or_else(|| {
            let new_label = self.new_label();
            self.block_map.insert(block.id, new_label.clone());
            new_label
        });

        // Only print label if it's not the entry block or if it has predecessors
        if !block.predecessors.is_empty() || block.id != func.entry_block {
            self.mlir_code.push_str(&format!("    ^{}:\n", label));
        }

        // Generate instructions
        for (value_id, instruction) in &block.instructions {
            let mlir_value = self.generate_instruction(instruction, *value_id);
            if !mlir_value.is_empty() {
                self.value_map.insert(*value_id, mlir_value);
            }
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
                self.mlir_code.push_str(&format!(
                    "      {} = arith.constant {} : i64\n",
                    result, value
                ));
            }
            Instruction::ConstFloat { value } => {
                self.mlir_code.push_str(&format!(
                    "      {} = arith.constant {} : f64\n",
                    result, value
                ));
            }
            Instruction::ConstBool { value } => {
                let val = if *value { "true" } else { "false" };
                self.mlir_code
                    .push_str(&format!("      {} = arith.constant {} : i1\n", result, val));
            }
            Instruction::ConstString { value } => {
                // Strings as memref or tensor
                self.mlir_code.push_str(&format!(
                    "      {} = arith.constant dense<\"{}\"> : tensor<{}xi8>\n",
                    result,
                    value,
                    value.len()
                ));
            }
            Instruction::ConstUnit => {
                return String::new();
            }
            Instruction::Add { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.addi {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Sub { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.subi {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Mul { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.muli {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Div { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.divsi {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Mod { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.remsi {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Eq { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.cmpi eq, {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Ne { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.cmpi ne, {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Lt { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.cmpi slt, {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Gt { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.cmpi sgt, {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Le { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.cmpi sle, {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Ge { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.cmpi sge, {}, {} : i64\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::And { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.andi {}, {} : i1\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Or { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.mlir_code.push_str(&format!(
                    "      {} = arith.ori {}, {} : i1\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Not { operand } => {
                let op_val = self.get_value(*operand);
                // Not: xor with true (1)
                let true_val = self.new_register();
                self.mlir_code
                    .push_str(&format!("      {} = arith.constant true : i1\n", true_val));
                self.mlir_code.push_str(&format!(
                    "      {} = arith.xori {}, {} : i1\n",
                    result, op_val, true_val
                ));
            }
            Instruction::Neg { operand } => {
                let op_val = self.get_value(*operand);
                self.mlir_code
                    .push_str(&format!("      {} = arith.negf {} : f64\n", result, op_val));
            }
            Instruction::TensorOp { op, args } => {
                self.generate_tensor_op(op, args, &result);
            }
            Instruction::Call {
                function_name,
                args,
                ..
            } => {
                if let Some(name) = function_name {
                    let arg_values: Vec<String> =
                        args.iter().map(|arg_id| self.get_value(*arg_id)).collect();
                    let args_str = arg_values.join(", ");
                    self.mlir_code.push_str(&format!(
                        "      {} = func.call @{}({}) : (",
                        result, name, args_str
                    ));
                    // TODO: Add proper type signatures
                    self.mlir_code.push_str(") -> i64\n");
                }
            }
            Instruction::Return { value } => {
                if let Some(val) = value {
                    let val_str = self.get_value(*val);
                    self.mlir_code
                        .push_str(&format!("      func.return {} : i64\n", val_str));
                } else {
                    self.mlir_code.push_str("      func.return\n");
                }
                return String::new();
            }
            _ => {
                // Fallback for unimplemented instructions
                self.mlir_code.push_str(&format!(
                    "      // TODO: Implement instruction {:?}\n",
                    inst
                ));
            }
        }
        result
    }

    fn generate_tensor_op(&mut self, op: &TensorOp, args: &[ValueId], result: &str) {
        let arg_values: Vec<String> = args.iter().map(|arg_id| self.get_value(*arg_id)).collect();

        match op {
            TensorOp::Add => {
                if arg_values.len() >= 2 {
                    self.mlir_code.push_str(&format!(
                        "      {} = arith.addf {}, {} : tensor<f32>\n",
                        result, arg_values[0], arg_values[1]
                    ));
                }
            }
            TensorOp::Mul => {
                if arg_values.len() >= 2 {
                    self.mlir_code.push_str(&format!(
                        "      {} = arith.mulf {}, {} : tensor<f32>\n",
                        result, arg_values[0], arg_values[1]
                    ));
                }
            }
            TensorOp::MatMul => {
                if arg_values.len() >= 2 {
                    // Use linalg.matmul for matrix multiplication
                    self.mlir_code.push_str(&format!(
                        "      {} = linalg.matmul ins({}, {}: tensor<f32>, tensor<f32>) -> tensor<f32>\n",
                        result, arg_values[0], arg_values[1]
                    ));
                }
            }
            TensorOp::Relu => {
                if !arg_values.is_empty() {
                    // ReLU: max(0, x)
                    self.mlir_code.push_str(&format!(
                        "      {} = arith.maxf {}, %zero : tensor<f32>\n",
                        result, arg_values[0]
                    ));
                }
            }
            TensorOp::Transpose => {
                if !arg_values.is_empty() {
                    self.mlir_code.push_str(&format!(
                        "      {} = linalg.transpose ins({}: tensor<f32>) -> tensor<f32>\n",
                        result, arg_values[0]
                    ));
                }
            }
            TensorOp::Reshape => {
                if arg_values.len() >= 2 {
                    self.mlir_code.push_str(&format!(
                        "      {} = tensor.reshape {} {{\n",
                        result, arg_values[0]
                    ));
                    // TODO: Add shape information
                    self.mlir_code.push_str("      }\n");
                }
            }
        }
    }

    fn generate_terminator(&mut self, terminator: &Instruction) {
        match terminator {
            Instruction::Branch {
                cond,
                then_block,
                else_block,
            } => {
                let cond_val = self.get_value(*cond);
                let then_label = self.block_map.get(then_block).cloned().unwrap_or_else(|| {
                    let label = self.new_label();
                    self.block_map.insert(*then_block, label.clone());
                    label
                });
                let else_label = self.block_map.get(else_block).cloned().unwrap_or_else(|| {
                    let label = self.new_label();
                    self.block_map.insert(*else_block, label.clone());
                    label
                });
                self.mlir_code.push_str(&format!(
                    "      cf.cond_br {}, ^{}, ^{}\n",
                    cond_val, then_label, else_label
                ));
            }
            Instruction::Jump { target } => {
                let target_label = self.block_map.get(target).cloned().unwrap_or_else(|| {
                    let label = self.new_label();
                    self.block_map.insert(*target, label.clone());
                    label
                });
                self.mlir_code
                    .push_str(&format!("      cf.br ^{}\n", target_label));
            }
            Instruction::Return { value } => {
                if let Some(val) = value {
                    let val_str = self.get_value(*val);
                    self.mlir_code
                        .push_str(&format!("      func.return {} : i64\n", val_str));
                } else {
                    self.mlir_code.push_str("      func.return\n");
                }
            }
            _ => {}
        }
    }

    fn mlir_type(&self, ty: &IrType) -> String {
        match ty {
            IrType::Int | IrType::Int64 => "i64".to_string(),
            IrType::Int32 => "i32".to_string(),
            IrType::Float32 => "f32".to_string(),
            IrType::Float64 => "f64".to_string(),
            IrType::Bool => "i1".to_string(),
            IrType::Str => "memref<?xi8>".to_string(),
            IrType::Unit => "()".to_string(),
            IrType::Pointer(inner) => {
                format!("memref<{}>", self.mlir_type(inner))
            }
            IrType::Array { element, size } => {
                let elem_type = self.mlir_type(element);
                if let Some(sz) = size {
                    format!("memref<{}x{}>", sz, elem_type)
                } else {
                    format!("memref<?x{}>", elem_type)
                }
            }
            IrType::Tensor { element, dims } => {
                let elem_type = self.mlir_type(element);
                let dims_str: Vec<String> = dims
                    .iter()
                    .map(|d| d.map(|s| s.to_string()).unwrap_or_else(|| "?".to_string()))
                    .collect();
                if dims_str.is_empty() {
                    format!("tensor<{}>", elem_type)
                } else {
                    format!("tensor<{}x{}>", dims_str.join("x"), elem_type)
                }
            }
            IrType::Struct { name, .. } => {
                format!("%struct.{}", name)
            }
            IrType::Function {
                params,
                return_type,
            } => {
                let param_types: Vec<String> = params.iter().map(|p| self.mlir_type(p)).collect();
                let ret_type = self.mlir_type(return_type);
                format!("({}) -> {}", param_types.join(", "), ret_type)
            }
            IrType::Named(name) => format!("%{}", name),
        }
    }

    fn get_value(&self, value_id: ValueId) -> String {
        self.value_map
            .get(&value_id)
            .cloned()
            .unwrap_or_else(|| format!("%v{}", value_id.0))
    }

    fn new_register(&mut self) -> String {
        let reg = format!("%{}", self.next_register);
        self.next_register += 1;
        reg
    }

    fn new_label(&mut self) -> String {
        let label = format!("bb{}", self.next_label);
        self.next_label += 1;
        label
    }
}
