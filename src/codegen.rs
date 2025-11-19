// Code generation - converts Pain IR to LLVM IR

use crate::ir::*;
use crate::stdlib::get_stdlib_functions;
use std::collections::HashMap;

pub struct CodeGenerator {
    ir: IrProgram,
    llvm_code: String,
    value_map: HashMap<ValueId, String>, // Maps IR ValueId to LLVM value name
    block_map: HashMap<BlockId, String>, // Maps IR BlockId to LLVM label name
    string_constants: HashMap<String, String>, // Maps string values to global variable names
    next_register: u32,
    next_label: u32,
    next_string_id: u32,
    current_function: Option<FunctionId>, // Track current function for return type
}

impl CodeGenerator {
    pub fn new(ir: IrProgram) -> Self {
        Self {
            ir,
            llvm_code: String::new(),
            value_map: HashMap::new(),
            block_map: HashMap::new(),
            string_constants: HashMap::new(),
            next_register: 0,
            next_label: 0,
            next_string_id: 0,
            current_function: None,
        }
    }

    /// Generate LLVM IR from Pain IR
    pub fn generate(self) -> String {
        self.generate_with_target(None)
    }

    /// Generate LLVM IR from Pain IR with specific target triple
    pub fn generate_with_target(mut self, target_triple: Option<&str>) -> String {
        self.llvm_code.push_str("; LLVM IR generated from Pain\n");
        self.llvm_code.push_str("target datalayout = \"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128\"\n");
        let triple = target_triple.unwrap_or("x86_64-unknown-linux-gnu");
        self.llvm_code
            .push_str(&format!("target triple = \"{}\"\n\n", triple));

        // Generate struct type definitions
        self.generate_struct_types();

        // Declare stdlib functions
        self.declare_stdlib_functions();

        // Generate string constants as global variables
        self.generate_string_constants();

        // Generate user-defined functions
        let functions = self.ir.functions.clone();
        for (idx, func) in functions.iter().enumerate() {
            self.current_function = Some(FunctionId(idx as u32));
            self.generate_function(func);
        }

        self.llvm_code
    }

    fn generate_struct_types(&mut self) {
        for struct_def in &self.ir.structs {
            self.llvm_code
                .push_str(&format!("%struct.{} = type {{", struct_def.name));
            let field_types: Vec<String> = struct_def
                .fields
                .iter()
                .map(|(_, ty)| self.llvm_type(ty))
                .collect();
            self.llvm_code.push_str(&field_types.join(", "));
            self.llvm_code.push_str("}\n");
        }
        if !self.ir.structs.is_empty() {
            self.llvm_code.push('\n');
        }
    }

    fn generate_string_constants(&mut self) {
        // Add format string for print function (%s\n)
        // "%s\n\0" = 4 bytes: '%', 's', '\n', '\0'
        self.llvm_code
            .push_str("@str.fmt.print = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\"\n");

        // Collect all string constants from IR
        for func in &self.ir.functions {
            for block in &func.blocks {
                for (_value_id, instruction) in &block.instructions {
                    if let Instruction::ConstString { value } = instruction {
                        if !self.string_constants.contains_key(value) {
                            let global_name = format!("@str.{}", self.next_string_id);
                            self.next_string_id += 1;
                            self.string_constants
                                .insert(value.clone(), global_name.clone());

                            // Generate global constant
                            self.llvm_code.push_str(&format!(
                                "{} = private unnamed_addr constant [{} x i8] c\"{}\\00\"\n",
                                global_name,
                                value.len() + 1,
                                value
                                    .replace("\\", "\\5C")
                                    .replace("\"", "\\22")
                                    .replace("\n", "\\0A")
                            ));
                        }
                    }
                }
            }
        }
        // Always add newline after string constants section
        self.llvm_code.push('\n');
    }

    fn declare_stdlib_functions(&mut self) {
        // Declare malloc for struct allocation
        self.llvm_code.push_str("declare i8* @malloc(i64)\n");
        self.llvm_code.push_str("declare void @free(i8*)\n\n");
        let stdlib_funcs = get_stdlib_functions();
        let mut declared = std::collections::HashSet::new();

        for func in &stdlib_funcs {
            // Avoid duplicate declarations
            if declared.contains(&func.name) {
                continue;
            }

            // Generate declaration based on function signature
            let param_types: Vec<String> = func
                .params
                .iter()
                .map(|(_, ty)| self.llvm_type_from_ast(ty))
                .collect();
            let ret_type = self.llvm_type_from_ast(&func.return_type);

            // Special handling for print (void return, but takes dynamic type)
            if func.name == "print" {
                // print uses printf from C standard library
                // Declare printf if not already declared
                if !declared.contains("printf") {
                    self.llvm_code.push_str("declare i32 @printf(i8*, ...)\n");
                    declared.insert("printf".to_string());
                }
            } else {
                self.llvm_code.push_str(&format!(
                    "declare {} @{}({})\n",
                    ret_type,
                    func.name,
                    param_types.join(", ")
                ));
            }

            declared.insert(func.name.clone());
        }

        if !declared.is_empty() {
            self.llvm_code.push('\n');
        }
    }

    fn llvm_type_from_ast(&self, ty: &crate::ast::Type) -> String {
        match ty {
            crate::ast::Type::Int => "i64".to_string(),
            crate::ast::Type::Float32 => "float".to_string(),
            crate::ast::Type::Float64 => "double".to_string(),
            crate::ast::Type::Bool => "i1".to_string(),
            crate::ast::Type::Str => "i8*".to_string(),
            crate::ast::Type::Dynamic => "i8*".to_string(), // Dynamic types as generic pointers
            _ => "i8*".to_string(),                         // Default to pointer for complex types
        }
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
        let label = self.block_map.get(&block.id).cloned().unwrap_or_else(|| {
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
            // Only store in value_map if it's not void (empty string)
            if !llvm_value.is_empty() {
                self.value_map.insert(*value_id, llvm_value);
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
                self.llvm_code
                    .push_str(&format!("  {} = add i64 0, {}\n", result, value));
            }
            Instruction::ConstFloat { value } => {
                self.llvm_code
                    .push_str(&format!("  {} = fadd double 0.0, {}\n", result, value));
            }
            Instruction::ConstBool { value } => {
                let val = if *value { 1 } else { 0 };
                self.llvm_code
                    .push_str(&format!("  {} = add i1 0, {}\n", result, val));
            }
            Instruction::ConstString { value } => {
                // String constants are global variables
                let global_name = self
                    .string_constants
                    .get(value)
                    .cloned()
                    .unwrap_or_else(|| {
                        // If not found, create it (shouldn't happen if generate_string_constants was called)
                        let name = format!("@str.{}", self.next_string_id);
                        self.next_string_id += 1;
                        self.string_constants.insert(value.clone(), name.clone());
                        name
                    });
                // Get pointer to string constant
                self.llvm_code.push_str(&format!(
                    "  {} = getelementptr inbounds [{} x i8], [{} x i8]* {}, i64 0, i64 0\n",
                    result,
                    value.len() + 1,
                    value.len() + 1,
                    global_name
                ));
            }
            Instruction::ConstUnit => {
                // Unit type - void
                return "void".to_string();
            }
            Instruction::Add { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = add i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Sub { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = sub i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Mul { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = mul i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Div { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = sdiv i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Mod { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = srem i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Eq { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = icmp eq i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Ne { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = icmp ne i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Lt { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = icmp slt i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Gt { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = icmp sgt i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Le { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = icmp sle i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::Ge { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code.push_str(&format!(
                    "  {} = icmp sge i64 {}, {}\n",
                    result, lhs_val, rhs_val
                ));
            }
            Instruction::And { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code
                    .push_str(&format!("  {} = and i1 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Or { lhs, rhs } => {
                let lhs_val = self.get_value(*lhs);
                let rhs_val = self.get_value(*rhs);
                self.llvm_code
                    .push_str(&format!("  {} = or i1 {}, {}\n", result, lhs_val, rhs_val));
            }
            Instruction::Not { operand } => {
                let op_val = self.get_value(*operand);
                self.llvm_code
                    .push_str(&format!("  {} = xor i1 {}, 1\n", result, op_val));
            }
            Instruction::Neg { operand } => {
                let op_val = self.get_value(*operand);
                self.llvm_code
                    .push_str(&format!("  {} = sub i64 0, {}\n", result, op_val));
            }
            Instruction::Return { value: _ } => {
                // Handled in generate_terminator
                return result;
            }
            Instruction::Branch { .. } | Instruction::Jump { .. } => {
                // Handled in generate_terminator
                return result;
            }
            Instruction::Load { ptr, .. } => {
                // Load value from pointer
                let ptr_val = self.get_value(*ptr);
                // For now, assume loading i64 (we'd need type info to be more precise)
                self.llvm_code
                    .push_str(&format!("  {} = load i64, i64* {}\n", result, ptr_val));
            }
            Instruction::Store { ptr, value, .. } => {
                // Store value to pointer
                let ptr_val = self.get_value(*ptr);
                let val = self.get_value(*value);
                // For now, assume storing i64
                self.llvm_code
                    .push_str(&format!("  store i64 {}, i64* {}\n", val, ptr_val));
                return "void".to_string();
            }
            Instruction::Alloc { size, align, .. } => {
                // Allocate memory on stack using alloca
                // Syntax: alloca <type> [, <ty> <NumElements>] [, align <alignment>]
                // For simplicity, allocate i64 array of given size
                let size_val = self.get_value(*size);
                if *align > 0 {
                    self.llvm_code.push_str(&format!(
                        "  {} = alloca i64, i64 {}, align {}\n",
                        result, size_val, align
                    ));
                } else {
                    self.llvm_code
                        .push_str(&format!("  {} = alloca i64, i64 {}\n", result, size_val));
                }
            }
            Instruction::Free { ptr, .. } => {
                // Free memory (for stack allocations, this is a no-op in LLVM)
                // For heap allocations, we'd need to call free() function
                let _ptr_val = self.get_value(*ptr);
                // Stack allocations are automatically freed, so this is a no-op
                // For heap allocations, we'd generate: call void @free(i8* %ptr)
                return "void".to_string();
            }
            Instruction::Phi { incoming } => {
                // Generate phi node for SSA form
                // Phi nodes need to know the type - for now assume i64
                let incoming_strs: Vec<String> = incoming
                    .iter()
                    .map(|(block_id, value_id)| {
                        let block_label = self
                            .block_map
                            .get(block_id)
                            .cloned()
                            .unwrap_or_else(|| format!("L{}", block_id.0));
                        let val = self.get_value(*value_id);
                        format!("[ {}, %{} ]", val, block_label)
                    })
                    .collect();
                self.llvm_code.push_str(&format!(
                    "  {} = phi i64 {}\n",
                    result,
                    incoming_strs.join(", ")
                ));
            }
            Instruction::Call {
                callee: _,
                args,
                function_name,
                is_tail_call,
                ..
            } => {
                // Generate function call
                if let Some(func_name) = function_name {
                    // Static call to user-defined function
                    // Find function in IR to get return type
                    let func = self.ir.functions.iter().find(|f| f.name == *func_name);

                    if let Some(func) = func {
                        let ret_type = self.llvm_type(&func.return_type);
                        let arg_values: Vec<String> =
                            args.iter().map(|arg_id| self.get_value(*arg_id)).collect();

                        // Get parameter types from function
                        let param_types: Vec<String> = func
                            .params
                            .iter()
                            .map(|(_, _, ty)| self.llvm_type(ty))
                            .collect();

                        // Add tail call attribute if this is a tail call
                        let tail_attr = if *is_tail_call { "tail " } else { "" };

                        if ret_type == "void" {
                            self.llvm_code.push_str(&format!(
                                "  {}call void @{}({})\n",
                                tail_attr,
                                func_name,
                                arg_values
                                    .iter()
                                    .zip(param_types.iter())
                                    .map(|(val, ty)| format!("{} {}", ty, val))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ));
                            return "void".to_string();
                        } else {
                            self.llvm_code.push_str(&format!(
                                "  {} = {}call {} @{}({})\n",
                                result,
                                tail_attr,
                                ret_type,
                                func_name,
                                arg_values
                                    .iter()
                                    .zip(param_types.iter())
                                    .map(|(val, ty)| format!("{} {}", ty, val))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ));
                        }
                    } else {
                        // Function not found - generate placeholder
                        self.llvm_code.push_str(&format!(
                            "  {} = add i64 0, 0  ; TODO: call {}\n",
                            result, func_name
                        ));
                    }
                } else {
                    // Dynamic call - not yet supported
                    self.llvm_code.push_str(&format!(
                        "  {} = add i64 0, 0  ; TODO: dynamic call\n",
                        result
                    ));
                }
            }
            Instruction::Intrinsic { name, args, .. } => {
                // Generate stdlib function call
                let arg_values: Vec<String> =
                    args.iter().map(|arg_id| self.get_value(*arg_id)).collect();

                // Map stdlib function names to LLVM intrinsics or external functions
                match name.as_str() {
                    "print" => {
                        // print uses printf from C standard library
                        if let Some(first_arg) = arg_values.first() {
                            // Use inline format string "%s\n" for strings
                            // For strings (i8*), use %s format
                            self.llvm_code.push_str(&format!(
                                "  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str.fmt.print, i64 0, i64 0), i8* {})\n",
                                first_arg
                            ));
                        }
                        // Return empty string for void, don't store in value_map
                        return String::new();
                    }
                    "abs" | "min" | "max" | "sqrt" | "pow" | "sin" | "cos" | "floor" | "ceil" => {
                        // Math functions - assume they take and return double for now
                        let ret_type = if name == "abs" && args.len() == 1 {
                            // Check if argument is integer (would need type info)
                            "i64"
                        } else {
                            "double"
                        };

                        let param_type = if name == "abs" && args.len() == 1 {
                            "i64"
                        } else {
                            "double"
                        };

                        if ret_type == "void" {
                            self.llvm_code.push_str(&format!(
                                "  call void @{}({})\n",
                                name,
                                arg_values
                                    .iter()
                                    .map(|val| format!("{} {}", param_type, val))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ));
                            return "void".to_string();
                        } else {
                            self.llvm_code.push_str(&format!(
                                "  {} = call {} @{}({})\n",
                                result,
                                ret_type,
                                name,
                                arg_values
                                    .iter()
                                    .map(|val| format!("{} {}", param_type, val))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ));
                        }
                    }
                    _ => {
                        // Unknown intrinsic - generate placeholder
                        self.llvm_code.push_str(&format!(
                            "  {} = add i64 0, 0  ; TODO: intrinsic {}\n",
                            result, name
                        ));
                    }
                }
            }
            Instruction::AllocStruct {
                struct_name,
                fields,
            } => {
                // Allocate memory for struct
                let struct_type = format!("%struct.{}", struct_name);
                let struct_ptr_type = format!("{}*", struct_type);

                // Allocate memory using malloc
                let size = self.calculate_struct_size(struct_name);
                let alloc_val = self.new_register();
                self.llvm_code.push_str(&format!(
                    "  {} = call i8* @malloc(i64 {})\n",
                    alloc_val, size
                ));

                // Cast to struct pointer
                self.llvm_code.push_str(&format!(
                    "  {} = bitcast i8* {} to {}*\n",
                    result, alloc_val, struct_ptr_type
                ));

                // Store field values
                let struct_def = self.ir.get_struct(struct_name).cloned();
                if let Some(struct_def) = struct_def {
                    for (idx, field_val) in fields.iter().enumerate() {
                        if idx < struct_def.fields.len() {
                            let (_, field_type) = &struct_def.fields[idx];
                            let field_llvm_type = self.llvm_type(field_type);
                            let gep_val = self.new_register();
                            self.llvm_code.push_str(&format!(
                                "  {} = getelementptr inbounds {}, {}* {}, i32 0, i32 {}\n",
                                gep_val, struct_type, struct_ptr_type, result, idx
                            ));
                            let field_val_str = self.get_value(*field_val);
                            self.llvm_code.push_str(&format!(
                                "  store {} {}, {}* {}\n",
                                field_llvm_type, field_val_str, field_llvm_type, gep_val
                            ));
                        }
                    }
                }
            }
            Instruction::GetField { obj, field_name } => {
                // Get struct pointer
                let obj_str = self.get_value(*obj);

                // Find struct type - need to track types better, for now use placeholder
                // In real implementation, we'd track the type of obj
                if let Some(struct_def) = self
                    .ir
                    .structs
                    .iter()
                    .find(|s| s.fields.iter().any(|(name, _)| name == field_name))
                {
                    let struct_type = format!("%struct.{}", struct_def.name);
                    let struct_ptr_type = format!("{}*", struct_type);

                    // Find field index
                    if let Some((field_idx, (_, field_type))) = struct_def
                        .fields
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _))| name == field_name)
                    {
                        let field_llvm_type = self.llvm_type(field_type);
                        let gep_val = self.new_register();
                        self.llvm_code.push_str(&format!(
                            "  {} = getelementptr inbounds {}, {}* {}, i32 0, i32 {}\n",
                            gep_val, struct_type, struct_ptr_type, obj_str, field_idx
                        ));

                        self.llvm_code.push_str(&format!(
                            "  {} = load {}, {}* {}\n",
                            result, field_llvm_type, field_llvm_type, gep_val
                        ));
                    } else {
                        // Field not found - generate placeholder
                        self.llvm_code.push_str(&format!(
                            "  {} = add i64 0, 0  ; TODO: GetField {} (field not found)\n",
                            result, field_name
                        ));
                    }
                } else {
                    // Struct not found - generate placeholder
                    self.llvm_code.push_str(&format!(
                        "  {} = add i64 0, 0  ; TODO: GetField {} (struct not found)\n",
                        result, field_name
                    ));
                }
            }
            Instruction::SetField {
                obj,
                field_name,
                value,
            } => {
                // Get struct pointer and value
                let obj_str = self.get_value(*obj);
                let val_str = self.get_value(*value);

                // Find struct type
                if let Some(struct_def) = self
                    .ir
                    .structs
                    .iter()
                    .find(|s| s.fields.iter().any(|(name, _)| name == field_name))
                {
                    let struct_type = format!("%struct.{}", struct_def.name);
                    let struct_ptr_type = format!("{}*", struct_type);

                    // Find field index
                    if let Some((field_idx, (_, field_type))) = struct_def
                        .fields
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _))| name == field_name)
                    {
                        let field_llvm_type = self.llvm_type(field_type);
                        let gep_val = self.new_register();
                        self.llvm_code.push_str(&format!(
                            "  {} = getelementptr inbounds {}, {}* {}, i32 0, i32 {}\n",
                            gep_val, struct_type, struct_ptr_type, obj_str, field_idx
                        ));

                        self.llvm_code.push_str(&format!(
                            "  store {} {}, {}* {}\n",
                            field_llvm_type, val_str, field_llvm_type, gep_val
                        ));

                        // SetField returns the stored value
                        return val_str;
                    }
                }

                // Fallback: generate placeholder
                self.llvm_code.push_str(&format!(
                    "  {} = add i64 0, 0  ; TODO: SetField {}\n",
                    result, field_name
                ));
            }
            _ => {
                // TODO: Implement remaining instructions
                self.llvm_code.push_str(&format!(
                    "  {} = add i64 0, 0  ; TODO: {}\n",
                    result,
                    format!("{:?}", inst)
                ));
            }
        }
        result
    }

    fn calculate_struct_size(&self, struct_name: &str) -> u64 {
        // Calculate size of struct in bytes (simplified)
        // In real implementation, would need proper alignment calculation
        if let Some(struct_def) = self.ir.get_struct(struct_name) {
            let mut size = 0u64;
            for (_, field_type) in &struct_def.fields {
                size += self.type_size(field_type);
            }
            size
        } else {
            8 // Default size
        }
    }

    fn type_size(&self, ir_type: &IrType) -> u64 {
        match ir_type {
            IrType::Int | IrType::Int64 => 8,
            IrType::Int32 => 4,
            IrType::Float32 => 4,
            IrType::Float64 => 8,
            IrType::Bool => 1,
            IrType::Str => 8, // Pointer size
            IrType::Pointer(_) => 8,
            IrType::Struct { name, .. } => self.calculate_struct_size(name),
            _ => 8, // Default
        }
    }

    fn generate_terminator(&mut self, terminator: &Instruction) {
        match terminator {
            Instruction::Return { value } => {
                if let Some(val) = value {
                    let val_str = self.get_value(*val);
                    // Get return type from current function
                    let ret_type = if let Some(func_id) = self.current_function {
                        if let Some(func) = self.ir.functions.get(func_id.0 as usize) {
                            self.llvm_type(&func.return_type)
                        } else {
                            "i64".to_string() // Fallback
                        }
                    } else {
                        "i64".to_string() // Fallback
                    };
                    self.llvm_code
                        .push_str(&format!("  ret {} {}\n", ret_type, val_str));
                } else {
                    self.llvm_code.push_str("  ret void\n");
                }
            }
            Instruction::Branch {
                cond,
                then_block,
                else_block,
            } => {
                let cond_val = self.get_value(*cond);
                let then_label = self.block_map.get(then_block).cloned().unwrap_or_else(|| {
                    let new_label = self.new_label();
                    self.block_map.insert(*then_block, new_label.clone());
                    new_label
                });
                let else_label = self.block_map.get(else_block).cloned().unwrap_or_else(|| {
                    let new_label = self.new_label();
                    self.block_map.insert(*else_block, new_label.clone());
                    new_label
                });
                self.llvm_code.push_str(&format!(
                    "  br i1 {}, label %{}, label %{}\n",
                    cond_val, then_label, else_label
                ));
            }
            Instruction::Jump { target } => {
                let target_label = self.block_map.get(target).cloned().unwrap_or_else(|| {
                    let new_label = self.new_label();
                    self.block_map.insert(*target, new_label.clone());
                    new_label
                });
                self.llvm_code
                    .push_str(&format!("  br label %{}\n", target_label));
            }
            _ => {
                self.llvm_code
                    .push_str(&format!("  ; TODO: terminator {:?}\n", terminator));
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
            IrType::Function {
                params,
                return_type,
            } => {
                let param_types: Vec<String> = params.iter().map(|p| self.llvm_type(p)).collect();
                format!(
                    "{} ({})*",
                    self.llvm_type(return_type),
                    param_types.join(", ")
                )
            }
            IrType::Struct { name, .. } => {
                format!("%struct.{}*", name) // Structs are passed by pointer
            }
            IrType::Named(name) => {
                // Try to resolve named type to struct
                if let Some(struct_def) = self.ir.get_struct(name) {
                    format!("%struct.{}*", struct_def.name)
                } else {
                    "i8*".to_string() // Placeholder for unknown named types
                }
            }
        }
    }

    fn get_value(&self, value_id: ValueId) -> String {
        self.value_map
            .get(&value_id)
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
