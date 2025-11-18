// IR Builder - converts AST to SSA-based IR

use crate::ast::*;
use crate::ir::*;
use crate::stdlib::is_stdlib_function;
use std::collections::HashMap;

pub struct IrBuilder {
    program: IrProgram,
    current_function: Option<FunctionId>,
    current_block: Option<BlockId>,
    variable_map: HashMap<String, ValueId>, // Maps variable names to SSA values
    function_map: HashMap<String, FunctionId>, // Maps function names to FunctionId
    class_map: HashMap<String, String>, // Maps class names to struct names (for now, same name)
    block_stack: Vec<BlockId>, // For tracking control flow
    // Loop context for break/continue
    loop_headers: Vec<BlockId>, // Stack of loop header blocks
    loop_continues: Vec<BlockId>, // Stack of loop continue blocks (after loop)
}

impl IrBuilder {
    pub fn new() -> Self {
        Self {
            program: IrProgram::new(),
            current_function: None,
            current_block: None,
            variable_map: HashMap::new(),
            function_map: HashMap::new(),
            class_map: HashMap::new(),
            block_stack: Vec::new(),
            loop_headers: Vec::new(),
            loop_continues: Vec::new(),
        }
    }

    /// Build IR from AST program
    pub fn build(mut self, ast: &Program) -> IrProgram {
        // First pass: build structs from classes
        for item in &ast.items {
            if let Item::Class(class) = item {
                self.build_class_struct(class);
            }
        }
        
        // Second pass: build functions (including methods)
        for item in &ast.items {
            match item {
                Item::Function(func) => {
                    self.build_function(func);
                }
                Item::Class(class) => {
                    // Build methods as functions with mangled names
                    for method in &class.methods {
                        self.build_method(class, method);
                    }
                }
            }
        }
        self.program
    }
    
    fn build_class_struct(&mut self, class: &Class) {
        // Convert class fields to IR struct fields
        let fields: Vec<(String, IrType)> = class.fields.iter()
            .map(|f| (f.name.clone(), IrType::from(&f.ty)))
            .collect();
        
        self.program.add_struct(class.name.clone(), fields);
        self.class_map.insert(class.name.clone(), class.name.clone());
    }
    
    /// Resolve Named types to Struct types if they refer to classes
    fn resolve_named_type(&self, name: &str) -> Option<IrType> {
        if self.class_map.contains_key(name) {
            if let Some(struct_def) = self.program.get_struct(name) {
                Some(IrType::Struct {
                    name: struct_def.name.clone(),
                    fields: struct_def.fields.clone(),
                })
            } else {
                None
            }
        } else {
            None
        }
    }
    
    fn build_method(&mut self, class: &Class, method: &Function) {
        // Build method as a function with mangled name: ClassName_methodName
        let mangled_name = format!("{}__{}", class.name, method.name);
        
        let func_id = self.program.new_function_id();
        let entry_block_id = self.program.new_block_id();
        let entry_block = BasicBlock::new(entry_block_id);
        
        // Add 'self' as first parameter
        let mut params = Vec::new();
        let self_type = IrType::Struct {
            name: class.name.clone(),
            fields: class.fields.iter()
                .map(|f| (f.name.clone(), IrType::from(&f.ty)))
                .collect(),
        };
        let self_value = self.program.new_value_id();
        params.push(("self".to_string(), self_value, self_type.clone()));
        self.variable_map.insert("self".to_string(), self_value);
        
        // Add method parameters
        for param in &method.params {
            let param_value = self.program.new_value_id();
            let ir_type = IrType::from(&param.ty);
            params.push((param.name.clone(), param_value, ir_type.clone()));
            self.variable_map.insert(param.name.clone(), param_value);
        }
        
        // Convert return type
        let return_type = method.return_type.as_ref()
            .map(|t| IrType::from(t))
            .unwrap_or(IrType::Unit);
        
        // Create function
        let ir_func = IrFunction {
            id: func_id,
            name: mangled_name.clone(),
            params,
            return_type,
            blocks: vec![entry_block],
            entry_block: entry_block_id,
            attributes: method.attrs.iter().map(|a| a.name.clone()).collect(),
        };
        
        self.program.functions.push(ir_func);
        self.function_map.insert(mangled_name, func_id);
        self.current_function = Some(func_id);
        self.current_block = Some(entry_block_id);
        
        // Build method body
        for stmt in &method.body {
            self.build_statement(stmt);
        }
        
        // Ensure method has a return if needed
        let func = &mut self.program.functions[func_id.0 as usize];
        let current_block_id = self.current_block.unwrap();
        let current_block = func.blocks.iter_mut()
            .find(|b| b.id == current_block_id)
            .expect("Current block not found");
        if current_block.terminator.is_none() {
            if matches!(func.return_type, IrType::Unit) {
                current_block.terminator = Some(Instruction::Return { value: None });
            } else {
                current_block.terminator = Some(Instruction::Return { value: None });
            }
        }
        
        self.current_function = None;
        self.current_block = None;
        self.variable_map.clear();
    }

    fn build_function(&mut self, func: &Function) {
        let func_id = self.program.new_function_id();
        let entry_block_id = self.program.new_block_id();
        
        let entry_block = BasicBlock::new(entry_block_id);
        
        // Convert parameters
        let mut params = Vec::new();
        for param in &func.params {
            let param_value = self.program.new_value_id();
            let ir_type = IrType::from(&param.ty);
            params.push((param.name.clone(), param_value, ir_type.clone()));
            self.variable_map.insert(param.name.clone(), param_value);
        }
        
        // Convert return type
        let return_type = func.return_type.as_ref()
            .map(|t| IrType::from(t))
            .unwrap_or(IrType::Unit);
        
        // Create function
        let ir_func = IrFunction {
            id: func_id,
            name: func.name.clone(),
            params,
            return_type,
            blocks: vec![entry_block],
            entry_block: entry_block_id,
            attributes: func.attrs.iter().map(|a| a.name.clone()).collect(),
        };
        
        self.program.functions.push(ir_func);
        self.function_map.insert(func.name.clone(), func_id);
        self.current_function = Some(func_id);
        self.current_block = Some(entry_block_id);
        
        // Build function body
        for stmt in &func.body {
            self.build_statement(stmt);
        }
        
        // Ensure function has a return if needed
        let func = &mut self.program.functions[func_id.0 as usize];
        let current_block_id = self.current_block.unwrap();
        let current_block = func.blocks.iter_mut()
            .find(|b| b.id == current_block_id)
            .expect("Current block not found");
        if current_block.terminator.is_none() {
            // Add implicit return
            if matches!(func.return_type, IrType::Unit) {
                current_block.terminator = Some(Instruction::Return { value: None });
            } else {
                // TODO: Handle missing return value error
                current_block.terminator = Some(Instruction::Return { value: None });
            }
        }
        
        self.current_function = None;
        self.current_block = None;
        self.variable_map.clear();
        self.loop_headers.clear();
        self.loop_continues.clear();
    }

    fn build_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expr(expr) => {
                self.build_expr(expr);
            }
            Statement::Let { name, init, .. } => {
                let value = self.build_expr(init);
                self.variable_map.insert(name.clone(), value);
            }
            Statement::Return(expr) => {
                let value = expr.as_ref().map(|e| self.build_expr(e));
                let func_id = self.current_function.unwrap();
                let block_id = self.current_block.unwrap();
                let func = &mut self.program.functions[func_id.0 as usize];
                let block = func.blocks.iter_mut()
                    .find(|b| b.id == block_id)
                    .expect("Block not found");
                block.terminator = Some(Instruction::Return { value });
            }
            Statement::If { cond, then, else_ } => {
                self.build_if(cond, then, else_.as_deref());
            }
            Statement::While { cond, body } => {
                self.build_while(cond, body);
            }
            Statement::For { var, iter, body } => {
                self.build_for(var, iter, body);
            }
            Statement::Break => {
                self.build_break();
            }
            Statement::Continue => {
                self.build_continue();
            }
        }
    }

    fn build_expr(&mut self, expr: &Expr) -> ValueId {
        let value_id = self.program.new_value_id();
        let instruction = match expr {
            Expr::Integer(n) => Instruction::ConstInt { value: *n },
            Expr::Float(f) => Instruction::ConstFloat { value: *f },
            Expr::Bool(b) => Instruction::ConstBool { value: *b },
            Expr::String(s) => Instruction::ConstString { value: s.clone() },
            Expr::None => Instruction::ConstUnit,
            
            Expr::Ident(name) => {
                // Look up variable in current scope
                if let Some(&var_id) = self.variable_map.get(name) {
                    // Return existing value (in SSA, we might need a phi node if reassigned)
                    return var_id;
                } else {
                    // TODO: Error - undefined variable
                    return value_id; // Placeholder
                }
            }
            
            Expr::Add(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Add { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Sub(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Sub { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Mul(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Mul { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Div(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Div { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Mod(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Mod { lhs: lhs_val, rhs: rhs_val }
            }
            
            Expr::Eq(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Eq { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Ne(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Ne { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Lt(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Lt { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Gt(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Gt { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Le(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Le { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Ge(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Ge { lhs: lhs_val, rhs: rhs_val }
            }
            
            Expr::And(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::And { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Or(lhs, rhs) => {
                let lhs_val = self.build_expr(lhs);
                let rhs_val = self.build_expr(rhs);
                Instruction::Or { lhs: lhs_val, rhs: rhs_val }
            }
            Expr::Not(operand) => {
                let op_val = self.build_expr(operand);
                Instruction::Not { operand: op_val }
            }
            Expr::Neg(operand) => {
                let op_val = self.build_expr(operand);
                Instruction::Neg { operand: op_val }
            }
            
            Expr::Call { callee, args } => {
                // Handle function calls, including method calls (obj.method())
                let arg_vals: Vec<ValueId> = args.iter().map(|a| self.build_expr(a)).collect();
                let call_result = self.program.new_value_id();
                
                // Check if this is a method call: obj.method()
                let instruction = if let Expr::Member(obj_expr, method_name) = callee.as_ref() {
                    // Method call: obj.method(args) -> ClassName__methodName(obj, args...)
                    let obj_val = self.build_expr(obj_expr);
                    
                    // Find which class this method belongs to
                    // For now, we'll try to find the method by mangled name pattern
                    // TODO: Track object types better to find the correct class
                    let mut method_found = false;
                    let mut mangled_method_name = String::new();
                    
                    // Try to find method in function_map by checking all class methods
                    for (func_name, _) in &self.function_map {
                        if func_name.ends_with(&format!("__{}", method_name)) {
                            mangled_method_name = func_name.clone();
                            method_found = true;
                            break;
                        }
                    }
                    
                    if method_found {
                        // Build args with 'self' as first argument
                        let mut method_args = vec![obj_val];
                        method_args.extend(arg_vals);
                        
                        Instruction::Call {
                            callee: self.program.new_value_id(), // Placeholder
                            args: method_args,
                            effect: MemoryEffect::None,
                            function_name: Some(mangled_method_name),
                            is_tail_call: false,
                        }
                    } else {
                        // Method not found - generate placeholder
                        Instruction::Call {
                            callee: obj_val,
                            args: arg_vals,
                            effect: MemoryEffect::None,
                            function_name: None,
                            is_tail_call: false,
                        }
                    }
                } else if let Expr::Ident(callee_name) = callee.as_ref() {
                    if is_stdlib_function(callee_name) {
                        // Use Intrinsic for stdlib functions
                        // Determine memory effect based on function name
                        let effect = match callee_name.as_str() {
                            "print" => MemoryEffect::Write, // print has side effects
                            _ => MemoryEffect::None, // Most math functions are pure
                        };
                        
                        Instruction::Intrinsic {
                            name: callee_name.clone(),
                            args: arg_vals,
                            effect,
                        }
                    } else if self.function_map.contains_key(callee_name) {
                        // User-defined function - store function name for codegen
                        // Use a placeholder ValueId (we'll resolve by name in codegen)
                        let func_ref_val = self.program.new_value_id();
                        
                        Instruction::Call {
                            callee: func_ref_val, // Placeholder, will be resolved by name in codegen
                            args: arg_vals,
                            effect: MemoryEffect::None, // User functions might have side effects, but default to None
                            function_name: Some(callee_name.clone()), // Store name for codegen
                            is_tail_call: false,
                        }
                    } else {
                        // Fallback: treat as expression that evaluates to a function
                        let callee_val = self.build_expr(callee);
                        Instruction::Call {
                            callee: callee_val,
                            args: arg_vals,
                            effect: MemoryEffect::None,
                            function_name: None, // Dynamic call
                            is_tail_call: false,
                        }
                    }
                } else {
                    // Fallback: treat as expression that evaluates to a function
                    let callee_val = self.build_expr(callee);
                    Instruction::Call {
                        callee: callee_val,
                        args: arg_vals,
                        effect: MemoryEffect::None,
                        function_name: None, // Dynamic call
                        is_tail_call: false,
                    }
                };
                
                // Add instruction to current block
                let func_id = self.current_function.unwrap();
                let block_id = self.current_block.unwrap();
                let func = &mut self.program.functions[func_id.0 as usize];
                let block = func.blocks.iter_mut()
                    .find(|b| b.id == block_id)
                    .expect("Block not found");
                block.instructions.push((call_result, instruction));
                
                return call_result;
            }
            
            Expr::Assign(lhs, rhs) => {
                // Handle assignment: x = value or obj.field = value
                let rhs_val = self.build_expr(rhs);
                if let Expr::Ident(name) = lhs.as_ref() {
                    // Simple variable assignment
                    // Update variable map with new value
                    self.variable_map.insert(name.clone(), rhs_val);
                    // Return the assigned value
                    return rhs_val;
                } else if let Expr::Member(obj_expr, field_name) = lhs.as_ref() {
                    // Field assignment: obj.field = value
                    let obj_val = self.build_expr(obj_expr);
                    let set_field_result = self.program.new_value_id();
                    
                    let func_id = self.current_function.unwrap();
                    let block_id = self.current_block.unwrap();
                    let func = &mut self.program.functions[func_id.0 as usize];
                    let block = func.blocks.iter_mut()
                        .find(|b| b.id == block_id)
                        .expect("Block not found");
                    
                    block.instructions.push((
                        set_field_result,
                        Instruction::SetField {
                            obj: obj_val,
                            field_name: field_name.clone(),
                            value: rhs_val,
                        },
                    ));
                    
                    return set_field_result;
                } else {
                    // TODO: Handle other complex lvalues (e.g., array[index] = value)
                    return rhs_val;
                }
            }
            
            Expr::AddAssign(lhs, rhs) => {
                // Handle +=: x += value -> x = x + value
                if let Expr::Ident(name) = lhs.as_ref() {
                    let old_val = self.variable_map.get(name).copied().unwrap_or_else(|| {
                        // TODO: Error - undefined variable
                        self.program.new_value_id()
                    });
                    let rhs_val = self.build_expr(rhs);
                    let new_val = self.program.new_value_id();
                    let func_id = self.current_function.unwrap();
                    let block_id = self.current_block.unwrap();
                    let func = &mut self.program.functions[func_id.0 as usize];
                    let block = func.blocks.iter_mut()
                        .find(|b| b.id == block_id)
                        .expect("Block not found");
                    block.instructions.push((
                        new_val,
                        Instruction::Add { lhs: old_val, rhs: rhs_val },
                    ));
                    self.variable_map.insert(name.clone(), new_val);
                    return new_val;
                } else {
                    // TODO: Handle complex lvalues
                    return self.build_expr(rhs);
                }
            }
            
            Expr::SubAssign(lhs, rhs) => {
                // Handle -=: x -= value -> x = x - value
                if let Expr::Ident(name) = lhs.as_ref() {
                    let old_val = self.variable_map.get(name).copied().unwrap_or_else(|| {
                        self.program.new_value_id()
                    });
                    let rhs_val = self.build_expr(rhs);
                    let new_val = self.program.new_value_id();
                    let func_id = self.current_function.unwrap();
                    let block_id = self.current_block.unwrap();
                    let func = &mut self.program.functions[func_id.0 as usize];
                    let block = func.blocks.iter_mut()
                        .find(|b| b.id == block_id)
                        .expect("Block not found");
                    block.instructions.push((
                        new_val,
                        Instruction::Sub { lhs: old_val, rhs: rhs_val },
                    ));
                    self.variable_map.insert(name.clone(), new_val);
                    return new_val;
                } else {
                    return self.build_expr(rhs);
                }
            }
            
            Expr::MulAssign(lhs, rhs) => {
                // Handle *=: x *= value -> x = x * value
                if let Expr::Ident(name) = lhs.as_ref() {
                    let old_val = self.variable_map.get(name).copied().unwrap_or_else(|| {
                        self.program.new_value_id()
                    });
                    let rhs_val = self.build_expr(rhs);
                    let new_val = self.program.new_value_id();
                    let func_id = self.current_function.unwrap();
                    let block_id = self.current_block.unwrap();
                    let func = &mut self.program.functions[func_id.0 as usize];
                    let block = func.blocks.iter_mut()
                        .find(|b| b.id == block_id)
                        .expect("Block not found");
                    block.instructions.push((
                        new_val,
                        Instruction::Mul { lhs: old_val, rhs: rhs_val },
                    ));
                    self.variable_map.insert(name.clone(), new_val);
                    return new_val;
                } else {
                    return self.build_expr(rhs);
                }
            }
            
            Expr::DivAssign(lhs, rhs) => {
                // Handle /=: x /= value -> x = x / value
                if let Expr::Ident(name) = lhs.as_ref() {
                    let old_val = self.variable_map.get(name).copied().unwrap_or_else(|| {
                        self.program.new_value_id()
                    });
                    let rhs_val = self.build_expr(rhs);
                    let new_val = self.program.new_value_id();
                    let func_id = self.current_function.unwrap();
                    let block_id = self.current_block.unwrap();
                    let func = &mut self.program.functions[func_id.0 as usize];
                    let block = func.blocks.iter_mut()
                        .find(|b| b.id == block_id)
                        .expect("Block not found");
                    block.instructions.push((
                        new_val,
                        Instruction::Div { lhs: old_val, rhs: rhs_val },
                    ));
                    self.variable_map.insert(name.clone(), new_val);
                    return new_val;
                } else {
                    return self.build_expr(rhs);
                }
            }
            
            Expr::New { class_name, args } => {
                // Build arguments
                let field_vals: Vec<ValueId> = args.iter().map(|a| self.build_expr(a)).collect();
                
                // Allocate struct
                Instruction::AllocStruct {
                    struct_name: class_name.clone(),
                    fields: field_vals,
                }
            }
            Expr::Member(obj, field_name) => {
                // Build object expression
                let obj_val = self.build_expr(obj);
                
                // Get field
                Instruction::GetField {
                    obj: obj_val,
                    field_name: field_name.clone(),
                }
            }
            Expr::Index(container, index) => {
                // TODO: Implement array indexing
                let _container_val = self.build_expr(container);
                let _index_val = self.build_expr(index);
                Instruction::ConstUnit
            }
            Expr::FString(_) => {
                // TODO: Implement f-string formatting
                Instruction::ConstString { value: String::new() }
            }
            Expr::IsInstance(_expr, _ty) => {
                // TODO: Implement isinstance check
                Instruction::ConstBool { value: false }
            }
            _ => {
                // TODO: Implement remaining expression types
                Instruction::ConstUnit
            }
        };
        
        // Add instruction to current block
        let func_id = self.current_function.unwrap();
        let block_id = self.current_block.unwrap();
        let func = &mut self.program.functions[func_id.0 as usize];
        // Find block by ID (blocks may not be in order)
        let block = func.blocks.iter_mut()
            .find(|b| b.id == block_id)
            .expect("Block not found");
        block.instructions.push((value_id, instruction));
        value_id
    }

    fn build_if(&mut self, cond: &Expr, then: &[Statement], else_: Option<&[Statement]>) {
        let cond_val = self.build_expr(cond);
        
        let func_id = self.current_function.unwrap();
        let current_block_id = self.current_block.unwrap();
        
        let then_block_id = self.program.new_block_id();
        let else_block_id = self.program.new_block_id();
        let merge_block_id = self.program.new_block_id();
        
        // Create blocks
        let then_block = BasicBlock::new(then_block_id);
        let else_block = BasicBlock::new(else_block_id);
        let merge_block = BasicBlock::new(merge_block_id);
        
        // Add branch to current block
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            let current_block = &mut func.blocks[current_block_id.0 as usize];
            current_block.terminator = Some(Instruction::Branch {
                cond: cond_val,
                then_block: then_block_id,
                else_block: else_block_id,
            });
            current_block.successors.push(then_block_id);
            current_block.successors.push(else_block_id);
            
            func.blocks.push(then_block);
            func.blocks.push(else_block);
            func.blocks.push(merge_block);
        }
        
        // Build then block
        self.current_block = Some(then_block_id);
        for stmt in then {
            self.build_statement(stmt);
        }
        // Jump to merge block
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            let then_block = &mut func.blocks[then_block_id.0 as usize];
            if then_block.terminator.is_none() {
                then_block.terminator = Some(Instruction::Jump { target: merge_block_id });
                then_block.successors.push(merge_block_id);
            }
            let merge_block = &mut func.blocks[merge_block_id.0 as usize];
            merge_block.predecessors.push(then_block_id);
        }
        
        // Build else block
        if let Some(else_body) = else_ {
            self.current_block = Some(else_block_id);
            for stmt in else_body {
                self.build_statement(stmt);
            }
            {
                let func = &mut self.program.functions[func_id.0 as usize];
                let else_block = &mut func.blocks[else_block_id.0 as usize];
                if else_block.terminator.is_none() {
                    else_block.terminator = Some(Instruction::Jump { target: merge_block_id });
                    else_block.successors.push(merge_block_id);
                }
                let merge_block = &mut func.blocks[merge_block_id.0 as usize];
                merge_block.predecessors.push(else_block_id);
            }
        } else {
            // No else block - jump directly to merge
            {
                let func = &mut self.program.functions[func_id.0 as usize];
                let else_block = &mut func.blocks[else_block_id.0 as usize];
                else_block.terminator = Some(Instruction::Jump { target: merge_block_id });
                else_block.successors.push(merge_block_id);
                let merge_block = &mut func.blocks[merge_block_id.0 as usize];
                merge_block.predecessors.push(else_block_id);
            }
        }
        
        // Continue in merge block
        self.current_block = Some(merge_block_id);
    }

    fn build_while(&mut self, cond: &Expr, body: &[Statement]) {
        let func_id = self.current_function.unwrap();
        let current_block_id = self.current_block.unwrap();
        
        // Create blocks: header (condition check), body, continue (after loop)
        let header_block_id = self.program.new_block_id();
        let body_block_id = self.program.new_block_id();
        let continue_block_id = self.program.new_block_id();
        
        let header_block = BasicBlock::new(header_block_id);
        let body_block = BasicBlock::new(body_block_id);
        let continue_block = BasicBlock::new(continue_block_id);
        
        // Add blocks to function
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            func.blocks.push(header_block);
            func.blocks.push(body_block);
            func.blocks.push(continue_block);
        }
        
        // Jump from current block to header
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            let current_block = &mut func.blocks[current_block_id.0 as usize];
            if current_block.terminator.is_none() {
                current_block.terminator = Some(Instruction::Jump { target: header_block_id });
                current_block.successors.push(header_block_id);
            }
            let header_block = &mut func.blocks[header_block_id.0 as usize];
            header_block.predecessors.push(current_block_id);
        }
        
        // Push loop context for break/continue
        self.loop_headers.push(header_block_id);
        self.loop_continues.push(continue_block_id);
        
        // Build header block: evaluate condition and branch
        self.current_block = Some(header_block_id);
        let cond_val = self.build_expr(cond);
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            let header_block = &mut func.blocks[header_block_id.0 as usize];
            header_block.terminator = Some(Instruction::Branch {
                cond: cond_val,
                then_block: body_block_id,
                else_block: continue_block_id,
            });
            header_block.successors.push(body_block_id);
            header_block.successors.push(continue_block_id);
            
            let body_block = &mut func.blocks[body_block_id.0 as usize];
            body_block.predecessors.push(header_block_id);
            let continue_block = &mut func.blocks[continue_block_id.0 as usize];
            continue_block.predecessors.push(header_block_id);
        }
        
        // Build body block
        self.current_block = Some(body_block_id);
        for stmt in body {
            self.build_statement(stmt);
        }
        // Jump back to header (unless there's already a terminator)
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            let body_block = &mut func.blocks[body_block_id.0 as usize];
            if body_block.terminator.is_none() {
                body_block.terminator = Some(Instruction::Jump { target: header_block_id });
                body_block.successors.push(header_block_id);
                let header_block = &mut func.blocks[header_block_id.0 as usize];
                header_block.predecessors.push(body_block_id);
            }
        }
        
        // Pop loop context
        self.loop_headers.pop();
        self.loop_continues.pop();
        
        // Continue in continue block
        self.current_block = Some(continue_block_id);
    }

    fn build_for(&mut self, var: &str, iter: &Expr, body: &[Statement]) {
        // For now, implement simple for loop: for x in range(start, end)
        // TODO: Support other iterables
        let func_id = self.current_function.unwrap();
        let current_block_id = self.current_block.unwrap();
        
        // Create blocks: init, header (condition), body, increment, continue
        let init_block_id = self.program.new_block_id();
        let header_block_id = self.program.new_block_id();
        let body_block_id = self.program.new_block_id();
        let increment_block_id = self.program.new_block_id();
        let continue_block_id = self.program.new_block_id();
        
        let init_block = BasicBlock::new(init_block_id);
        let header_block = BasicBlock::new(header_block_id);
        let body_block = BasicBlock::new(body_block_id);
        let increment_block = BasicBlock::new(increment_block_id);
        let continue_block = BasicBlock::new(continue_block_id);
        
        // Add blocks to function
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            func.blocks.push(init_block);
            func.blocks.push(header_block);
            func.blocks.push(body_block);
            func.blocks.push(increment_block);
            func.blocks.push(continue_block);
        }
        
        // Jump from current block to init
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            let current_block = &mut func.blocks[current_block_id.0 as usize];
            if current_block.terminator.is_none() {
                current_block.terminator = Some(Instruction::Jump { target: init_block_id });
                current_block.successors.push(init_block_id);
            }
            let init_block = &mut func.blocks[init_block_id.0 as usize];
            init_block.predecessors.push(current_block_id);
        }
        
        // Push loop context
        self.loop_headers.push(header_block_id);
        self.loop_continues.push(continue_block_id);
        
        // Build init block: initialize loop variable
        self.current_block = Some(init_block_id);
        // For now, assume iter is a range or list - initialize var to first element
        // TODO: Proper iterator support
        let iter_val = self.build_expr(iter);
        // Store initial value in variable map
        self.variable_map.insert(var.to_string(), iter_val);
        // Jump to header
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            let init_block = &mut func.blocks[init_block_id.0 as usize];
            init_block.terminator = Some(Instruction::Jump { target: header_block_id });
            init_block.successors.push(header_block_id);
            let header_block = &mut func.blocks[header_block_id.0 as usize];
            header_block.predecessors.push(init_block_id);
        }
        
        // Build header block: check condition (simplified - always true for now)
        // TODO: Proper condition checking for iterables
        self.current_block = Some(header_block_id);
        // For now, create a dummy condition (always true)
        // In real implementation, check if iterator has more elements
        let true_val = self.program.new_value_id();
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            let header_block = &mut func.blocks[header_block_id.0 as usize];
            // Create a constant true condition (placeholder)
            header_block.instructions.push((
                true_val,
                Instruction::ConstBool { value: true },
            ));
            header_block.terminator = Some(Instruction::Branch {
                cond: true_val,
                then_block: body_block_id,
                else_block: continue_block_id,
            });
            header_block.successors.push(body_block_id);
            header_block.successors.push(continue_block_id);
            
            let body_block = &mut func.blocks[body_block_id.0 as usize];
            body_block.predecessors.push(header_block_id);
            let continue_block = &mut func.blocks[continue_block_id.0 as usize];
            continue_block.predecessors.push(header_block_id);
        }
        
        // Build body block
        self.current_block = Some(body_block_id);
        for stmt in body {
            self.build_statement(stmt);
        }
        // Jump to increment block
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            let body_block = &mut func.blocks[body_block_id.0 as usize];
            if body_block.terminator.is_none() {
                body_block.terminator = Some(Instruction::Jump { target: increment_block_id });
                body_block.successors.push(increment_block_id);
                let increment_block = &mut func.blocks[increment_block_id.0 as usize];
                increment_block.predecessors.push(body_block_id);
            }
        }
        
        // Build increment block: increment loop variable and jump back to header
        self.current_block = Some(increment_block_id);
        // TODO: Proper increment based on iterator type
        // For now, just jump back to header
        {
            let func = &mut self.program.functions[func_id.0 as usize];
            let increment_block = &mut func.blocks[increment_block_id.0 as usize];
            increment_block.terminator = Some(Instruction::Jump { target: header_block_id });
            increment_block.successors.push(header_block_id);
            let header_block = &mut func.blocks[header_block_id.0 as usize];
            header_block.predecessors.push(increment_block_id);
        }
        
        // Pop loop context
        self.loop_headers.pop();
        self.loop_continues.pop();
        
        // Continue in continue block
        self.current_block = Some(continue_block_id);
    }
    
    fn build_break(&mut self) {
        if let Some(continue_block) = self.loop_continues.last() {
            let func_id = self.current_function.unwrap();
            let current_block_id = self.current_block.unwrap();
            
            let func = &mut self.program.functions[func_id.0 as usize];
            let current_block = &mut func.blocks[current_block_id.0 as usize];
            if current_block.terminator.is_none() {
                current_block.terminator = Some(Instruction::Jump { target: *continue_block });
                current_block.successors.push(*continue_block);
                let continue_block = &mut func.blocks[continue_block.0 as usize];
                continue_block.predecessors.push(current_block_id);
            }
        }
    }
    
    fn build_continue(&mut self) {
        if let Some(header_block) = self.loop_headers.last() {
            let func_id = self.current_function.unwrap();
            let current_block_id = self.current_block.unwrap();
            
            let func = &mut self.program.functions[func_id.0 as usize];
            let current_block = &mut func.blocks[current_block_id.0 as usize];
            if current_block.terminator.is_none() {
                current_block.terminator = Some(Instruction::Jump { target: *header_block });
                current_block.successors.push(*header_block);
                let header_block = &mut func.blocks[header_block.0 as usize];
                header_block.predecessors.push(current_block_id);
            }
        }
    }
}

impl Default for IrBuilder {
    fn default() -> Self {
        Self::new()
    }
}

