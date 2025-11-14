// IR Builder - converts AST to SSA-based IR

use crate::ast::*;
use crate::ir::*;
use std::collections::HashMap;

pub struct IrBuilder {
    program: IrProgram,
    current_function: Option<FunctionId>,
    current_block: Option<BlockId>,
    variable_map: HashMap<String, ValueId>, // Maps variable names to SSA values
    #[allow(dead_code)]
    block_stack: Vec<BlockId>, // For tracking control flow (reserved for future use)
}

impl IrBuilder {
    pub fn new() -> Self {
        Self {
            program: IrProgram::new(),
            current_function: None,
            current_block: None,
            variable_map: HashMap::new(),
            block_stack: Vec::new(),
        }
    }

    /// Build IR from AST program
    pub fn build(mut self, ast: &Program) -> IrProgram {
        for item in &ast.items {
            match item {
                Item::Function(func) => {
                    self.build_function(func);
                }
            }
        }
        self.program
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
            Statement::Break | Statement::Continue => {
                // TODO: Implement break/continue
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
                // TODO: Handle function calls properly
                let callee_val = self.build_expr(callee);
                let arg_vals: Vec<ValueId> = args.iter().map(|a| self.build_expr(a)).collect();
                Instruction::Call {
                    callee: callee_val,
                    args: arg_vals,
                    effect: MemoryEffect::None, // TODO: Determine memory effects
                }
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

    fn build_while(&mut self, _cond: &Expr, _body: &[Statement]) {
        // TODO: Implement while loop
        // This requires loop header, body, and back edge
    }

    fn build_for(&mut self, _var: &str, _iter: &Expr, _body: &[Statement]) {
        // TODO: Implement for loop
        // This requires iterator setup, loop header, body, and increment
    }
}

impl Default for IrBuilder {
    fn default() -> Self {
        Self::new()
    }
}

