// Pain IR - SSA-based Intermediate Representation

use crate::ast::Type;

/// Unique identifier for an IR value (SSA variable)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

/// Unique identifier for a basic block
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

/// Unique identifier for a function
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub u32);

/// IR type system - richer than AST types, includes tensor dimensions
#[derive(Debug, Clone, PartialEq)]
pub enum IrType {
    Int,
    Int32,
    Int64,
    Float32,
    Float64,
    Bool,
    Str,
    Unit, // Void type
    
    // Composite types
    Pointer(Box<IrType>),
    Array {
        element: Box<IrType>,
        size: Option<u64>, // None for dynamic arrays
    },
    Tensor {
        element: Box<IrType>,
        dims: Vec<Option<u64>>, // None for dynamic dimensions
    },
    
    // Function type
    Function {
        params: Vec<IrType>,
        return_type: Box<IrType>,
    },
    
    // Named type (will be resolved later)
    Named(String),
}

/// Memory effect annotations for operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryEffect {
    None,
    Read,
    Write,
    Alloc,
    Free,
    ReadWrite,
}

/// IR instruction - SSA form
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Constants
    ConstInt { value: i64 },
    ConstFloat { value: f64 },
    ConstBool { value: bool },
    ConstString { value: String },
    ConstUnit,
    
    // Binary operations
    Add { lhs: ValueId, rhs: ValueId },
    Sub { lhs: ValueId, rhs: ValueId },
    Mul { lhs: ValueId, rhs: ValueId },
    Div { lhs: ValueId, rhs: ValueId },
    Mod { lhs: ValueId, rhs: ValueId },
    
    // Comparisons
    Eq { lhs: ValueId, rhs: ValueId },
    Ne { lhs: ValueId, rhs: ValueId },
    Lt { lhs: ValueId, rhs: ValueId },
    Gt { lhs: ValueId, rhs: ValueId },
    Le { lhs: ValueId, rhs: ValueId },
    Ge { lhs: ValueId, rhs: ValueId },
    
    // Logical operations
    And { lhs: ValueId, rhs: ValueId },
    Or { lhs: ValueId, rhs: ValueId },
    Not { operand: ValueId },
    
    // Unary operations
    Neg { operand: ValueId },
    
    // Memory operations
    Load { ptr: ValueId, effect: MemoryEffect },
    Store { ptr: ValueId, value: ValueId, effect: MemoryEffect },
    Alloc { size: ValueId, align: u64, effect: MemoryEffect },
    Free { ptr: ValueId, effect: MemoryEffect },
    
    // Function calls
    Call {
        callee: ValueId,
        args: Vec<ValueId>,
        effect: MemoryEffect,
    },
    
    // Control flow
    Branch {
        cond: ValueId,
        then_block: BlockId,
        else_block: BlockId,
    },
    Jump { target: BlockId },
    Return { value: Option<ValueId> },
    
    // Phi node (SSA form for merging values from different blocks)
    Phi {
        incoming: Vec<(BlockId, ValueId)>,
    },
    
    // Intrinsics (for SIMD, atomics, etc.)
    Intrinsic {
        name: String,
        args: Vec<ValueId>,
        effect: MemoryEffect,
    },
    
    // Tensor operations (placeholder for future)
    TensorOp {
        op: TensorOp,
        args: Vec<ValueId>,
    },
}

/// Tensor operations
#[derive(Debug, Clone, PartialEq)]
pub enum TensorOp {
    Add,
    Mul,
    MatMul,
    Relu,
    Transpose,
    Reshape,
    // More to be added
}

/// Basic block - sequence of instructions ending with terminator
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<(ValueId, Instruction)>, // (result, instruction)
    pub terminator: Option<Instruction>, // Branch, Jump, or Return
    pub predecessors: Vec<BlockId>,
    pub successors: Vec<BlockId>,
}

impl BasicBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            instructions: Vec::new(),
            terminator: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }
    }
}

/// Function in IR form
#[derive(Debug, Clone)]
pub struct IrFunction {
    pub id: FunctionId,
    pub name: String,
    pub params: Vec<(String, ValueId, IrType)>, // (name, value_id, type)
    pub return_type: IrType,
    pub blocks: Vec<BasicBlock>,
    pub entry_block: BlockId,
    pub attributes: Vec<String>, // e.g., "@kernel", "@inline"
}

/// Complete IR program
#[derive(Debug, Clone)]
pub struct IrProgram {
    pub functions: Vec<IrFunction>,
    pub next_value_id: u32,
    pub next_block_id: u32,
    pub next_function_id: u32,
}

impl IrProgram {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            next_value_id: 0,
            next_block_id: 0,
            next_function_id: 0,
        }
    }
    
    pub fn new_value_id(&mut self) -> ValueId {
        let id = ValueId(self.next_value_id);
        self.next_value_id += 1;
        id
    }
    
    pub fn new_block_id(&mut self) -> BlockId {
        let id = BlockId(self.next_block_id);
        self.next_block_id += 1;
        id
    }
    
    pub fn new_function_id(&mut self) -> FunctionId {
        let id = FunctionId(self.next_function_id);
        self.next_function_id += 1;
        id
    }
}

impl Default for IrProgram {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert AST Type to IR Type
impl From<&Type> for IrType {
    fn from(ast_type: &Type) -> Self {
        match ast_type {
            Type::Int => IrType::Int64,
            Type::Float32 => IrType::Float32,
            Type::Float64 => IrType::Float64,
            Type::Bool => IrType::Bool,
            Type::Str => IrType::Str,
            Type::Dynamic => IrType::Named("dynamic".to_string()),
            Type::List(element) => IrType::Array {
                element: Box::new(IrType::from(element.as_ref())),
                size: None,
            },
            Type::Array(element) => IrType::Array {
                element: Box::new(IrType::from(element.as_ref())),
                size: None, // TODO: extract size from AST if available
            },
            Type::Map(_key, _value) => {
                // Map is represented as a pointer to a hash table structure
                IrType::Pointer(Box::new(IrType::Named("Map".to_string())))
            }
            Type::Tensor(element, dims) => {
                // Convert dimension expressions to optional u64
                let ir_dims: Vec<Option<u64>> = dims.iter()
                    .map(|_| None) // TODO: evaluate constant dimensions
                    .collect();
                IrType::Tensor {
                    element: Box::new(IrType::from(element.as_ref())),
                    dims: ir_dims,
                }
            }
            Type::Named(name) => IrType::Named(name.clone()),
        }
    }
}

