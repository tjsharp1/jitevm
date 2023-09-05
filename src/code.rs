use primitive_types::U256;
use std::collections::{HashMap, HashSet};
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EvmOp {
    Stop,
    Add,
    Mul,
    Sub,
    Div,
    Sdiv,
    Mod,
    Smod,
    Addmod,
    Mulmod,
    Exp,
    Signextend,
    Lt,
    Gt,
    Slt,
    Sgt,
    Eq,
    Iszero,
    And,
    Or,
    Xor,
    Not, // 0x19 = 25
    Byte,
    Shl,
    Shr,
    Sar,
    Sha3, // 0x20 = 32
    Address,
    Balance,
    Origin,
    Caller,
    Callvalue,
    Calldataload,
    Calldatasize,
    CalldataCopy,
    Codesize,
    CodeCopy,
    GasPrice,
    ExtCodeSize,
    ExtCodeCopy,
    ReturnDataSize,
    ReturnDataCopy,
    ExtCodeHash,
    BlockHash,
    Coinbase,
    Timestamp,
    Number,
    PrevRandao,
    GasLimit,
    ChainId,
    SelfBalance,
    BaseFee,
    Pop,
    Mload,
    Mstore,
    Mstore8,
    Sload,
    Sstore,
    Jump,
    Jumpi,
    Pc,
    Msize,
    Gas,
    Jumpdest,
    Push(usize, U256),
    Dup1,
    Dup2,
    Dup3,
    Dup4,
    Dup5,
    Dup6,
    Dup7,
    Dup8,
    Dup9,
    Dup10,
    Dup11,
    Dup12,
    Dup13,
    Dup14,
    Dup15,
    Dup16,
    Swap1,
    Swap2,
    Swap3,
    Swap4,
    Swap5,
    Swap6,
    Swap7,
    Swap8,
    Swap9,
    Swap10,
    Swap11,
    Swap12,
    Swap13,
    Swap14,
    Swap15,
    Swap16,
    Log0,
    Log1,
    Log2,
    Log3,
    Log4,
    Create,
    Call,
    CallCode,
    Return,
    DelegateCall,
    Create2,
    StaticCall,
    Revert,
    Invalid,
    Selfdestruct,

    AugmentedPushJump(usize, U256),
    AugmentedPushJumpi(usize, U256),

    Unknown(u8),
}

#[derive(Error, Debug)]
pub enum EvmOpError {
    #[error("parser error: incomplete instruction")]
    ParserErrorIncompleteInstruction,
    #[error("parser error: unknown instruction")]
    ParserErrorUnknownInstruction(u8),
}

#[derive(Debug, Clone, Copy)]
pub enum EvmOpParserMode {
    Lax,
    Strict,
}

impl EvmOp {
    pub fn len(&self) -> usize {
        use EvmOp::*;

        match self {
            Push(len, _) => 1 + len,
            AugmentedPushJump(len, _) => 1 + len + 1,
            AugmentedPushJumpi(len, _) => 1 + len + 1,
            Unknown(_) => 1,
            _ => 1,
        }
    }

    pub fn opcode(&self) -> u16 {
        use EvmOp::*;

        match self {
            Stop => 0x00,
            Add => 0x01,
            Mul => 0x02,
            Sub => 0x03,
            Div => 0x04,
            Sdiv => 0x05,
            Mod => 0x06,
            Smod => 0x07,
            Addmod => 0x08,
            Mulmod => 0x09,
            Exp => 0x0a,
            Signextend => 0x0b,
            Lt => 0x10,
            Gt => 0x11,
            Slt => 0x12,
            Sgt => 0x13,
            Eq => 0x14,
            Iszero => 0x15,
            And => 0x16,
            Or => 0x17,
            Xor => 0x18,
            Not => 0x19,
            Byte => 0x1a,
            Shl => 0x1b,
            Shr => 0x1c,
            Sar => 0x1d,
            Sha3 => 0x20,
            Address => 0x30,
            Balance => 0x31,
            Origin => 0x32,
            Caller => 0x33,
            Callvalue => 0x34,
            Calldataload => 0x35,
            Calldatasize => 0x36,
            CalldataCopy => 0x37,
            Codesize => 0x38,
            CodeCopy => 0x39,
            GasPrice => 0x3a,
            ExtCodeSize => 0x3b,
            ExtCodeCopy => 0x3c,
            ReturnDataSize => 0x3d,
            ReturnDataCopy => 0x3e,
            ExtCodeHash => 0x3f,
            BlockHash => 0x40,
            Coinbase => 0x41,
            Timestamp => 0x42,
            Number => 0x43,
            PrevRandao => 0x44,
            GasLimit => 0x45,
            ChainId => 0x46,
            SelfBalance => 0x47,
            BaseFee => 0x48,
            Pop => 0x50,
            Mload => 0x51,
            Mstore => 0x52,
            Mstore8 => 0x53,
            Sload => 0x54,
            Sstore => 0x55,
            Jump => 0x56,
            Jumpi => 0x57,
            Pc => 0x58,
            Msize => 0x59,
            Gas => 0x5a,
            Jumpdest => 0x5b,
            Push(len, _) => {
                assert!(*len <= 32);
                0x5f + *len as u16
            }
            Dup1 => 0x80,
            Dup2 => 0x81,
            Dup3 => 0x82,
            Dup4 => 0x83,
            Dup5 => 0x84,
            Dup6 => 0x85,
            Dup7 => 0x86,
            Dup8 => 0x87,
            Dup9 => 0x88,
            Dup10 => 0x89,
            Dup11 => 0x8a,
            Dup12 => 0x8b,
            Dup13 => 0x8c,
            Dup14 => 0x8d,
            Dup15 => 0x8e,
            Dup16 => 0x8f,
            Swap1 => 0x90,
            Swap2 => 0x91,
            Swap3 => 0x92,
            Swap4 => 0x93,
            Swap5 => 0x94,
            Swap6 => 0x95,
            Swap7 => 0x96,
            Swap8 => 0x97,
            Swap9 => 0x98,
            Swap10 => 0x99,
            Swap11 => 0x9a,
            Swap12 => 0x9b,
            Swap13 => 0x9c,
            Swap14 => 0x9d,
            Swap15 => 0x9e,
            Swap16 => 0x9f,
            Log0 => 0xa0,
            Log1 => 0xa1,
            Log2 => 0xa2,
            Log3 => 0xa3,
            Log4 => 0xa4,
            Create => 0xf0,
            Call => 0xf1,
            CallCode => 0xf2,
            Return => 0xf3,
            DelegateCall => 0xf4,
            Create2 => 0xf5,
            StaticCall => 0xfa,
            Revert => 0xfd,
            Invalid => 0xfe,
            Selfdestruct => 0xff,
            AugmentedPushJump(len, val) => {
                let code = Push(*len, *val).opcode();
                (code << 8) | Jump.opcode()
            }
            AugmentedPushJumpi(len, val) => {
                let code = Push(*len, *val).opcode();
                (code << 8) | Jumpi.opcode()
            }

            Unknown(opcode) => *opcode as u16,
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        use EvmOp::*;

        match self {
            Stop => vec![0x00],
            Add => vec![0x01],
            Mul => vec![0x02],
            Sub => vec![0x03],
            Div => vec![0x04],
            Sdiv => vec![0x05],
            Mod => vec![0x06],
            Smod => vec![0x07],
            Addmod => vec![0x08],
            Mulmod => vec![0x09],
            Exp => vec![0x0a],
            Signextend => vec![0x0b],
            Lt => vec![0x10],
            Gt => vec![0x11],
            Slt => vec![0x12],
            Sgt => vec![0x13],
            Eq => vec![0x14],
            Iszero => vec![0x15],
            And => vec![0x16],
            Or => vec![0x17],
            Xor => vec![0x18],
            Not => vec![0x19],
            Byte => vec![0x1a],
            Shl => vec![0x1b],
            Shr => vec![0x1c],
            Sar => vec![0x1d],
            Sha3 => vec![0x20],
            Address => vec![0x30],
            Balance => vec![0x31],
            Origin => vec![0x32],
            Caller => vec![0x33],
            Callvalue => vec![0x34],
            Calldataload => vec![0x35],
            Calldatasize => vec![0x36],
            CalldataCopy => vec![0x37],
            Codesize => vec![0x38],
            CodeCopy => vec![0x39],
            GasPrice => vec![0x3a],
            ExtCodeSize => vec![0x3b],
            ExtCodeCopy => vec![0x3c],
            ReturnDataSize => vec![0x3d],
            ReturnDataCopy => vec![0x3e],
            ExtCodeHash => vec![0x3f],
            BlockHash => vec![0x40],
            Coinbase => vec![0x41],
            Timestamp => vec![0x42],
            Number => vec![0x43],
            PrevRandao => vec![0x44],
            GasLimit => vec![0x45],
            ChainId => vec![0x46],
            SelfBalance => vec![0x47],
            BaseFee => vec![0x48],
            Pop => vec![0x50],
            Mload => vec![0x51],
            Mstore => vec![0x52],
            Mstore8 => vec![0x53],
            Sload => vec![0x54],
            Sstore => vec![0x55],
            Jump => vec![0x56],
            Jumpi => vec![0x57],
            Pc => vec![0x58],
            Msize => vec![0x59],
            Gas => vec![0x5a],
            Jumpdest => vec![0x5b],
            Push(len, val) => {
                assert!(*len <= 32);

                if *len == 0 {
                    vec![0x5f]
                } else {
                    let mut v = [0u8; 32];
                    val.to_big_endian(&mut v);
                    let mut w = vec![0x60 + (len - 1) as u8];
                    w.append(&mut v[32 - len..32].to_vec());
                    w
                }
            }
            Dup1 => vec![0x80],
            Dup2 => vec![0x81],
            Dup3 => vec![0x82],
            Dup4 => vec![0x83],
            Dup5 => vec![0x84],
            Dup6 => vec![0x85],
            Dup7 => vec![0x86],
            Dup8 => vec![0x87],
            Dup9 => vec![0x88],
            Dup10 => vec![0x89],
            Dup11 => vec![0x8a],
            Dup12 => vec![0x8b],
            Dup13 => vec![0x8c],
            Dup14 => vec![0x8d],
            Dup15 => vec![0x8e],
            Dup16 => vec![0x8f],
            Swap1 => vec![0x90],
            Swap2 => vec![0x91],
            Swap3 => vec![0x92],
            Swap4 => vec![0x93],
            Swap5 => vec![0x94],
            Swap6 => vec![0x95],
            Swap7 => vec![0x96],
            Swap8 => vec![0x97],
            Swap9 => vec![0x98],
            Swap10 => vec![0x99],
            Swap11 => vec![0x9a],
            Swap12 => vec![0x9b],
            Swap13 => vec![0x9c],
            Swap14 => vec![0x9d],
            Swap15 => vec![0x9e],
            Swap16 => vec![0x9f],
            Log0 => vec![0xa0],
            Log1 => vec![0xa1],
            Log2 => vec![0xa2],
            Log3 => vec![0xa3],
            Log4 => vec![0xa4],
            Create => vec![0xf0],
            Call => vec![0xf1],
            CallCode => vec![0xf2],
            Return => vec![0xf3],
            DelegateCall => vec![0xf4],
            Create2 => vec![0xf5],
            StaticCall => vec![0xfa],
            Revert => vec![0xfd],
            Invalid => vec![0xfe],
            Selfdestruct => vec![0xff],

            AugmentedPushJump(len, val) => Push(*len, *val)
                .to_bytes()
                .into_iter()
                .chain(Jump.to_bytes().into_iter())
                .collect(),
            AugmentedPushJumpi(len, val) => Push(*len, *val)
                .to_bytes()
                .into_iter()
                .chain(Jumpi.to_bytes().into_iter())
                .collect(),

            Unknown(opcode) => vec![*opcode],
        }
    }

    pub fn new_from_bytes(b: &[u8], mode: EvmOpParserMode) -> Result<(Self, usize), EvmOpError> {
        use EvmOp::*;

        if b.len() == 0 {
            return Err(EvmOpError::ParserErrorIncompleteInstruction);
        }

        let opcode = b[0];
        if 0x60u8 <= opcode && opcode <= 0x7Fu8 {
            // PUSH (read operand from code)
            let len = (opcode - 0x60 + 1) as usize;

            if 1 + len > b.len() {
                return Err(EvmOpError::ParserErrorIncompleteInstruction);
            } else {
                let val = U256::from_big_endian(&b[1..1 + len]);
                return Ok((Push(len, val), 1 + len));
            }
        } else {
            // other opcodes
            match opcode {
                0x00 => Ok((Stop, 1)),
                0x01 => Ok((Add, 1)),
                0x02 => Ok((Mul, 1)),
                0x03 => Ok((Sub, 1)),
                0x04 => Ok((Div, 1)),
                0x05 => Ok((Sdiv, 1)),
                0x06 => Ok((Mod, 1)),
                0x07 => Ok((Smod, 1)),
                0x08 => Ok((Addmod, 1)),
                0x09 => Ok((Mulmod, 1)),
                0x0a => Ok((Exp, 1)),
                0x0b => Ok((Signextend, 1)),
                0x10 => Ok((Lt, 1)),
                0x11 => Ok((Gt, 1)),
                0x12 => Ok((Slt, 1)),
                0x13 => Ok((Sgt, 1)),
                0x14 => Ok((Eq, 1)),
                0x15 => Ok((Iszero, 1)),
                0x16 => Ok((And, 1)),
                0x17 => Ok((Or, 1)),
                0x18 => Ok((Xor, 1)),
                0x19 => Ok((Not, 1)),
                0x1b => Ok((Shl, 1)),
                0x1c => Ok((Shr, 1)),
                0x1d => Ok((Sar, 1)),
                0x20 => Ok((Sha3, 1)),
                0x30 => Ok((Address, 1)),
                0x31 => Ok((Balance, 1)),
                0x32 => Ok((Origin, 1)),
                0x33 => Ok((Caller, 1)),
                0x34 => Ok((Callvalue, 1)),
                0x35 => Ok((Calldataload, 1)),
                0x36 => Ok((Calldatasize, 1)),
                0x37 => Ok((CalldataCopy, 1)),
                0x38 => Ok((Codesize, 1)),
                0x39 => Ok((CodeCopy, 1)),
                0x3a => Ok((GasPrice, 1)),
                0x3b => Ok((ExtCodeSize, 1)),
                0x3c => Ok((ExtCodeCopy, 1)),
                0x3d => Ok((ReturnDataSize, 1)),
                0x3e => Ok((ReturnDataCopy, 1)),
                0x3f => Ok((ExtCodeHash, 1)),
                0x40 => Ok((BlockHash, 1)),
                0x41 => Ok((Coinbase, 1)),
                0x42 => Ok((Timestamp, 1)),
                0x43 => Ok((Number, 1)),
                0x44 => Ok((PrevRandao, 1)),
                0x45 => Ok((GasLimit, 1)),
                0x46 => Ok((ChainId, 1)),
                0x47 => Ok((SelfBalance, 1)),
                0x48 => Ok((BaseFee, 1)),
                0x50 => Ok((Pop, 1)),
                0x51 => Ok((Mload, 1)),
                0x52 => Ok((Mstore, 1)),
                0x53 => Ok((Mstore8, 1)),
                0x54 => Ok((Sload, 1)),
                0x55 => Ok((Sstore, 1)),
                0x56 => Ok((Jump, 1)),
                0x57 => Ok((Jumpi, 1)),
                0x58 => Ok((Pc, 1)),
                0x59 => Ok((Msize, 1)),
                0x5a => Ok((Gas, 1)),
                0x5b => Ok((Jumpdest, 1)),
                0x5f => Ok((Push(0, U256::zero()), 1)),

                0x80 => Ok((Dup1, 1)),
                0x81 => Ok((Dup2, 1)),
                0x82 => Ok((Dup3, 1)),
                0x83 => Ok((Dup4, 1)),
                0x84 => Ok((Dup5, 1)),
                0x85 => Ok((Dup6, 1)),
                0x86 => Ok((Dup7, 1)),
                0x87 => Ok((Dup8, 1)),
                0x88 => Ok((Dup9, 1)),
                0x89 => Ok((Dup10, 1)),
                0x8a => Ok((Dup11, 1)),
                0x8b => Ok((Dup12, 1)),
                0x8c => Ok((Dup13, 1)),
                0x8d => Ok((Dup14, 1)),
                0x8e => Ok((Dup15, 1)),
                0x8f => Ok((Dup16, 1)),

                0x90 => Ok((Swap1, 1)),
                0x91 => Ok((Swap2, 1)),
                0x92 => Ok((Swap3, 1)),
                0x93 => Ok((Swap4, 1)),
                0x94 => Ok((Swap5, 1)),
                0x95 => Ok((Swap6, 1)),
                0x96 => Ok((Swap7, 1)),
                0x97 => Ok((Swap8, 1)),
                0x98 => Ok((Swap9, 1)),
                0x99 => Ok((Swap10, 1)),
                0x9a => Ok((Swap11, 1)),
                0x9b => Ok((Swap12, 1)),
                0x9c => Ok((Swap13, 1)),
                0x9d => Ok((Swap14, 1)),
                0x9e => Ok((Swap15, 1)),
                0x9f => Ok((Swap16, 1)),

                0xa0 => Ok((Log0, 1)),
                0xa1 => Ok((Log1, 1)),
                0xa2 => Ok((Log2, 1)),
                0xa3 => Ok((Log3, 1)),
                0xa4 => Ok((Log4, 1)),
                0xf0 => Ok((Create, 1)),
                0xf1 => Ok((Call, 1)),
                0xf2 => Ok((CallCode, 1)),
                0xf3 => Ok((Return, 1)),
                0xf4 => Ok((DelegateCall, 1)),
                0xf5 => Ok((Create2, 1)),
                0xfa => Ok((StaticCall, 1)),
                0xfd => Ok((Revert, 1)),
                0xfe => Ok((Invalid, 1)),
                0xff => Ok((Selfdestruct, 1)),

                _ => match mode {
                    EvmOpParserMode::Lax => Ok((Unknown(opcode), 1)),
                    EvmOpParserMode::Strict => {
                        return Err(EvmOpError::ParserErrorUnknownInstruction(opcode));
                    }
                },
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvmCode {
    pub ops: Vec<EvmOp>,
}

#[derive(Error, Debug)]
pub enum EvmCodeError {
    #[error("parser error: incomplete instruction (PUSH) at offset {0}")]
    ParserErrorIncompleteInstruction(usize),
    #[error("parser error: unknown instruction at offset {0}: {1:#04x}")]
    ParserErrorUnknownInstruction(usize, u8),
}

impl EvmCode {
    pub fn new_from_bytes(b: &[u8], mode: EvmOpParserMode) -> Result<Self, EvmCodeError> {
        let mut idx = 0;
        let mut ops = Vec::new();

        while idx < b.len() {
            match EvmOp::new_from_bytes(&b[idx..], mode) {
                Ok((op, offset)) => {
                    ops.push(op);
                    idx += offset;
                }
                Err(EvmOpError::ParserErrorIncompleteInstruction) => {
                    return Err(EvmCodeError::ParserErrorIncompleteInstruction(idx));
                }
                Err(EvmOpError::ParserErrorUnknownInstruction(opcode)) => {
                    return Err(EvmCodeError::ParserErrorUnknownInstruction(idx, opcode));
                }
            }
        }

        Ok(Self { ops })
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut ret = Vec::new();

        for op in &self.ops {
            ret.append(&mut op.to_bytes());
        }

        ret
    }

    pub fn augment(&self) -> Self {
        use EvmOp::*;

        let mut ops = Vec::new();
        let mut idx = 0;

        while idx < self.ops.len() {
            if idx < self.ops.len() - 1 {
                if let Push(len, val) = self.ops[idx] {
                    if self.ops[idx + 1] == Jump {
                        ops.push(AugmentedPushJump(len, val));
                        idx += 2;
                        continue;
                    } else if self.ops[idx + 1] == Jumpi {
                        ops.push(AugmentedPushJumpi(len, val));
                        idx += 2;
                        continue;
                    }
                }
            }

            ops.push(self.ops[idx].clone());
            idx += 1;
        }

        Self { ops }
    }

    pub fn index(&self) -> IndexedEvmCode {
        IndexedEvmCode::new_from_evmcode(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct IndexedEvmCode {
    pub code: EvmCode,
    pub opidx2target: HashMap<usize, U256>,
    pub target2opidx: HashMap<U256, usize>,
    pub jumpdests: HashSet<usize>,
}

impl IndexedEvmCode {
    pub fn new_from_evmcode(code: EvmCode) -> Self {
        let mut opidx2target = HashMap::new();
        let mut target2opidx = HashMap::new();
        let mut jumpdests = HashSet::new();

        let mut target = 0;
        for opidx in 0..code.ops.len() {
            opidx2target.insert(opidx, U256::zero() + target);
            target2opidx.insert(U256::zero() + target, opidx);
            target += code.ops[opidx].len();

            if code.ops[opidx] == EvmOp::Jumpdest {
                jumpdests.insert(opidx);
            }
        }

        Self {
            code,
            opidx2target,
            target2opidx,
            jumpdests,
        }
    }
}
