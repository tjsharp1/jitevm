use crate::jit::{
    context::{JitContractExecutionResult, TransactionContext},
    contract::{BuilderContext, JitEvmEngineBookkeeping, JitEvmEngineSimpleBlock},
    EvmBlock, EvmBlocks, EvmOp, JitEvmEngineError, EVM_STACK_ELEMENT_SIZE, EVM_STACK_SIZE,
};
use inkwell::AddressSpace;
use std::collections::{HashMap, HashSet};

pub trait LendingIteratorLifetime<'this, ImplicitBounds: Sealed = Bounds<&'this Self>> {
    type Item;
}

mod sealed {
    pub trait Sealed: Sized {}
    pub struct Bounds<T>(T);
    impl<T> Sealed for Bounds<T> {}
}
use sealed::{Bounds, Sealed};

pub trait LendingIterator: for<'this> LendingIteratorLifetime<'this> {
    fn next(&mut self) -> Option<<Self as LendingIteratorLifetime<'_>>::Item>;
}

pub(crate) struct Current<'a, 'ctx> {
    idx: usize,
    codesize: usize,
    jumpdests: &'a HashSet<usize>,
    target2blockidx: &'a HashMap<usize, usize>,
    book: JitEvmEngineBookkeeping<'ctx>,
    block: JitEvmEngineSimpleBlock<'ctx>,
    blocks: &'a Vec<JitEvmEngineSimpleBlock<'ctx>>,
    next_block: &'a JitEvmEngineSimpleBlock<'ctx>,
    evm_block: &'a EvmBlock,
}

impl<'a, 'ctx> Current<'a, 'ctx> {
    pub fn step(&mut self) -> bool {
        self.idx += 1;

        let instruction_len = self.evm_block.ops.len();
        self.idx < instruction_len
    }

    pub fn is_last_instruction(&self) -> bool {
        self.idx + 1 == self.evm_block.ops.len()
    }

    pub fn jumpdests(&self) -> &HashSet<usize> {
        self.jumpdests
    }

    pub fn target2blockidx(&self) -> &HashMap<usize, usize> {
        self.target2blockidx
    }

    pub fn blocks(&self) -> &Vec<JitEvmEngineSimpleBlock<'ctx>> {
        self.blocks
    }

    pub fn code_size(&self) -> usize {
        self.codesize
    }

    pub fn idx(&self) -> usize {
        self.evm_block.opidx(self.idx)
    }

    pub fn op(&self) -> EvmOp {
        self.evm_block.ops[self.idx]
    }

    pub fn insert_block(
        &mut self,
        book: JitEvmEngineBookkeeping<'ctx>,
        block: JitEvmEngineSimpleBlock<'ctx>,
    ) {
        self.book = book;
        self.block = block;
    }

    pub fn block(&self) -> &JitEvmEngineSimpleBlock<'ctx> {
        &self.block
    }

    pub fn next_block(&self) -> &JitEvmEngineSimpleBlock<'ctx> {
        self.next_block
    }

    pub fn book_ref_mut(&mut self) -> &mut JitEvmEngineBookkeeping<'ctx> {
        &mut self.book
    }

    pub fn book_ref(&self) -> &JitEvmEngineBookkeeping<'ctx> {
        &self.book
    }
}

pub(crate) struct InstructionIter<'a, 'ctx> {
    init: bool,
    current: Current<'a, 'ctx>,
}

impl<'a, 'ctx> InstructionIter<'a, 'ctx> {
    pub fn new(
        codesize: usize,
        target2blockidx: &'a HashMap<usize, usize>,
        jumpdests: &'a HashSet<usize>,
        blocks: &'a Vec<JitEvmEngineSimpleBlock<'ctx>>,
        evm_block: &'a EvmBlock,
        book: JitEvmEngineBookkeeping<'ctx>,
        block: JitEvmEngineSimpleBlock<'ctx>,
        next_block: &'a JitEvmEngineSimpleBlock<'ctx>,
    ) -> Self {
        let idx = 0;

        Self {
            init: true,
            current: Current {
                idx,
                codesize,
                target2blockidx,
                jumpdests,
                blocks,
                block,
                book,
                next_block,
                evm_block,
            },
        }
    }
}

impl<'this, 'a, 'ctx> LendingIteratorLifetime<'this> for InstructionIter<'a, 'ctx> {
    type Item = &'this mut Current<'a, 'ctx>;
}

impl<'a, 'ctx> LendingIterator for InstructionIter<'a, 'ctx> {
    fn next(&mut self) -> Option<<Self as LendingIteratorLifetime<'_>>::Item> {
        if self.init {
            self.init = false;
            Some(&mut self.current)
        } else if self.current.step() {
            Some(&mut self.current)
        } else {
            None
        }
    }
}

pub(crate) struct CurrentBlock<'a, 'ctx> {
    idx: usize,
    block: Option<JitEvmEngineSimpleBlock<'ctx>>,
    book: JitEvmEngineBookkeeping<'ctx>,
    render: &'a BlockCursor<'ctx>,
}

impl<'a, 'ctx> CurrentBlock<'a, 'ctx> {
    pub fn step(&mut self) -> bool {
        self.idx += 1;
        self.block = None;

        // exclude the stop block, which is already built
        let blocks_len = self.render.blocks.len() - 1;

        if self.idx < blocks_len {
            self.book = self.render.blocks[self.idx].book();
        }

        self.idx < blocks_len
    }

    pub fn index(&self) -> usize {
        self.idx
    }

    pub fn evm_block(&self) -> &EvmBlock {
        &self.render.code.blocks[self.idx]
    }

    pub fn book_ref(&self) -> &JitEvmEngineBookkeeping<'ctx> {
        &self.book
    }

    pub fn book_ref_mut(&mut self) -> &mut JitEvmEngineBookkeeping<'ctx> {
        &mut self.book
    }

    pub fn block(&self) -> &JitEvmEngineSimpleBlock<'ctx> {
        self.block.as_ref().unwrap_or(&self.render.blocks[self.idx])
    }

    pub fn insert_block(
        &mut self,
        book: JitEvmEngineBookkeeping<'ctx>,
        block: JitEvmEngineSimpleBlock<'ctx>,
    ) {
        self.book = book;
        self.block = Some(block);
    }

    pub fn instruction_iter<'b>(&'b mut self) -> InstructionIter<'b, 'ctx> {
        let block = self.block();
        let len = self.render.code.len;
        let targets = &self.render.code.target2blockidx;
        let dests = &self.render.code.jumpdests;
        let blocks = &self.render.blocks;

        InstructionIter::new(
            len,
            targets,
            dests,
            blocks,
            &self.render.code.blocks[self.idx],
            self.book,
            *block,
            &self.render.blocks[self.idx + 1],
        )
    }
}

pub(crate) struct BlockIter<'a, 'ctx> {
    init: bool,
    current: CurrentBlock<'a, 'ctx>,
}

impl<'a, 'ctx> BlockIter<'a, 'ctx> {
    pub fn new(render: &'a BlockCursor<'ctx>) -> Self {
        let idx = 0;
        let book = render.blocks[idx].book();

        Self {
            init: true,
            current: CurrentBlock {
                idx,
                block: None,
                book,
                render,
            },
        }
    }
}

impl<'this, 'a, 'ctx> LendingIteratorLifetime<'this> for BlockIter<'a, 'ctx> {
    type Item = &'this mut CurrentBlock<'a, 'ctx>;
}

impl<'a, 'ctx> LendingIterator for BlockIter<'a, 'ctx> {
    fn next(&mut self) -> Option<<Self as LendingIteratorLifetime<'_>>::Item> {
        if self.init {
            self.init = false;
            Some(&mut self.current)
        } else if self.current.step() {
            Some(&mut self.current)
        } else {
            None
        }
    }
}

pub(crate) struct BlockCursor<'ctx> {
    code: EvmBlocks,
    blocks: Vec<JitEvmEngineSimpleBlock<'ctx>>,
}

impl<'ctx> BlockCursor<'ctx> {
    pub fn new(ctx: &BuilderContext<'ctx>, code: EvmBlocks) -> Result<Self, JitEvmEngineError> {
        let blocks_len = code.blocks.len();
        if blocks_len == 0 {
            return Err(JitEvmEngineError::NoInstructionsToRender);
        }

        // SETUP JIT'ED CONTRACT FUNCTION

        let executecontract_fn_type = ctx.types.type_void.fn_type(
            &[
                ctx.types.type_ptrint.into(),
                ctx.types.type_ptrint.into(),
                ctx.types.type_i64.into(),
            ],
            false,
        );
        let function = ctx
            .module
            .add_function("executecontract", executecontract_fn_type, None);

        // SETUP HANDLER

        let setup_block = ctx.context.append_basic_block(function, "setup");
        ctx.builder.position_at_end(setup_block);

        let setup_book = {
            let execution_context = function.get_nth_param(0).unwrap().into_int_value();
            let execution_context_ptr = ctx.builder.build_int_to_ptr(
                execution_context,
                ctx.types.type_ptrint.ptr_type(AddressSpace::default()),
                "ctx_ptr",
            )?;
            let sp_int = ctx
                .builder
                .build_load(ctx.types.type_ptrint, execution_context_ptr, "sp_int")?
                .into_int_value();
            let max_offset = EVM_STACK_SIZE as u64 * EVM_STACK_ELEMENT_SIZE;
            let sp_max = ctx.builder.build_int_add(
                sp_int,
                ctx.types.type_ptrint.const_int(max_offset, false),
                "sp_max",
            )?;
            let mem_offset = ctx.builder.build_int_add(
                execution_context,
                ctx.types.type_ptrint.size_of(),
                "mem_offset",
            )?;
            let mem_ptr = ctx.builder.build_int_to_ptr(
                mem_offset,
                ctx.types.type_ptrint.ptr_type(AddressSpace::default()),
                "mem_ptr",
            )?;

            let init_gas = function.get_nth_param(2).unwrap().into_int_value();
            let gas_limit = TransactionContext::gas_limit(ctx, execution_context)?;
            let gas_remaining = ctx.builder.build_int_sub(gas_limit, init_gas, "")?;
            let gas_refund = ctx.types.type_i64.const_int(0, false);

            let mem = ctx
                .builder
                .build_load(ctx.types.type_ptrint, mem_ptr, "mem")?
                .into_int_value();
            let mem_size = ctx.types.type_i64.const_int(0, false);
            let mem_gas = ctx.types.type_i64.const_int(0, false);

            JitEvmEngineBookkeeping {
                execution_context,
                sp_min: sp_int,
                sp_max,
                gas_remaining,
                gas_refund,
                sp: sp_int,
                mem,
                mem_size,
                mem_gas,
            }
        };

        // BLOCKS

        let mut blocks: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
        for i in 0..blocks_len {
            let block_before = if i == 0 {
                setup_block
            } else {
                blocks[i - 1].block
            };
            let label = format!("block_{}", i);
            blocks.push(JitEvmEngineSimpleBlock::new(
                ctx,
                block_before,
                &label,
                &format!("_{}", i),
            )?);
        }

        ctx.builder.position_at_end(setup_block);
        ctx.builder.build_unconditional_branch(blocks[0].block)?;
        blocks[0].phi_setup_block(&setup_book, &setup_block);

        // terminal blocks
        blocks.push(JitEvmEngineSimpleBlock::new(
            ctx,
            blocks[blocks_len - 1].block,
            "stop",
            "-stop",
        )?);
        JitContractExecutionResult::build_exit_stop(ctx, &blocks[blocks_len].book())?;

        Ok(BlockCursor { code, blocks })
    }

    pub fn iter<'a>(&'a self) -> BlockIter<'a, 'ctx> {
        BlockIter::new(self)
    }
}
