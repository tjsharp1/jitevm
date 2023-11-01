use super::i256::{div_u256, i256_cmp, i256_div, i256_mod, i256_sign, two_compl, Sign};
use alloy_primitives::{U256, U512};
use core::cmp::Ordering;
use std::ops::{BitAnd, BitOr, BitXor, Rem};

macro_rules! op2_u256_method {
    ($fname:ident, $method:ident) => {
        #[allow(non_snake_case)]
        pub fn $fname(a: U256, b: U256) -> U256 {
            let ret = a.$method(b);
            ret
        }
    };
}

macro_rules! op2_u256_method_ret_tuple {
    ($fname:ident, $method:ident) => {
        #[allow(non_snake_case)]
        pub fn $fname(a: U256, b: U256) -> U256 {
            let (ret, _) = a.$method(b);
            ret
        }
    };
}

macro_rules! op2_u256_method_ref_ret_bool {
    ($fname:ident, $method:ident) => {
        #[allow(non_snake_case)]
        pub fn $fname(a: U256, b: U256) -> U256 {
            let ret = a.$method(&b);
            let ret = if ret { U256::from(1) } else { U256::ZERO };
            ret
        }
    };
}

op2_u256_method_ret_tuple!(add, overflowing_add);
op2_u256_method_ret_tuple!(mul, overflowing_mul);
op2_u256_method_ret_tuple!(sub, overflowing_sub);
op2_u256_method!(and, bitand);
op2_u256_method!(or, bitor);
op2_u256_method!(xor, bitxor);
op2_u256_method_ref_ret_bool!(lt, lt);
op2_u256_method_ref_ret_bool!(gt, gt);
op2_u256_method_ref_ret_bool!(eq, eq);

pub fn slt(op1: U256, op2: U256) -> U256 {
    if i256_cmp(op1, op2) == Ordering::Less {
        U256::from(1)
    } else {
        U256::ZERO
    }
}

pub fn sgt(op1: U256, op2: U256) -> U256 {
    if i256_cmp(op1, op2) == Ordering::Greater {
        U256::from(1)
    } else {
        U256::ZERO
    }
}

pub fn iszero(op1: U256) -> U256 {
    if op1 == U256::ZERO {
        U256::from(1)
    } else {
        U256::ZERO
    }
}

pub fn not(op1: U256) -> U256 {
    !op1
}

pub fn byte(op1: U256, op2: U256) -> U256 {
    let mut ret = U256::ZERO;

    for i in 0..256 {
        if i < 8 && op1 < U256::from(32) {
            let o: usize = op1.to::<usize>();
            let t = 255 - (7 - i + 8 * o);
            let bit_mask = U256::from(1) << t;
            let value = (op2 & bit_mask) >> t;
            ret = ret.overflowing_add(value << i).0;
        }
    }

    ret
}

pub fn shl(shift: U256, value: U256) -> U256 {
    if value == U256::ZERO || shift >= U256::from(256) {
        U256::ZERO
    } else {
        let shift: u64 = shift.to::<u64>();
        value << shift as usize
    }
}

pub fn shr(shift: U256, value: U256) -> U256 {
    if value == U256::ZERO || shift >= U256::from(256) {
        U256::ZERO
    } else {
        let shift: u64 = shift.to::<u64>();
        value >> shift as usize
    }
}

pub fn sar(shift: U256, mut value: U256) -> U256 {
    let value_sign = i256_sign::<true>(&mut value);

    if value == U256::ZERO || shift >= U256::from(256) {
        match value_sign {
            // value is 0 or >=1, pushing 0
            Sign::Plus | Sign::Zero => U256::ZERO,
            // value is <0, pushing -1
            Sign::Minus => two_compl(U256::from(1)),
        }
    } else {
        let shift: u64 = shift.to::<u64>();

        match value_sign {
            Sign::Plus | Sign::Zero => value >> shift as usize,
            Sign::Minus => {
                let shifted = ((value.overflowing_sub(U256::from(1)).0) >> shift as usize)
                    .overflowing_add(U256::from(1))
                    .0;
                two_compl(shifted)
            }
        }
    }
}

pub fn div(op1: U256, op2: U256) -> U256 {
    if op2 == U256::ZERO {
        U256::ZERO
    } else {
        //op1 / op2
        div_u256::div_mod(op1, op2).0
    }
}

pub fn sdiv(op1: U256, op2: U256) -> U256 {
    i256_div(op1, op2)
}

pub fn rem(op1: U256, op2: U256) -> U256 {
    if op2 == U256::ZERO {
        U256::ZERO
    } else {
        op1.rem(op2)
    }
}

pub fn smod(op1: U256, op2: U256) -> U256 {
    if op2 == U256::ZERO {
        U256::ZERO
    } else {
        i256_mod(op1, op2)
    }
}

pub fn addmod(op1: U256, op2: U256, op3: U256) -> U256 {
    if op3 == U256::ZERO {
        U256::ZERO
    } else {
        let op1: U512 = U512::from(op1);
        let op2: U512 = U512::from(op2);
        let op3: U512 = U512::from(op3);
        let v = (op1 + op2) % op3;
        v.to::<U256>()
    }
}

pub fn mulmod(op1: U256, op2: U256, op3: U256) -> U256 {
    if op3 == U256::ZERO {
        U256::ZERO
    } else {
        let op1: U512 = U512::from(op1);
        let op2: U512 = U512::from(op2);
        let op3: U512 = U512::from(op3);
        let v = (op1 * op2) % op3;
        v.to::<U256>()
    }
}

pub fn exp(op1: U256, op2: U256) -> U256 {
    let mut op1 = op1;
    let mut op2 = op2;
    let mut r = U256::from(1);

    while op2 != U256::ZERO {
        if op2 & U256::from(1) != U256::ZERO {
            r = r.overflowing_mul(op1).0;
        }
        op2 >>= 1;
        op1 = op1.overflowing_mul(op1).0;
    }
    r
}

/// In the yellow paper `SIGNEXTEND` is defined to take two inputs, we will call them
/// `x` and `y`, and produce one output. The first `t` bits of the output (numbering from the
/// left, starting from 0) are equal to the `t`-th bit of `y`, where `t` is equal to
/// `256 - 8(x + 1)`. The remaining bits of the output are equal to the corresponding bits of `y`.
/// Note: if `x >= 32` then the output is equal to `y` since `t <= 0`. To efficiently implement
/// this algorithm in the case `x < 32` we do the following. Let `b` be equal to the `t`-th bit
/// of `y` and let `s = 255 - t = 8x + 7` (this is effectively the same index as `t`, but
/// numbering the bits from the right instead of the left). We can create a bit mask which is all
/// zeros up to and including the `t`-th bit, and all ones afterwards by computing the quantity
/// `2^s - 1`. We can use this mask to compute the output depending on the value of `b`.
/// If `b == 1` then the yellow paper says the output should be all ones up to
/// and including the `t`-th bit, followed by the remaining bits of `y`; this is equal to
/// `y | !mask` where `|` is the bitwise `OR` and `!` is bitwise negation. Similarly, if
/// `b == 0` then the yellow paper says the output should start with all zeros, then end with
/// bits from `b`; this is equal to `y & mask` where `&` is bitwise `AND`.

pub fn signextend(op1: U256, op2: U256) -> U256 {
    if op1 < U256::from(32) {
        // `low_u32` works since op1 < 32
        let bit_index = (8 * op1.to::<u32>() + 7) as usize;
        let bit = op2.bit(bit_index);
        let mask = (U256::from(1) << bit_index) - U256::from(1);
        if bit {
            op2 | !mask
        } else {
            op2 & mask
        }
    } else {
        op2
    }
}
