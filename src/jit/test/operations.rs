use super::i256::{div_u256, i256_cmp, i256_div, i256_mod, i256_sign, two_compl, Sign};
use core::cmp::Ordering;
use primitive_types::{U256, U512};
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
            let ret = if ret { U256::one() } else { U256::zero() };
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
        U256::one()
    } else {
        U256::zero()
    }
}

pub fn sgt(op1: U256, op2: U256) -> U256 {
    if i256_cmp(op1, op2) == Ordering::Greater {
        U256::one()
    } else {
        U256::zero()
    }
}

pub fn iszero(op1: U256) -> U256 {
    if op1.is_zero() {
        U256::one()
    } else {
        U256::zero()
    }
}

pub fn not(op1: U256) -> U256 {
    !op1
}

pub fn byte(op1: U256, op2: U256) -> U256 {
    let mut ret = U256::zero();

    for i in 0..256 {
        if i < 8 && op1 < 32.into() {
            let o: usize = op1.as_usize();
            let t = 255 - (7 - i + 8 * o);
            let bit_mask = U256::one() << t;
            let value = (op2 & bit_mask) >> t;
            ret = ret.overflowing_add(value << i).0;
        }
    }

    ret
}

pub fn shl(shift: U256, value: U256) -> U256 {
    if value.is_zero() || shift >= U256::from(256) {
        U256::zero()
    } else {
        let shift: u64 = shift.as_u64();
        value << shift as usize
    }
}

pub fn shr(shift: U256, value: U256) -> U256 {
    if value.is_zero() || shift >= U256::from(256) {
        U256::zero()
    } else {
        let shift: u64 = shift.as_u64();
        value >> shift as usize
    }
}

pub fn sar(shift: U256, mut value: U256) -> U256 {
    let value_sign = i256_sign::<true>(&mut value);

    if value.is_zero() || shift >= U256::from(256) {
        match value_sign {
            // value is 0 or >=1, pushing 0
            Sign::Plus | Sign::Zero => U256::zero(),
            // value is <0, pushing -1
            Sign::Minus => two_compl(U256::one()),
        }
    } else {
        let shift: u64 = shift.as_u64();

        match value_sign {
            Sign::Plus | Sign::Zero => value >> shift as usize,
            Sign::Minus => {
                let shifted = ((value.overflowing_sub(U256::one()).0) >> shift as usize)
                    .overflowing_add(U256::one())
                    .0;
                two_compl(shifted)
            }
        }
    }
}

pub fn div(op1: U256, op2: U256) -> U256 {
    if op2.is_zero() {
        U256::zero()
    } else {
        //op1 / op2
        div_u256::div_mod(op1, op2).0
    }
}

pub fn sdiv(op1: U256, op2: U256) -> U256 {
    i256_div(op1, op2)
}

pub fn rem(op1: U256, op2: U256) -> U256 {
    if op2.is_zero() {
        U256::zero()
    } else {
        op1.rem(op2)
    }
}

pub fn smod(op1: U256, op2: U256) -> U256 {
    if op2.is_zero() {
        U256::zero()
    } else {
        i256_mod(op1, op2)
    }
}

pub fn addmod(op1: U256, op2: U256, op3: U256) -> U256 {
    if op3.is_zero() {
        U256::zero()
    } else {
        let op1: U512 = op1.into();
        let op2: U512 = op2.into();
        let op3: U512 = op3.into();
        let v = (op1 + op2) % op3;
        v.try_into()
            .expect("op3 is less than U256::MAX, thus it never overflows; qed")
    }
}

pub fn mulmod(op1: U256, op2: U256, op3: U256) -> U256 {
    if op3.is_zero() {
        U256::zero()
    } else {
        let op1: U512 = op1.into();
        let op2: U512 = op2.into();
        let op3: U512 = op3.into();
        let v = (op1 * op2) % op3;
        v.try_into()
            .expect("op3 is less than U256::MAX, thus it never overflows; qed")
    }
}

pub fn exp(op1: U256, op2: U256) -> U256 {
    let mut op1 = op1;
    let mut op2 = op2;
    let mut r: U256 = 1.into();

    while op2 != 0.into() {
        if op2 & 1.into() != 0.into() {
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
        let bit_index = (8 * op1.low_u32() + 7) as usize;
        let bit = op2.bit(bit_index);
        let mask = (U256::one() << bit_index) - U256::one();
        if bit {
            op2 | !mask
        } else {
            op2 & mask
        }
    } else {
        op2
    }
}
