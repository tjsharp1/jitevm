use alloy_primitives::{uint, U256};
use core::cmp::Ordering;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Sign {
    Plus,
    Minus,
    Zero,
}

pub const MIN_NEGATIVE_VALUE: U256 =
    uint!(0x8000000000000000000000000000000000000000000000000000000000000000_U256);

const SIGN_BITMASK_U64: u64 = 0x8000000000000000;
const FLIPH_BITMASK_U64: u64 = 0x7FFFFFFFFFFFFFFF;
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct I256(pub Sign, pub U256);

#[inline(always)]
pub fn i256_sign<const DO_TWO_COMPL: bool>(val: &mut U256) -> Sign {
    if val.as_limbs()[3] & SIGN_BITMASK_U64 == 0 {
        if *val == U256::ZERO {
            Sign::Zero
        } else {
            Sign::Plus
        }
    } else {
        if DO_TWO_COMPL {
            two_compl_mut(val);
        }
        Sign::Minus
    }
}

#[inline(always)]
fn u256_remove_sign(val: &mut U256) {
    unsafe {
        val.as_limbs_mut()[3] &= FLIPH_BITMASK_U64;
    }
}

#[inline(always)]
pub fn two_compl_mut(op: &mut U256) {
    *op = two_compl(*op);
}

pub fn two_compl(op: U256) -> U256 {
    !op + U256::from(1)
}

#[inline(always)]
pub fn i256_cmp(mut first: U256, mut second: U256) -> Ordering {
    let first_sign = i256_sign::<false>(&mut first);
    let second_sign = i256_sign::<false>(&mut second);
    match (first_sign, second_sign) {
        (Sign::Zero, Sign::Zero) => Ordering::Equal,
        (Sign::Zero, Sign::Plus) => Ordering::Less,
        (Sign::Zero, Sign::Minus) => Ordering::Greater,
        (Sign::Minus, Sign::Zero) => Ordering::Less,
        (Sign::Minus, Sign::Plus) => Ordering::Less,
        (Sign::Minus, Sign::Minus) => first.cmp(&second),
        (Sign::Plus, Sign::Minus) => Ordering::Greater,
        (Sign::Plus, Sign::Zero) => Ordering::Greater,
        (Sign::Plus, Sign::Plus) => first.cmp(&second),
    }
}

#[inline(always)]
pub fn i256_div(mut first: U256, mut second: U256) -> U256 {
    let second_sign = i256_sign::<true>(&mut second);
    if second_sign == Sign::Zero {
        return U256::ZERO;
    }
    let first_sign = i256_sign::<true>(&mut first);
    if first_sign == Sign::Minus && first == MIN_NEGATIVE_VALUE && second == U256::from(1) {
        return two_compl(MIN_NEGATIVE_VALUE);
    }

    //let mut d = first / second;
    let mut d = div_u256::div_mod(first, second).0;

    u256_remove_sign(&mut d);
    //set sign bit to zero

    if d == U256::ZERO {
        return U256::ZERO;
    }

    match (first_sign, second_sign) {
        (Sign::Zero, Sign::Plus)
        | (Sign::Plus, Sign::Zero)
        | (Sign::Zero, Sign::Zero)
        | (Sign::Plus, Sign::Plus)
        | (Sign::Minus, Sign::Minus) => d,
        (Sign::Zero, Sign::Minus)
        | (Sign::Plus, Sign::Minus)
        | (Sign::Minus, Sign::Zero)
        | (Sign::Minus, Sign::Plus) => two_compl(d),
    }
}

#[inline(always)]
pub fn i256_mod(mut first: U256, mut second: U256) -> U256 {
    let first_sign = i256_sign::<true>(&mut first);
    if first_sign == Sign::Zero {
        return U256::ZERO;
    }

    let _ = i256_sign::<true>(&mut second);
    let mut r = first % second;
    u256_remove_sign(&mut r);
    if r == U256::ZERO {
        return U256::ZERO;
    }
    if first_sign == Sign::Minus {
        two_compl(r)
    } else {
        r
    }
}

pub mod div_u256 {
    use super::*;

    const WORD_BITS: usize = 64;
    /// Returns a pair `(self / other, self % other)`.
    ///
    /// # Panics
    ///
    /// Panics if `other` is zero.
    #[inline(always)]
    pub fn div_mod(me: U256, other: U256) -> (U256, U256) {
        let my_bits = me.bit_len();
        let your_bits = other.bit_len();

        assert!(your_bits != 0, "division by zero");

        // Early return in case we are dividing by a larger number than us
        if my_bits < your_bits {
            return (U256::ZERO, me);
        }

        if your_bits <= WORD_BITS {
            return div_mod_small(me, other.to::<u64>());
        }

        let (n, m) = {
            let my_words = words(my_bits);
            let your_words = words(your_bits);
            (your_words, my_words - your_words)
        };

        div_mod_knuth(me, other, n, m)
    }

    #[inline(always)]
    fn div_mod_small(mut me: U256, other: u64) -> (U256, U256) {
        let mut rem = 0u64;
        unsafe {
            for d in me.as_limbs_mut().iter_mut().rev() {
                let (q, r) = div_mod_word(rem, *d, other);
                *d = q;
                rem = r;
            }
        }
        (me, U256::from(rem))
    }

    // Whether this fits u64.
    #[inline(always)]
    fn fits_word(me: &U256) -> bool {
        for i in me.as_limbs().iter().take(4).skip(1) {
            if *i != 0 {
                return false;
            }
        }
        true
    }

    // See Knuth, TAOCP, Volume 2, section 4.3.1, Algorithm D.
    #[inline(always)]
    fn div_mod_knuth(me: U256, mut v: U256, n: usize, m: usize) -> (U256, U256) {
        debug_assert!(me.bit_len() >= v.bit_len() && !fits_word(&v));
        debug_assert!(n + m <= 4);
        // D1.
        // Make sure 64th bit in v's highest word is set.
        // If we shift both self and v, it won't affect the quotient
        // and the remainder will only need to be shifted back.
        let shift = v.as_limbs()[n - 1].leading_zeros();
        v = v.wrapping_shl(shift as usize);
        // u will store the remainder (shifted)
        let mut u = full_shl(me, shift);

        // quotient
        let mut q = U256::ZERO;
        let v_n_1 = v.as_limbs()[n - 1];
        let v_n_2 = v.as_limbs()[n - 2];

        // D2. D7.
        // iterate from m downto 0
        for j in (0..=m).rev() {
            let u_jn = u[j + n];

            // D3.
            // q_hat is our guess for the j-th quotient digit
            // q_hat = min(b - 1, (u_{j+n} * b + u_{j+n-1}) / v_{n-1})
            // b = 1 << WORD_BITS
            // Theorem B: q_hat >= q_j >= q_hat - 2
            let mut q_hat = if u_jn < v_n_1 {
                let (mut q_hat, mut r_hat) = div_mod_word(u_jn, u[j + n - 1], v_n_1);
                // this loop takes at most 2 iterations
                loop {
                    // check if q_hat * v_{n-2} > b * r_hat + u_{j+n-2}
                    let (hi, lo) = split_u128(u128::from(q_hat) * u128::from(v_n_2));
                    if (hi, lo) <= (r_hat, u[j + n - 2]) {
                        break;
                    }
                    // then iterate till it doesn't hold
                    q_hat -= 1;
                    let (new_r_hat, overflow) = r_hat.overflowing_add(v_n_1);
                    r_hat = new_r_hat;
                    // if r_hat overflowed, we're done
                    if overflow {
                        break;
                    }
                }
                q_hat
            } else {
                // here q_hat >= q_j >= q_hat - 1
                u64::max_value()
            };

            // ex. 20:
            // since q_hat * v_{n-2} <= b * r_hat + u_{j+n-2},
            // either q_hat == q_j, or q_hat == q_j + 1

            // D4.
            // let's assume optimistically q_hat == q_j
            // subtract (q_hat * v) from u[j..]
            let q_hat_v = full_mul_u64(v, q_hat);
            // u[j..] -= q_hat_v;
            let c = sub_slice(&mut u[j..], &q_hat_v[..n + 1]);

            // D6.
            // actually, q_hat == q_j + 1 and u[j..] has overflowed
            // highly unlikely ~ (1 / 2^63)
            if c {
                q_hat -= 1;
                // add v to u[j..]
                let c = add_slice(&mut u[j..], &v.as_limbs()[..n]);
                u[j + n] = u[j + n].wrapping_add(u64::from(c));
            }

            // D5.
            unsafe {
                q.as_limbs_mut()[j] = q_hat;
            }
        }

        // D8.
        let remainder = full_shr(u, shift);

        (q, remainder)
    }

    #[inline(always)]
    fn add_slice(a: &mut [u64], b: &[u64]) -> bool {
        binop_slice(a, b, u64::overflowing_add)
    }

    #[inline(always)]
    fn sub_slice(a: &mut [u64], b: &[u64]) -> bool {
        binop_slice(a, b, u64::overflowing_sub)
    }

    #[inline(always)]
    fn binop_slice(
        a: &mut [u64],
        b: &[u64],
        binop: impl Fn(u64, u64) -> (u64, bool) + Copy,
    ) -> bool {
        let mut c = false;
        a.iter_mut().zip(b.iter()).for_each(|(x, y)| {
            let (res, carry) = binop_carry(*x, *y, c, binop);
            *x = res;
            c = carry;
        });
        c
    }

    #[inline(always)]
    fn binop_carry(
        a: u64,
        b: u64,
        c: bool,
        binop: impl Fn(u64, u64) -> (u64, bool),
    ) -> (u64, bool) {
        let (res1, overflow1) = b.overflowing_add(u64::from(c));
        let (res2, overflow2) = binop(a, res1);
        (res2, overflow1 || overflow2)
    }

    #[inline(always)]
    fn full_shl(me: U256, shift: u32) -> [u64; 4 + 1] {
        debug_assert!(shift < WORD_BITS as u32);
        let mut u = [0u64; 4 + 1];
        let u_lo = me.as_limbs()[0] << shift;
        let u_hi = me.wrapping_shr(WORD_BITS - shift as usize);
        u[0] = u_lo;
        u[1..].copy_from_slice(u_hi.as_limbs());
        u
    }

    #[inline(always)]
    fn full_shr(u: [u64; 4 + 1], shift: u32) -> U256 {
        debug_assert!(shift < WORD_BITS as u32);
        let mut res = U256::ZERO;
        for (i, item) in u.iter().enumerate().take(4) {
            unsafe {
                res.as_limbs_mut()[i] = item >> shift;
            }
        }
        // carry
        if shift > 0 {
            for (i, item) in u.iter().enumerate().skip(1) {
                unsafe {
                    res.as_limbs_mut()[i - 1] |= item << (WORD_BITS as u32 - shift);
                }
            }
        }
        res
    }

    #[inline(always)]
    fn full_mul_u64(me: U256, by: u64) -> [u64; 4 + 1] {
        let (prod, carry) = overflowing_mul_u64(me, by);
        let mut res = [0u64; 4 + 1];
        res[..4].copy_from_slice(prod.as_limbs());
        res[4] = carry;
        res
    }

    /// Overflowing multiplication by u64.
    /// Returns the result and carry.
    #[inline(always)]
    fn overflowing_mul_u64(mut me: U256, other: u64) -> (U256, u64) {
        let mut carry = 0u64;

        unsafe {
            for d in me.as_limbs_mut().iter_mut() {
                let (res, c) = mul_u64(*d, other, carry);
                *d = res;
                carry = c;
            }
        }

        (me, carry)
    }

    #[inline(always)]
    // Returns the least number of words needed to represent the nonzero number
    fn words(bits: usize) -> usize {
        debug_assert!(bits > 0);
        1 + (bits - 1) / WORD_BITS
    }

    #[inline(always)]
    fn mul_u64(a: u64, b: u64, carry: u64) -> (u64, u64) {
        let (hi, lo) = split_u128(a as u128 * b as u128 + carry as u128);
        (lo, hi)
    }

    #[inline(always)]
    const fn split_u128(a: u128) -> (u64, u64) {
        ((a >> 64) as _, (a & 0xFFFFFFFFFFFFFFFF) as _)
    }

    #[inline(always)]
    fn div_mod_word(hi: u64, lo: u64, y: u64) -> (u64, u64) {
        debug_assert!(hi < y);
        let x = (u128::from(hi) << 64) + u128::from(lo);
        let d = u128::from(y);
        ((x / d) as u64, (x % d) as u64)
    }
}
