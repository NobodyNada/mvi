use anyhow::Context;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{escaped_transform, tag},
    character::complete::{hex_digit1, multispace0, none_of, one_of},
    combinator::{map, map_res, opt, value},
    multi::fold_many0,
    number::complete::double,
    sequence::{delimited, pair},
};
use serde::{Deserialize, Serialize};
use std::{fmt, str::FromStr};

/// Complex memory watch expression
///
/// The expression is executed using a stack and is serialized as a list of operators that interact
/// with this stack.
///
/// # Syntax
///
/// The expression is parsed using a language similar to mesen's. See [`StackOperation`]s for
/// operator syntax.
///
/// Number format:
/// - `1234` is decimal
/// - `$1234` or `0x1234` is hexadecimal
#[derive(Serialize, Deserialize, Clone, Default)]
pub struct Expression {
    /// The expression represented in Reverse Polish notation
    operations: Vec<StackOperation>,

    /// The original text form of the expression
    raw: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
enum StackOperation {
    /// Syntax: `a & b`
    /// Pops two integers from the stack, performs bitwise AND, then pushes the result back
    BitwiseAnd,

    /// Syntax: `~a` or `!a`
    /// Pops one integer from the stack, performs bitwise NOT, then pushes the result back
    BitwiseNot,

    /// Syntax: `a | b`
    /// Pops two integers from the stack, performs bitwise OR, then pushes the result back
    BitwiseOr,

    /// Syntax: `a << b`
    /// Pops two integers from the stack, performs arithmetic left shift, then pushes the result back
    /// The second operand (first one to be poped from the stack) must be positive and fit in `u32`
    BitwiseShiftLeft,

    /// Syntax: `a >> b`
    /// Pops two integers from the stack, performs arithmetic right shift, then pushes the result back
    /// The second operand (first one to be popped from the stack) must be positive and fit in `u32`
    BitwiseShiftRight,

    /// Syntax: `a ^ b`
    /// Pops two integers from the stack, performs bitwise XOR, then pushes the result back
    BitwiseXor,

    /// Syntax: `a / b`
    /// Pops two numbers from the stack, divides them, then pushes the result back
    /// If both numbers are integers, integer division is performed. In this case, the second
    /// operand (first one to be popped from the stack) must not be zero.
    /// If one of the numbers are floats, both are cast to floats and floating point division is
    /// performed. In this case, both operands cannot be zero.
    Divide,

    /// Syntax: `[a]`
    /// Pops an integer from the stack, uses it as an address to read 1 byte from memory, then
    /// pushes the value back.
    /// The integer must fit in `u32`.
    Fetch1Byte,

    /// Syntax: `{a}`
    /// Pops an integer from the stack, uses it as an address to read 2 bytes from memory, then
    /// pushes the value back.
    /// The integer must fit in `u32`.
    Fetch2Bytes,

    /// Syntax: `#a`
    /// Pops an integer from the stack, uses it as an address to read 4 bytes from memory, then
    /// pushes the value back.
    /// The integer must fit in `u32`.
    Fetch4Bytes,

    // TODO doc
    LogicalAnd,
    LogicalOr,
    Minus,
    Modulo,
    Multiply,
    Plus,
    PushFloat(f64),
    PushInteger(i64),
    PushString(String),
    TestEqual,
    TestLowerEqual,
    TestLowerStrict,
    TestGreaterEqual,
    TestGreaterStrict,
    TestUnequal,
    UnaryNegation,
}

enum StackValue<'a> {
    Integer(i64),
    Float(f64),
    String(&'a str),
}

pub enum ExecutionLog {
    // TODO
}

impl FromStr for Expression {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

impl Expression {
    fn parse(s: &str) -> anyhow::Result<Self> {
        fn quoted_string(s: &str) -> IResult<&str, Vec<StackOperation>> {
            let (s, quoted) = delimited(
                tag("'"),
                escaped_transform(
                    none_of("\\'"),
                    '\\',
                    alt((value("\\", tag("\\")), value("'", tag("'")))),
                ),
                tag("'"),
            )(s)?;
            Ok((s, vec![StackOperation::PushString(quoted)]))
        }

        fn double_quoted_string(s: &str) -> IResult<&str, Vec<StackOperation>> {
            let (s, quoted) = delimited(
                tag("\""),
                escaped_transform(
                    none_of("\\\""),
                    '\\',
                    alt((value("\\", tag("\\")), value("\"", tag("\"")))),
                ),
                tag("\""),
            )(s)?;
            Ok((s, vec![StackOperation::PushString(quoted)]))
        }

        fn term(s: &str) -> IResult<&str, Vec<StackOperation>> {
            delimited(
                multispace0,
                alt((
                    delimited(tag("("), binary_operators(BINARY_OPERATORS), tag(")")),
                    map(
                        delimited(tag("["), binary_operators(BINARY_OPERATORS), tag("]")),
                        |mut rpn| {
                            rpn.push(StackOperation::Fetch1Byte);
                            rpn
                        },
                    ),
                    map(
                        delimited(tag("{"), binary_operators(BINARY_OPERATORS), tag("}")),
                        |mut rpn| {
                            rpn.push(StackOperation::Fetch2Bytes);
                            rpn
                        },
                    ),
                    map_res(
                        pair(opt(alt((tag("0x"), tag("$")))), hex_digit1),
                        |(hex_prefix, digits)| {
                            let radix = if hex_prefix.is_some() { 16 } else { 10 };
                            i64::from_str_radix(digits, radix)
                                .map(|i| vec![StackOperation::PushInteger(i)])
                        },
                    ),
                    map(double, |f| vec![StackOperation::PushFloat(f)]),
                    quoted_string,
                    double_quoted_string,
                    // If() and functions...
                )),
                multispace0,
            )(s)
        }

        fn unary_operators(s: &str) -> IResult<&str, Vec<StackOperation>> {
            map(pair(opt(one_of("-~#")), term), |(op, mut rpn)| {
                match op {
                    Some('-') => rpn.push(StackOperation::UnaryNegation),
                    Some('~') => rpn.push(StackOperation::BitwiseNot),
                    Some('#') => rpn.push(StackOperation::Fetch4Bytes),
                    Some(_) => unreachable!(),
                    None => {}
                }
                rpn
            })(s)
        }

        /// binary operators ordered by precedence.
        /// operators at lower indices have lower precedence
        const BINARY_OPERATORS: &[&[&str]] = &[
            &["||"],
            &["&&"],
            &["==", "!=", "<=", ">=", "<", ">"],
            &["|"],
            &["^"],
            &["&"],
            &["<<", ">>"],
            &["+", "-"],
            &["*", "/", "%"],
        ];

        fn binary_operator(ops: &[&str]) -> impl FnMut(&str) -> IResult<&str, &str> {
            |s: &str| match *ops {
                [a] => alt((tag(a),))(s),
                [a, b] => alt((tag(a), tag(b)))(s),
                [a, b, c] => alt((tag(a), tag(b), tag(c)))(s),
                [a, b, c, d, e, f] => alt((tag(a), tag(b), tag(c), tag(d), tag(e), tag(f)))(s),
                _ => unreachable!(),
            }
        }

        fn binary_operators(
            binops: &[&[&str]],
        ) -> impl FnMut(&str) -> IResult<&str, Vec<StackOperation>> {
            |s: &str| -> IResult<&str, Vec<StackOperation>> {
                let Some((low_precedence_binops, binops)) = binops.split_first() else {
                    return unary_operators(s);
                };
                let (s, init) = binary_operators(binops)(s)?;
                fold_many0(
                    pair(
                        binary_operator(low_precedence_binops),
                        binary_operators(binops),
                    ),
                    move || init.clone(),
                    |mut acc, (op, term)| {
                        acc.extend(term);
                        match op {
                            "||" => acc.push(StackOperation::LogicalOr),
                            "&&" => acc.push(StackOperation::LogicalAnd),
                            "==" => acc.push(StackOperation::TestEqual),
                            "!=" => acc.push(StackOperation::TestUnequal),
                            "<=" => acc.push(StackOperation::TestLowerEqual),
                            ">=" => acc.push(StackOperation::TestGreaterEqual),
                            "<" => acc.push(StackOperation::TestLowerStrict),
                            ">" => acc.push(StackOperation::TestGreaterStrict),
                            "|" => acc.push(StackOperation::BitwiseOr),
                            "^" => acc.push(StackOperation::BitwiseXor),
                            "&" => acc.push(StackOperation::BitwiseAnd),
                            "<<" => acc.push(StackOperation::BitwiseShiftLeft),
                            ">>" => acc.push(StackOperation::BitwiseShiftRight),
                            "+" => acc.push(StackOperation::Plus),
                            "-" => acc.push(StackOperation::Minus),
                            "*" => acc.push(StackOperation::Multiply),
                            "/" => acc.push(StackOperation::Divide),
                            "%" => acc.push(StackOperation::Modulo),
                            _ => unreachable!(),
                        }
                        acc
                    },
                )(s)
            }
        }

        fn segments(s: &str) -> IResult<&str, Vec<StackOperation>> {
            fold_many0(
                binary_operators(BINARY_OPERATORS),
                Vec::new,
                |mut acc, term| {
                    acc.extend(term);
                    acc
                },
            )(s)
        }

        let tokens = match segments(s) {
            Ok((rest, tokens)) => {
                if !rest.is_empty() {
                    anyhow::bail!("junk {rest:?} at the end");
                }
                tokens
            }
            Err(err) => anyhow::bail!("parsing failed: {err}"),
        };

        Ok(Self {
            operations: tokens,
            raw: s.to_string(),
        })
    }

    /// `read_memory` takes an address and returns the byte at that address
    pub fn execute<F>(&self, mut read_memory: F) -> anyhow::Result<String>
    where
        F: FnMut(usize) -> Option<u8>,
    {
        let mut stack = Vec::new();
        for operation in &self.operations {
            operation.execute(&mut stack, &mut read_memory)?;
        }

        let mut result = String::new();
        for token in stack {
            use std::fmt::Write;

            match token {
                StackValue::Integer(i) => {
                    let _ = write!(&mut result, "{i}");
                }
                StackValue::Float(f) => {
                    let _ = write!(&mut result, "{f}");
                }
                StackValue::String(s) => result.push_str(s),
            }
        }

        Ok(result)
    }
}

pub fn read_memory_many<F>(address: usize, width: u8, mut read_memory: F) -> anyhow::Result<u64>
where
    F: FnMut(usize) -> Option<u8>,
{
    let mut result = 0;
    for offset in 0..width as usize {
        let address = address + offset;
        let memory_value =
            read_memory(address).with_context(|| format!("out of bounds read at {address}"))?;
        result |= (memory_value as u64) << (offset * 8);
    }
    Ok(result)
}

impl StackOperation {
    fn execute<'a, F>(
        &'a self,
        stack: &mut Vec<StackValue<'a>>,
        read_memory: F,
    ) -> anyhow::Result<()>
    where
        F: FnMut(usize) -> Option<u8>,
    {
        fn single_int_op<F>(stack: &mut Vec<StackValue<'_>>, op: F) -> anyhow::Result<()>
        where
            F: FnOnce(i64) -> anyhow::Result<i64>,
        {
            let Some(StackValue::Integer(i)) = stack.last_mut() else {
                unreachable!()
            };
            *i = op(*i)?;
            Ok(())
        }

        fn double_int_op<F>(stack: &mut Vec<StackValue<'_>>, op: F) -> anyhow::Result<()>
        where
            F: FnOnce(i64, i64) -> anyhow::Result<i64>,
        {
            let Some(StackValue::Integer(j)) = stack.pop() else {
                unreachable!()
            };
            let Some(StackValue::Integer(i)) = stack.last_mut() else {
                unreachable!()
            };
            *i = op(*i, j)?;
            Ok(())
        }

        fn double_float_op<F, G>(
            stack: &mut Vec<StackValue<'_>>,
            op_int: F,
            op_float: G,
        ) -> anyhow::Result<()>
        where
            F: FnOnce(i64, i64) -> anyhow::Result<i64>,
            G: FnOnce(f64, f64) -> anyhow::Result<f64>,
        {
            let j = stack.pop().unwrap();
            let i = stack.pop().unwrap();
            let op_result = match (i, j) {
                (StackValue::Integer(i), StackValue::Integer(j)) => {
                    StackValue::Integer(op_int(i, j)?)
                }
                (StackValue::Integer(i), StackValue::Float(j)) => {
                    StackValue::Float(op_float(i as f64, j)?)
                }
                (StackValue::Float(i), StackValue::Integer(j)) => {
                    StackValue::Float(op_float(i, j as f64)?)
                }
                (StackValue::Float(i), StackValue::Float(j)) => StackValue::Float(op_float(i, j)?),
                _ => unreachable!(),
            };
            stack.push(op_result);
            Ok(())
        }

        fn test<F, G>(stack: &mut Vec<StackValue<'_>>, test_int: F, test_float: G)
        where
            F: FnOnce(i64, i64) -> bool,
            G: FnOnce(f64, f64) -> bool,
        {
            let j = stack.pop().unwrap();
            let i = stack.pop().unwrap();
            let test_result = match (i, j) {
                (StackValue::Integer(i), StackValue::Integer(j)) => test_int(i, j),
                (StackValue::Integer(i), StackValue::Float(j)) => test_float(i as f64, j),
                (StackValue::Float(i), StackValue::Integer(j)) => test_float(i, j as f64),
                (StackValue::Float(i), StackValue::Float(j)) => test_float(i, j),
                _ => unreachable!(),
            };
            stack.push(StackValue::Integer(i64::from(test_result)));
        }

        match self {
            Self::BitwiseAnd => double_int_op(stack, |i, j| Ok(i & j))?,
            Self::BitwiseNot => single_int_op(stack, |i| Ok(!i))?,
            Self::BitwiseOr => double_int_op(stack, |i, j| Ok(i | j))?,
            Self::BitwiseShiftLeft => double_int_op(stack, |i, j| {
                u32::try_from(j)
                    .ok()
                    .and_then(|j| i64::checked_shl(i, j))
                    .with_context(|| format!("overflow during left shift of {i} << {j}"))
            })?,
            Self::BitwiseShiftRight => double_int_op(stack, |i, j| {
                u32::try_from(j)
                    .ok()
                    .and_then(|j| i64::checked_shr(i, j))
                    .with_context(|| format!("overflow during right shift of {i} >> {j}"))
            })?,
            Self::BitwiseXor => double_int_op(stack, |i, j| Ok(i ^ j))?,
            Self::Divide => double_float_op(
                stack,
                |i, j| i64::checked_div(i, j).with_context(|| format!("divided {i} by zero")),
                |f, g| {
                    let x = f / g;
                    if x.is_nan() {
                        anyhow::bail!("tried to divide {f} by {g}");
                    }
                    Ok(x)
                },
            )?,
            Self::Fetch1Byte | Self::Fetch2Bytes | Self::Fetch4Bytes => {
                let width = match self {
                    Self::Fetch1Byte => 1,
                    Self::Fetch2Bytes => 2,
                    Self::Fetch4Bytes => 4,
                    _ => unreachable!(),
                };
                single_int_op(stack, |i| {
                    let address =
                        usize::try_from(i).with_context(|| format!("out-of-bounds read at {i}"))?;
                    let value = read_memory_many(address, width, read_memory)?;
                    Ok(i64::try_from(value).unwrap()) // we don't read more than 4 bytes
                })?;
            }
            Self::PushFloat(f) => stack.push(StackValue::Float(*f)),
            Self::PushInteger(i) => stack.push(StackValue::Integer(*i)),
            Self::LogicalAnd => double_int_op(stack, |i, j| Ok(i64::from(i != 0 && j != 0)))?,
            Self::LogicalOr => double_int_op(stack, |i, j| Ok(i64::from(i != 0 || j != 0)))?,
            Self::Minus => double_float_op(
                stack,
                |i, j| {
                    i64::checked_sub(i, j)
                        .with_context(|| format!("overflow during substraction of {i} - {j}"))
                },
                |f, g| Ok(f - g),
            )?,
            Self::Modulo => double_int_op(stack, |i, j| {
                i64::checked_rem(i, j)
                    .with_context(|| format!("overflow during modulo of {i} % {j}"))
            })?,
            Self::Multiply => double_float_op(
                stack,
                |i, j| {
                    i64::checked_mul(i, j)
                        .with_context(|| format!("overflow during multiplication of {i} * {j}"))
                },
                |f, g| Ok(f * g),
            )?,
            Self::Plus => double_float_op(
                stack,
                |i, j| {
                    i64::checked_add(i, j)
                        .with_context(|| format!("overflow during addition of {i} + {j}"))
                },
                |f, g| Ok(f + g),
            )?,
            Self::PushString(s) => stack.push(StackValue::String(s)),
            Self::TestEqual => test(stack, |i, j| i == j, |f, g| f == g),
            Self::TestLowerEqual => test(stack, |i, j| i <= j, |f, g| f <= g),
            Self::TestLowerStrict => test(stack, |i, j| i < j, |f, g| f < g),
            Self::TestGreaterEqual => test(stack, |i, j| i >= j, |f, g| f >= g),
            Self::TestGreaterStrict => test(stack, |i, j| i > j, |f, g| f > g),
            Self::TestUnequal => test(stack, |i, j| i != j, |f, g| f != g),
            Self::UnaryNegation => match stack.last_mut().unwrap() {
                StackValue::Integer(i) => {
                    *i = i64::checked_neg(*i)
                        .with_context(|| format!("overflow during negation of {i}"))?
                }
                StackValue::Float(f) => {
                    *f = -*f;
                }
                _ => unreachable!(),
            },
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parse and run the expression, all memory reads return 42.
    fn assert_expr(
        expression: &str,
        expected_operations: Vec<StackOperation>,
        expected_result: &str,
    ) {
        let expression: Expression = expression.parse().unwrap();
        assert_eq!(expression.operations, expected_operations);
        let actual_result = expression.execute(|_| Some(42)).unwrap();
        assert_eq!(&actual_result, expected_result);
    }

    #[test]
    fn test_empty() {
        assert_expr("", vec![], "");
    }

    #[test]
    fn test_simple() {
        assert_expr("1", vec![StackOperation::PushInteger(1)], "1");
    }

    #[test]
    fn test_add() {
        assert_expr(
            " 1 +1",
            vec![
                StackOperation::PushInteger(1),
                StackOperation::PushInteger(1),
                StackOperation::Plus,
            ],
            "2",
        );
    }

    #[test]
    fn test_string() {
        assert_expr(
            " \" my string\"  \n 21*(1 + 1)\n",
            vec![
                StackOperation::PushString(" my string".to_owned()),
                StackOperation::PushInteger(21),
                StackOperation::PushInteger(1),
                StackOperation::PushInteger(1),
                StackOperation::Plus,
                StackOperation::Multiply,
            ],
            " my string42",
        );
    }

    #[test]
    fn test_memory() {
        assert_expr(
            "'game state: ' {0x998}",
            vec![
                StackOperation::PushString("game state: ".to_owned()),
                StackOperation::PushInteger(0x998),
                StackOperation::Fetch2Bytes,
            ],
            &format!("game state: {}", 42 << 8 | 42),
        );
    }

    #[test]
    fn test_sm_jump_speed() {
        assert_expr(
            "'jump speed will be ' [$A1C] * {0x909eb9+2*(1+(({$9A2}&(1<<8)==0)&&({$195e}>=0)&&({$afa}+{$b00}>{$195E}))+6*({$9A2}&(1<<8)!=0))} + ({$9A2}&(1<<13)!=0)*({$B42}/2) 'px/f'",
            vec![
                StackOperation::PushString("jump speed will be ".to_owned()),
                StackOperation::PushInteger(0xA1C),
                StackOperation::Fetch1Byte,
                StackOperation::PushInteger(0x909EB9),
                StackOperation::PushInteger(2),
                StackOperation::PushInteger(0x9A2),
                StackOperation::Fetch2Bytes,
                StackOperation::PushInteger(1),
                StackOperation::PushInteger(8),
                StackOperation::BitwiseShiftLeft,
                StackOperation::BitwiseAnd,
                StackOperation::PushInteger(0),
                StackOperation::TestEqual,
                StackOperation::PushInteger(0x195E),
                StackOperation::Fetch2Bytes,
                StackOperation::PushInteger(0),
                StackOperation::TestGreaterEqual,
                StackOperation::LogicalAnd,
                // TODO
            ],
            "jump speed will be px/f",
        );
    }

    #[test]
    fn test_sm_graple_swing() {
        assert_expr(
            "'graple X speed will be ' 'px/f'",
            vec![
                StackOperation::PushString("graple X speed will be ".to_owned()),
                // TODO
            ],
            "graple X speed will be px/f",
        );
    }

    #[test]
    fn test_invalid() {
        assert!("[".parse::<Expression>().is_err());
    }
}
