use std::io;
use std::fs;
use std::str::FromStr;

const USE_EXAMPLE: bool = false;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct SNAFU(isize);

impl FromStr for SNAFU {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let decimal = s
        .bytes()
        .rev()
        .enumerate()
        .fold(0, |acc, (i, c)| {
            let place_modifier: isize = match c {
                b'2' => 2,
                b'1' => 1,
                b'0' => 0,
                b'-' => -1,
                b'=' => -2,
                _ => unreachable!(),
            };
            acc + 5isize.pow(i as u32) * place_modifier
        });

        Ok(Self(decimal))
    }
}

impl SNAFU {
    pub fn to_str(self) -> String {
        let mut current_remaining = self.0;
        let mut result: Vec<u8> = vec!();

        // NOTE: understood the ones digit and had the same match arm there, but
        // didn't think you could that extend that to the whole number
        // credit: SpokenSpruce
        // Working from right to left, while value > 0
        // 
        // Take value mod 5 as digit
        // divide value by 5
        // If digit in 0..=2: set digit to '0', '1' or '2'
        // If digit is 3: set digit to '-' and add 1 to value
        // If digit is 4: same as above, but with '='
        // Prepend digit to resulting string
        // 
        while current_remaining > 0 {
            // remainder what can't be expressed as 5er place (needs - or =)
            let rem = current_remaining % 5;
            // finished with that digit and one 5er division
            // so reduce one fiver digit by dividing by 5
            //
            // SpokenSpruce's version just adds one as carry for - or =
            // after dividing by 5
            // TiagoPaolini's version described at the bottom of this file subtracts all the added
            // numbers (1 => acc -= 1; 3 => acc -= -2, etc.) so increasing the acc for the - and = case
            // and then divides after
            // -> this version is a little more intuitive, but can be more error prone if you
            //    screw up and the acc isn't evenly divisible by 5 anymore
            current_remaining /= 5;
            result.push(match rem {
                0 => b'0',
                1 => {
                    b'1'
                },
                2 => {
                    b'2'
                },
                // -2
                3 => {
                    // rightmost digit is out of range
                    // -> add the base (5) to the accumulator (current_remaining)
                    //    and prepend (append if reversing at the end) the string
                    //    represantation of the remainder: e.g. here =
                    // since we subtracted instead of adding we add back a five
                    // (since we add after the division by 5, +1 actually is +5)
                    current_remaining += 1;
                    b'='
                },
                // -1
                4 => {
                    // since we subtracted instead of adding we add back a five
                    // (since we add after the division by 5, +1 actually is +5)
                    current_remaining += 1;
                    b'-'
                },
                _ => unreachable!(),
            });
        }

        result.reverse();
        String::from_utf8(result).expect("Should not fail!")
    }

}

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string(if USE_EXAMPLE { "../d25.example.in" } else { "../d25.in" })?;
    let contents = contents.trim();

    let list = contents
        .lines()
        .map(|l| l.parse::<SNAFU>().expect("Should be a valid SNAFU number!"))
        .collect::<Vec<_>>();

    // println!("Numbers: {:?}", list);
    // for n in &list {
    //     println!("{} -> {}", n.to_str(), n.0);
    // }

    let sum = SNAFU(list.iter().fold(0, |acc, x| acc + x.0));
    println!("Part1: Fuel required as SNAFU number {}", sum.to_str());

    Ok(())
}

// NOTE: good general conversion explanation
// u/TiagoPaolini
// For converting other bases to decimal:
// 
// 1. Have an accumulator (initialized to zero) for storing the decimal result.
// 2. Have an register to store the magnitude of the current digit (initialized to one).
// 3. Start from the rightmost digit.
// 4. Multiply the digit's value by the magnitude. Then add the result to the accumulator.
// 5. Multiply the magnitude by the base size (in our case, 5).
// 6. Move to the next digit to the left
// 7. Repeat steps 4 to 6 until you covered all digits, then return the accumulator.
// 
// For converting decimal to other bases:
// 
// 1. Have an accumulator initialized to the decimal value.
// 2. Have an string buffer to store the output.
// 3. Take the modulo of the accumulator by the base size (in our case, 5).
// 4. Convert the result of the previous step to the character that represents the value on the
//    other base. In our c5se, 4 becomes -, and 3 becomes =. The other results remain the same.
// 6. Append the character to the left of the string.
// 7. Subtract the digit's value from the accumulator. Do not forget that - has a value of -1 and =
//    a value of -2 (i8 which cases, you end up actually adding 1 or 2 to the accumulator).
// 9. Divide the accumulator by the base size (in our case, 5). Note: the accumulator should be
//    divisible by the base size, if you did not do anything wrong.
//10. Repeat steps 3 to 7 until the accumulator is zero. Then you have the string that represents
//    the number in the other base.
