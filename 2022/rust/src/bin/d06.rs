use std::io;
use std::fs;
use std::ops::ControlFlow;

pub fn main() -> Result<(), io::Error> {
    let contents = fs::read_to_string("../d06.in")?;
    let contents = contents.trim();
    // windows will give us an iterator over slices of the passed window_size
    // for_each for iter with side effect
    let window_size = 4;
    contents.as_bytes().windows(window_size).enumerate().try_for_each(|(i, w)| {
        // check if any duplicate char in the window
        // iterate over zip offsets 1 to window_size - 1
        // then zip the main slice together with the offset slice
        // qjpq
        //  qjp
        //   qj
        //    q
        // if any of those comparisons match return true (duplicate)
        // if any true value is returned from the zipped comparison also return true
        // -> duplicate character
        if (1..window_size).map(|i| w.iter().zip(w[i..].iter()).any(|(a, b)| a == b)).any(|dupe| dupe) {
            // has duplicate chars
            // println!("DUPE {}, {}", String::from_utf8_lossy(w), w.len())
            // to match return types we need to return Continue here
            ControlFlow::Continue(())
        } else {
            // i represents the i-th window or the index of the starting pos of the window
            // we need the ending pos -> + window_size
            println!("Marker pos {}", i + window_size);
            // println!("{}: {}", i, String::from_utf8_lossy(w));
            // break out of iterator
            return ControlFlow::Break(i + window_size);
        }
    });

    let window_size = 14;
    // we can also capture the return value e.g. Break(x) and use that as result
    contents.as_bytes().windows(window_size).enumerate().try_for_each(|(i, w)| {
        if !(1..window_size).map(|i| w.iter().zip(w[i..].iter()).any(|(a, b)| a == b)).any(|dupe| dupe) {
            println!("Marker pos {}", i + window_size);
            // println!("{}: {}", i, String::from_utf8_lossy(w));
            return ControlFlow::Break(i + window_size);
        }

        ControlFlow::Continue(())
    });

    Ok(())
}

