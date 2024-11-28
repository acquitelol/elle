pub mod build;
pub mod colors;
pub mod help;
pub mod macros;
pub mod modules;

pub fn interleave_with<T: Clone>(vec: Vec<T>, value: T) -> Vec<T> {
    let mut interleaved = Vec::with_capacity(vec.len() * 2);

    for item in vec.iter() {
        interleaved.push(value.clone());
        interleaved.push(item.clone());
    }

    interleaved
}
