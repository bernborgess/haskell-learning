use std::collections::HashMap;
use std::hash::Hash;

fn memoize<T: Eq + Hash, R, F: Fn(T) -> R>(fn: F) -> impl Fn(T) -> R {
    let mut cache = HashMap::new();

    move |t: T| -> R {
        if let Some(result) = cache.get(&t) {
            *result
        } else {
            let result = fn(t);
            cache.insert(t, result);
            result
        }
    }
}
