type UnaryFunction<T, R> = (x: T) => R;

function memoize<T, R>(fn: UnaryFunction<T, R>): UnaryFunction<T, R> {
    const cache = new Map<T, R>();

    return function(x: T): R {
        if (!cache.has(x)) {
            cache.set(x, fn(x));
        }
        return cache.get(x)!;
    }
}
