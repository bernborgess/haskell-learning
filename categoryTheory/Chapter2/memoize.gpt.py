def memoize(fn):
    cache = {}
    def wrapper(x):
        if x not in cache:
            cache[x] = fn(x)
        return cache[x]
    return wrapper
