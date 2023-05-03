#include <functional>
#include <unordered_map>

template <typename T, typename R>
std::function<R(T)> memoize(std::function<R(T)> fn) {
    std::unordered_map<T, R> cache;
    return [fn, cache](T t) mutable -> R {
        auto it = cache.find(t);
        if (it != cache.end()) {
            return it->second;
        }
        R result = fn(t);
        cache[t] = result;
        return result;
    };
}
