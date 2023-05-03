#include <chrono>
#include <iomanip>
#include <iostream>
#include <map>
using namespace std;

auto memoize(auto fn) {
  return [done = std::map<int, int>{}, fn](auto n) mutable {
    if (auto it = done.find(n); it != done.end())
      return it->second;
    return done[n] = fn(n);
  };
}

int main() {
  auto sqrIter = [&](int n) -> int {
    if (n <= 1) return 1;
    int ans = 0;
    for (int i = 0; i < n; i++)
      for (int j = 0; j < n; j++)
        ans += i * j;
    return ans;
  };
  auto memozd = memoize(sqrIter);
  int n;
  while (true) {
    cout << "n: ";
    cin >> n;
    auto start = chrono::high_resolution_clock::now();
    int rRes = memozd(n);
    auto end = chrono::high_resolution_clock::now();
    double time_taken =
        chrono::duration_cast<chrono::nanoseconds>(end - start).count();
    time_taken *= 1e-9;
    cout << "Result = " << rRes << endl;
    cout << "Time taken to f(" << n << ") is : " << fixed
         << time_taken << setprecision(9) << " sec" << endl;
  }
}
