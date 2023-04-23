#include <bits/stdc++.h>

struct Point {
  int x;
  int y;
};

int myFunc(const std::vector<Point>& myPoints) {
  int result = 0;
  for (const auto& point : myPoints) {
    if (result % 2 == 0) {
      result += point.x;
    } else {
      result += point.y;
    }
  }
  return result;
}

int main() {
  int n;
  std::cin >> n;
  std::vector<Point> v(n);
  for (auto& [x, y] : v)
    std::cin >> x >> y;
  // 42
  std::cout << myFunc(v) << std::endl;
  return 0;
}