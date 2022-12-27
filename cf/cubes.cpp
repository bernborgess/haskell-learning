#include <bits/stdc++.h>

using namespace std;
typedef long long ll;
bool val(ll x, ll n) {
  return (x * x * x + 3 * x * x + 2 * x) / 6 <= n;
}

int solve(int i, int s, int n) {
  int l = 1, r = 1010;
  while (l + 1 < r) {
    int m = l + (r - l) / 2;
    if (val(m, n))
      l = m;
    else
      r = m - 1;
  }
  return l;
  return val(l+1,n)?l+1:l;
}

int main() {
  int n;
  cin >> n;
  cout << solve(1, 1, n) << endl;
}
