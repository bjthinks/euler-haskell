#include <vector>
#include <algorithm>
#include <iostream>

using namespace std;

int nimVal(int n, const vector<int> &nimVals) {
  static vector<int> vals(16);
  for (int i=0; i<16; ++i)
    vals[i] = 0;
  for (int i=0; i<n-1; ++i) {
    int v_i = nimVals[i] ^ nimVals[n-i-2];
    if (v_i < 16)
      vals[v_i] = 1;
    else
      abort();
  }
  int i;
  for (i=0; i<16; ++i)
    if (vals[i] == 0)
      break;
  return i;
}

int main(int argc, char *argv[]) {
  int N = atoi(argv[1]);
  vector<int> nimVals(N);

  nimVals[0] = 0;
  nimVals[1] = 0;
  for (int i=2; i<N; ++i) {
    nimVals[i] = nimVal(i, nimVals);
    if (i % 1000 == 1) cout << i << endl;
  }

  int k = 0;
  for (int i=0; i<N; ++i)
    if (nimVals[i] > 0)
      ++k;
  cout << k << endl;

  return 0;
}
