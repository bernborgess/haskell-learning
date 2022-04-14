#include<iostream>
#include<stack>
#include<vector>
using namespace std;
typedef vector<int> vint;
typedef stack<int> sint;
typedef pair<float,float> pff;


// count e =
//   foldr (\x acc -> if e==x then acc+1 else acc) 0
int count(int e,vint a) {
  int acc=0;
  for(int i=a.size()-1;i>=0;i--) {
  
    acc=(e==a[i]?acc+1:acc);

    // if(e==a[i]) {
    //   acc+=1;
    // } else {
    //   acc;
    // }
  }
  return acc;
}


// rev :: [a] -> [a]
// rev = foldl (\acc x -> x : acc) []
// sint rev(sint a) {
//   sint acc;
//   while(!a.empty()) {
//     a.top();

//     a.pop();
//   }
// }



// lagrange :: [(Float,Float)] -> Float -> Float
// lagrange xs x = foldl (\acc (xj,y)->acc+(y* l xj)) 0 xs
//  where
//   l xj = foldl (\acc (xm,_) -> if xj==xm then acc else acc * ((x-xm)/(xj-xm))) 1 xs 
// -- <-- a lista em si mas so primeira coordenada X importa aqui.
// --  ^-- elemento neutro do produto.
float lagrange(vector<pff> xs,float x) {
  float acc=0;

  auto l = [&](float xj) -> float {
    float acc=1;
    for(auto&[xm,_]:xs) {
      acc=(xj==xm?acc:acc*((x-xm)/(xj-xm)));
    }
    return acc;
  };

  for(auto&[xj,y]:xs) {
    acc=acc+(y*l(xj));
  }
  return acc;
}


int main() {
  
  
  
  // foldl
}