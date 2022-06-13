def f(a,L=[]):
  L.append(a)
  print(L)

def g(a):
  a += 1
  print(a)

def f1(a,L=[]):
  L.append(a)
  print(L)

if __name__ == '__main__':
  # lst = []
  # f(0,lst)
  # f(1,lst)
  # print(lst)

  # x = 0
  # g(x)
  # print(x)

  f1(0)
  f1(2,[3])
  f1(1)