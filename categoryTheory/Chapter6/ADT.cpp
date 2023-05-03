
template <class A>
class Node {
  Node(A a, List<A> l){} ? ?
};

template <class A>
class List {
  Node<A>* _head;

 public:
  List() : _head(nullptr) {}  // Nil
  List(A a, List<A> l)
      : _head(new Node<A>(a, l)) {}
};