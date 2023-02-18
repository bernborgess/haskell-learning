
const ord = (s: string) => s.charCodeAt(0);

const char = (i: number): string => String.fromCharCode(i);

const charList = (fst: string, lst: string): string[] => Array.from(
  { length: ord(lst) - ord(fst) + 1 },
  (_, i) => char(i + ord(fst))
);

const filter = <A>(pred: (x: A) => boolean) => (list: A[]): A[] =>
  list.filter(pred);

const take = <A>(amount: number) => (list: A[]): A[] =>
  list.slice(0, amount);

//! (.) :: (b -> c) -> (a -> b) -> (a -> c)
// const dot = <A, B, C>(f: (x: B) => C) => (g: (x: A) => B) => (x: A) => f(g(x))

const cmp = <A, B, C>(f: (x: A) => B, g: (x: B) => C) =>
  (x: A) => g(f(x))

const addOne = (x: number) => x + 1;
const show = (x: number) => x.toString();

const showOneMore = cmp(addOne, show);

//? Conclusion: typescript doesn't work for currying


// const firstNofF = <A>(n:number,pred:(x:A)=>Boolean,list:A[])
// take n . filter pred