# A gentle introduction to Haskell for beginners.
### By Bartosz Milewski

The prerequisite for this series of tutorials is some knowledge of imperative programming, whether C++, Java, Pascal, you name it. If you have some familiarity with functional programming, that's even better.

Since I came from an imperative background -- I can write some mean C++ code -- I'm familiar with the potential obstacles in embracing Haskell. There are many myths about functional programming, and Haskell in particular, that have no basis in reality. I will try to dispel them.

One such myth is that you have to know advanced mathematics (category theory in particular) to be a good Haskell programmer. This is not true, and you'll find no high math in this series of tutorials.

Another myth is that you can't do imperative programming in Haskell. This is technically true, in the sense that Haskell is able to reduce all imperative programming to pure functions, but in practice Haskell is the best imperative language that there is. For instance, it gives you full control over side effects. This is a nice thing in general, but it's a real life saver once you decide to write concurrent or parallel programs. In fact I was virtually pushed into functional programming by the necessities of concurrency. If the term "data race" is familiar to you, I don't need to convince you any more.

<br>

## Structure 
<br>  
  
 1. Calling functions 
    
    Basic function call syntax. Without it you can't read any Haskell program.

 2. My First Program 

    Defining simple functions
    
 3. Pure Functions, Laziness, I/O, and Monads 
  Enough about monads to get you going

    a. The Tao of Monad 
 
    An alternative take on monads

 4. Symbolic Calculator: Recursion 
  
    Starting the first Haskell project. Function types. Recursive functions.
 5. Tokenizer: Data Types 

    Defining data types, working with lists.

 6. Tokenizer: Function Types 
 
    Currying, guards.

 7. Tokenizer: Higher Order Functions 

    Functions that take functions as arguments, anonymous functions, map, filter, and fold.

 8. Parser 

    Top-down recursive parser, dealing with state, case/of clause

 9. Evaluator 

    Data.Map, Maybe, module system, expression problem.

 10. Error Handling

     Either monad, typeclasses

 11. State Monad 
 
     A monad to encapsulate state manipulation.

 12. The List Monad 

     Non-deterministic computations, list comprehensions