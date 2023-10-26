-- For many programmers, monad transformers are indistinguishable from magick, 
-- so we want to approach them from both angles and demonstrate that they are
-- both comprehensible via their types and quite practical in normal programming

-- Functors and Applicatives are both closed under composition, when you compose
-- two monads the result is not necessarily another Monad.

-- Composing monads allow to build up computations with multiple effects. By
-- stacking, for example, a Maybe monad with an IO, you can be performing IO
-- actions while also building up computations that have a possibility of 
-- failure, handled by the Maybe monad.

-- * A Monad Transformer is a variant of an ordinary type that takes an additional
-- type argument which is assumed to have a monad instance. 

-- For example, MaybeT is the transformer variant of the Maybe type.
-- The transformer variant of a type gives us a Monad instance that binds over 
-- both bits of structure. This allows us to compose Monads and combine their effects.
 
-- Getting comfortable with monad transformers is important to becoming proficient 
-- in Haskell. 

-- In this chapter we will:
-- * Demonstrate why composing two monads does not give you another monad;
-- * Examine the Identity and Compose types;
-- * Manipulate types until we can make monads compose;
-- * Meet some common monad transformers;
-- * Work through and Identity crisis.
-- Have fun

