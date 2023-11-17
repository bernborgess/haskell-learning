-- ? Types you probably don't want to use

-- Not every type will necessarily be performant or make sense.
-- ListT and Writer/WriterT are examples of this.

-- ? Why not use Writer or WriterT?

-- It’s a bit too easy to get into a situation where Writer is either too
-- lazy or too strict for the problem you’re solving, and then it’ll use
-- more memory than you’d like.

-- Writer can accumulate unevaluated thunks, causing memory leaks.
-- It’s also inappropriate for logging long running or ongoing programs
-- due to the fact that you can’t retrieve any of the logged values
-- until the computation is complete.

-- Usually when Writer is used in an application, it’s not called Writer.
-- Instead a one-off is created for a specific type 𝑤.
-- Given that, it’s still useful to know when you’re looking at something
-- that’s actually a Reader, Writer, or State,
-- even if the author didn’t use the types by those names
-- from the transformers library.
-- Sometimes this is because they wanted a stricter Writer
-- than the Strict Writer already available

-- Determining and measuring when more strictness
-- (more eagerly evaluating your thunks) is needed in your programs
-- is the topic of the upcoming chapter on nonstrictness.

-- ? The ListT you want isn't made from the List type

-- The most obvious way to implement ListT is generally not recommended
-- for a variety of reasons, including:

-- * 1. Most people's first attempt won't pass the associativity law.

--      We're not going to show you a way to write it that does pass
--      that law because it's not really worth it for the reasons
--      listed below.

-- * 2. It's not very fast

-- * 3. Streaming libraries like pipes and conduit do it better for most use-cases

-- Prior art for "ListT done right" also includes Amb/AmbT by Conal Elliot,
-- although you will probably find it challenging to understand if you aren't
-- familiar with ContT and the motivation behind Amb.

-- Lists in Haskell are as much as a controls structure as a data structure,
-- so streaming libraries such as pipes generally suffice if you need a transformer.

-- This is less of a sticking point in writing applications than you'd think.
