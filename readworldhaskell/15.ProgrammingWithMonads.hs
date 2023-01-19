import Control.Monad

-- application/x-www-form-urlencoded
-- name=Attila+%42The+Hun%42&occupation=Khan

type Alist = [(String, Maybe String)]

attila :: Alist
attila =
  [ ("name", Just "Attile \"The Hun\"")
  , ("occupation", Just "Khan")
  ]

-- Let's say we want to use one of these alists to
-- fill out a data structure
data MovieReview = MovieReview
  { revTitle :: String
  , revUser :: String
  , revReview :: String
  }

-- We'll begin by belabouring the obvious with a naive
-- function.
simpleReview :: Alist -> Maybe MovieReview
simpleReview alist =
  case lookup "title" alist of
    Just (Just title@(_ : _)) ->
      case lookup "user" alist of
        Just (Just user@(_ : _)) ->
          case lookup "review" alist of
            Just (Just review@(_ : _)) ->
              Just (MovieReview title user review)
            _ -> Nothing -- no review
        _ -> Nothing -- no user
    _ -> Nothing -- no title

-- It only returns a MovieReview if the alist contains
-- all of the necessary values, and they're all non-empty
-- strings. However, the fact that it validates its inputs
-- is its only merit: it suffers badly from the “staircasing”
-- that we've learned to be wary of, and it knows the
-- intimate details of the representation of an alist

-- Since we're now well acquainted with the Maybe monad, we
-- can tidy up the staircasing. No comments
maybeReview :: Alist -> Maybe MovieReview
maybeReview alist = do
  title <- lookupM "title" alist
  user <- lookupM "user" alist
  review <- lookupM "review" alist
  return (MovieReview title user review)

lookupM :: String -> Alist -> Maybe String
lookupM key alist =
  case lookup key alist of
    Just (Just s@(_ : _)) -> Just s
    _ -> Nothing

-- Although this is much tidier, we're still repeating
-- ourselves. We can take advantage of the fact that the
-- MovieReview constructor acts as a normal, pure function
-- by lifting it into the monad, as we discussed in the
-- section called “Mixing pure and monadic code”.

listedReview :: Alist -> Maybe MovieReview
listedReview alist =
  liftM3
    MovieReview
    (lookupM "title" alist)
    (lookupM "user" alist)
    (lookupM "review" alist)

-- https://book.realworldhaskell.org/read/programming-with-monads.html#:~:text=monad%2C%20as%20we-,discussed%20in,-the%20section%20called