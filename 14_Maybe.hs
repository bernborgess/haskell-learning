import Data.Maybe
-- Maybe
-- data Maybe a = Nothing | Just a

-- f x
-- if normal, -> Result
-- if error, -> ???

-- if normal, -> Just Result
-- if error, -> Nothing

-- Ex:
safediv :: Integral a => a ->a -> Maybe a
safediv a b =
  if b == 0 then Nothing else Just $ a `div` b


-- * Data.Maybe
-- isJust :: Maybe a -> Bool
-- isNothing :: Maybe a -> Bool
-- fromJust :: Maybe a -> a

-- Default value if error
-- fromMaybe :: a -> Maybe a -> a
-- fromMaybe 3.1415 (Nothing)
--  => 3.1415
-- fromMaybe 3.1415 (Just 2.7183)
-- => 2.7183
