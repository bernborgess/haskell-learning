import Control.Monad.Trans.Reader
import Data.Bool

-- Read-only

data Config = Conf
    { tweet_key :: String
    , api_secret :: String
    }
    deriving (Show)

config = Conf "teste" "segredo"

postTweet :: String -> Reader Config String
postTweet t = do
    -- b <- validate t
    -- return $ if b then t else "senha invalida"
    bool "senha invalida" t <$> validate t

-- ask' :: Reader e e
-- ask' = reader id

-- askFor :: (e -> a) -> Reader e a
-- askFor f = f <$> ask

validate :: String -> Reader Config Bool
validate t = do
    -- key <- askFor tweet_key
    -- return $ key == "teste"
    ("teste" ==) . tweet_key <$> ask
