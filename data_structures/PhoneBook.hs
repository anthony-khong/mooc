import           Control.Applicative
import           Control.Monad
import qualified Data.Map            as M
import           Data.Monoid
import qualified Data.Sequence       as S

{---------------------------------------------------------------------}
newtype Writer w a = Writer { runWriter :: (a, w) } deriving (Show)

instance Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w)

instance (Monoid w) => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    (Writer (f, w)) <*> (Writer (x, w')) = Writer (f x, w `mappend` w')

instance (Monoid w) => Monad (Writer w) where
    return a              = Writer (a, mempty)
    (Writer (a, w)) >>= f = Writer (a', w `mappend` w')
        where (a', w') = runWriter $ f a
{---------------------------------------------------------------------}

type Logs = S.Seq String
type Query = [String]
type PhoneBook = M.Map String String
type Logged a = Writer Logs a

main :: IO ()
main = do
    n <- fmap parseInt getLine
    qs <- replicateM n (fmap words $ getLine)
    let Writer (_, logs) = traverseQueries qs
        logs' = S.filter (not . null) logs
    putStrLn $ S.foldlWithIndex conc "" logs'
        where conc x 0 y = x ++ y
              conc x _ y = x ++ '\n':y

parseInt :: String -> Int
parseInt = read

traverseQueries :: [Query] -> Logged PhoneBook
traverseQueries = foldl (\pb q -> pb >>= parseQuery q) (return M.empty)

parseQuery :: Query -> PhoneBook -> Logged PhoneBook
parseQuery ["add", key, value] pb = Writer (pb', S.singleton "")
    where pb' = M.insert key value pb
parseQuery ["find", key] pb = Writer (pb, S.singleton name)
    where name = M.findWithDefault "not found" key pb
parseQuery ["del", key] pb = Writer (pb', S.singleton "")
    where pb' = M.delete key pb
parseQuery _ _ = undefined




