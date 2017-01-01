newtype Logged a = Logged { runWriter :: (a, [Int]) } deriving (Show)

instance Monad Logged where
    return a             = Logged (a,mempty)
    (Logged (a,w)) >>= f = let (a',w') = runWriter $ f a in Logged (a',w `mappend` w')

instance Functor Logged where
    fmap f (Logged (x, logs)) = Logged (f x, logs)

instance Applicative Logged where
    pure x = Logged (x, [])
    Logged (f, fLogs) <*> Logged (x, xLogs) = Logged (f x, fLogs ++ xLogs)

