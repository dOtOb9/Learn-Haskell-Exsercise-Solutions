import Data.Ratio
import Data.List (all)


newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs


instance Applicative Prob where
    pure x = Prob [(x, 1%1)]
    
    -- <*> (apply) の定義
    -- Prob (f, pf) <*> Prob (x, px) = Prob (f x, pf * px) を拡張したもの
    (Prob fs) <*> (Prob xs) = Prob $
        [ (f x, pf * px) 
        | (f, pf) <- fs
        , (x, px) <- xs
        ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs


instance Monad Prob where
    return = pure
    m >>= f = flatten (fmap f m)


instance MonadFail Prob where
    fail _ = Prob []


------------------------------------------------------

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob ([(Heads, 1%10), (Tails, 9%10)])

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Heads) [a,b,c])

    