import Control.Monad.Instances


addStuff :: Int -> Int
addStuff = do
    x <- (*2)
    y <- (+10)
    return (x + y)

