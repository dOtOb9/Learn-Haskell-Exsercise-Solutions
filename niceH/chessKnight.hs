import Control.Monad (guard)

type KnightPos = (Int, Int)


moveKnight :: [[KnightPos]] -> [[KnightPos]]
moveKnight (route:_) = do
    let (c,r) = head route
    (c', r') <- [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1),
                 (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)
                 ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return ((c', r') : route)

------------------------------------------------

in3 :: KnightPos -> [[KnightPos]]
in3 start = do
    first <- moveKnight [[start]]
    second <- moveKnight [first]
    moveKnight [second]

canReachIn3 :: KnightPos -> KnightPos -> [[KnightPos]]
canReachIn3 start end = filter (\route -> head route == end) (in3 start)