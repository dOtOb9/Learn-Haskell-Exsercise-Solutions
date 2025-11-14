type Bird = Int
type Pole = (Bird, Bird)

landLeft :: Bird -> Pole -> Either String Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise                     = Left (show (left + n, right) ++ " is too many birds on the left!")

landRight :: Bird -> Pole -> Either String Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Right (left, right + n)
    | otherwise                     = Left (show (left, right + n) ++ " is too many birds on the right!")

routine :: Either String Pole
routine = do
    start <- return (0,4)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second