doubleMe x = x*2

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x= (if x > 100 then x else x*2) + 1

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r
    