el :: String -> String -> String
el tag content = 
    "<" <> tag <> ">" <> content <> "<" <> tag <> ">"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"
