newtype Html = Html String

newtype Structure = Structure String

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) 
    = Structure (a <> b)


render :: Html -> String
render html = 
    case html of
        Html str -> str
        