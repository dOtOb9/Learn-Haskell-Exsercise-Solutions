main = putStrLn myHtml

html_::String -> String
html_ content = "<html>" <> content <> "</html>"

body_::String -> String
body_ content = "<body>" <> content <> "</body>"

head_::String -> String
head_ content = "<head>" <> content <> "</head>"

title_::String -> String
title_ content = "<title>" <> content <> "</title>"

makeHtml::String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

myHtml::String
myHtml = makeHtml "MyPageTitle" "My page content"