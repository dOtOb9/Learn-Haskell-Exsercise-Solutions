html_ content = "<html>" <> content <> "</html>"

body_ content = "<body>" <> content <> "</body>"
myHtml content = html_ (body_ "Hello, world!")
