import Data.List (break)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)


myDisk :: FSItem
myDisk = 
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaa"
        , File "pop_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "besy mastard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source_code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]


data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])


fsUp :: FSZipper -> Maybe FSZipper 
fsUp (item, FSCrumb name ls rs:bs) = 
    Just (Folder name (ls ++ [item] ++ rs), bs)
otherwise = Nothing

fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo name (Folder folderName items, bs) = 
    let (ls, item:rs) = break (nameIs name) items 
    in Just (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRename :: Name -> FSZipper -> Maybe FSZipper
fsRename newName (Folder name items, bs) = Just (Folder newName items, bs)
fsRename newName (File name dat, bs) = Just (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> Maybe FSZipper
fsNewFile item (Folder folderName items, bs) = 
    Just (Folder folderName (item:items), bs)