{- WARNING: UGLY CODE, READ AT YOUR OWN RISK -}
{-# LANGUAGE OverloadedStrings #-}
import System.Directory (createDirectory)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml)

run :: FilePath -> FilePath -> (Html -> Html) -> IO ()
run f0 f1 temp = readFile f0 >>= writeFile ("./" <> f1 <> "index.html")  . renderHtml . temp . mdToHtml

addPost :: FilePath -> [String] -> IO ()
addPost f [name, date, path] = do
    appendFile "./database.txt" $ name <> " | " <> date <> " | " <> path <> "\n"
    s <- readFile f
    let htm = tempPoop $ mdToHtml s
    createDirectory $ "./posts/" <> path
    writeFile ("./posts/" <> path <> "/index.html") $ renderHtml htm

updArchive :: IO ()
updArchive = do
    s <- readFile "./database.txt"
    let db  = reverse . parse $ T.pack s
        htm = tempPost $ list db
    writeFile "./archive/index.html" $ renderHtml htm

home :: Html -> Html
home latest = tempStd $ do
    latest
    H.p $ H.a ! A.href "./archive/" $ H.toHtml ("View all posts..." :: String)

list :: [[Text]] -> Html
list = H.ul . mapM_ link

link :: [Text] -> Html
link [name, date, path] = do
    H.a ! A.href (H.toValue $ "../posts/" <> path) $ H.toHtml name
    H.p ! A.class_ "archive" $ H.toHtml date
link _ = error "AAAH, COMPUTAR GONNA EXPL0DEZ!1!!!"

parse :: Text -> [[Text]]
parse = fmap (fmap (T.filter (/= ' '))) . fmap (T.split (== '|')) . T.lines

mdToHtml :: String -> Html
mdToHtml = writeHtml def . readMarkdown def

tempStd  = template "./css/style.css" ""
tempPost = template "../css/style.css" "."
tempPoop = template "../../css/style.css" "../."

template :: String -> String -> Html -> Html
template x y body = H.docTypeHtml $ do
    H.meta ! A.charset "utf-8"
    H.head $ do
        H.title $ H.toHtml ("klrr's blog" :: String)
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.toValue x)
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "http://fonts.googleapis.com/css?family=Droid+Serif|Oleo+Script:700"
    H.body $ do
        H.h1 ! A.class_ "fronttitle" $ H.toHtml ("klrr's blog" :: String)
        H.p ! A.class_ "menu" $ do
            H.a ! A.href (H.toValue (y <> "./archive/")) $ H.toHtml ("posts" :: String)
            H.toHtml (" · " :: String)
            H.a ! A.href "https://github.com/klrr" $ H.toHtml ("code" :: String)
            H.toHtml (" · " :: String)
            H.a ! A.href (H.toValue (y <> "./about/")) $ H.toHtml ("me" :: String)
        body
        H.footer $ H.toHtml ("(C) 2014 Karl-Oskar Rikås" :: String)
