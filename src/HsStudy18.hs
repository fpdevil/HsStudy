-----------------------------------------------------------------------------
-- |
-- Module         : HsStudy18
-- Copyright      :  (c) Some description... 2018
--
-- License        : MIT
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
-- Description    :
--
-----------------------------------------------------------------------------
module HsStudy18
  (
    tom
  , jerry
  , tomAndJerry
  , runJerryRun
  , runTomRun
  , hdiv
  , hp
  , hh1
  , hh2
  , hh3
  , article
  , content
  , combine
  , generateHtmlDocContent
  , page
  , right
  , left
  , view
  , topNav
  , widget
  , main
  ) where

import           Control.Monad.Reader
import qualified Data.List            as L
import           System.Environment

{-
ask   :: Reader e e                            -- get whole env
asks  :: (e -> a) -> Reader e a                -- get part of env
local :: (e -> b) -> Reader b a -> Reader e a  -- change env locally
-}

tom :: Reader String String
tom = do
  env <- ask
  return (env ++ " This is Tom!")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry!")

tomAndJerry :: Reader String String
tomAndJerry = do
  t <- tom
  j <- jerry
  return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = runReader tomAndJerry "From Jerry"

runTomRun :: String
runTomRun = runReader tomAndJerry "From Tom"

-- learning the Reader using html generation
-- creating dynamic markup for the web
type HTML = String
type Email = String

hdiv :: [HTML] -> HTML
hdiv children = "<div>" ++ combine children ++ "</div>"

hp :: [HTML] -> HTML
hp children = "<p>" ++ combine children ++ "</p>"

hh1 :: [HTML] -> HTML
hh1 children = "<h1>" ++ combine children ++ "</h1>"

hh2 :: [HTML] -> HTML
hh2 children = "<h2>" ++ combine children ++ "</h2>"


hh3 :: [HTML] -> HTML
hh3 children = "<h3>" ++ combine children ++ "</h3>"


combine :: [HTML] -> HTML
combine = L.intercalate ""

generateHtmlDocContent :: HTML -> HTML
generateHtmlDocContent html =
  "<!DOCTYPE html>\n\
      \<html lang=\"en\">\n\
      \\t<head>\n\
      \\t\t<meta charset=\"UTF-8\">\n\
      \\t\t<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n\
      \\t\t<meta http-equiv=\"X-UA-Compatible\" content=\"ie=edge\">\n\
      \\t\t<title>Document</title>\n\
    \\t</head>\n\
    \\t<body>\n"
  ++ "\t\t" ++ html
  ++ "\n\t</body>\n\
  \</html>\n"

view :: Reader Email HTML
view = do
  page' <- page
  return $ hdiv [ page' ]

page :: Reader Email HTML
page = do
  content' <- content
  return $ hdiv [ topNav
                , content'
                ]

topNav :: HTML
topNav = hdiv [ hh1 ["OurSite.com"] ]

content :: Reader Email HTML
content = do
  email <- ask
  right' <- right
  return $ hdiv [ hh3 ["Custom Content for " ++ email]
                , left
                , right'
                ]

left :: HTML
left = hdiv [ hp [ "this is the left side" ]
            ]

right :: Reader Email HTML
right = do
  article' <- article
  return $ hdiv [ article' ]

article :: Reader Email HTML
article = do
  widget' <- widget
  return $ hdiv [ hp [ "this is an article" ]
                , widget'
                ]

widget :: Reader Email HTML
widget = do
  email <- ask
  return $ hdiv [ hp [ "Hey " ++ email ++ ", we've got a great offer for you" ]
                ]
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "*** BUILD FAILED ***"
      putStrLn "Please provide a email address as a command line parameter"
    (email : _) -> do
      let string = generateHtmlDocContent (runReader view email)
      writeFile filePath string
      putStrLn "*** BUILD SUCCESSFUL ***"
      putStrLn $ "HTML generated and written to the file \"" ++ filePath ++ "\"."
    where
      filePath = "/tmp/reader-demo.html"

-- λ> main
-- *** BUILD FAILED ***
-- Please provide a email address as a command line parameter
-- λ> :main "Singamsetty.Sampath@gmail.com"
-- *** BUILD SUCCESSFUL ***
-- HTML generated and written to the file "/tmp/reader-demo.html".
