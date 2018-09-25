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
  ) where

import           Control.Monad.Reader
import qualified Data.List            as L

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

-- creating dynamic markup for the web
type HTML = String
type Email = String

hdiv :: [HTML] -> HTML
hdiv children = "<div>" ++ combine children ++ "</div>"

hh1 :: [HTML] -> HTML
hh1 children = "<h1>" ++ combine children ++ "</h1>"

hp :: [HTML] -> HTML
hp children = "<p>" ++ combine children ++ "</p>"

combine :: [HTML] -> HTML
combine = L.intercalate ""

generateHtmlDocContent :: HTML -> HTML
generateHtmlDocContent = undefined
