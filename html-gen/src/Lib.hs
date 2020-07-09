{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import           Lucid
  ( Html
  , h1_
  )
import qualified Lucid

someFunc :: IO ()
someFunc = do
  Lucid.renderToFile "../dist/index'.html" indexHtml

----------------
-- COMPONENTS --
----------------

navbar :: Html ()
navbar = undefined

-----------
-- PAGES --
-----------

indexHtml :: Html ()
indexHtml = h1_ "Hello World!"
