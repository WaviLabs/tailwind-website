{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
  ( words
  )
import Lucid
  ( Attribute
  , Html
  -- TAGS
  , a_
  , body_
  , button_
  , div_
  , doctype_
  , head_
  , html_
  , h1_
  , img_
  , nav_
  , script_
  , span_
  , svg_
  , title_
  -- ATTRIBUTES
  , charset_
  , class_
  , classes_
  , content_
  , height_
  , href_
  , lang_
  , meta_
  , name_
  , src_
  , width_
  )
import qualified Lucid

import Lucid.Base (HtmlT, Term, makeAttribute, makeElement, makeElementNoEnd, termRaw, with)

someFunc :: IO ()
someFunc = do
  Lucid.renderToFile "../dist/index'.html" indexHtml

----------------
-- COMPONENTS --
----------------

-- | Wraps the HTML in the same ol' same base HTML
--   with all the header stuff.
wrapBody :: Html () -> Html ()
wrapBody innerHtml = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      title_ "Wavi Labs LLC"
    body_ innerHtml
    script_ [src_ "bundle.js"] ("" :: Text)

mkClasses_ :: Text -> Attribute
mkClasses_ = classes_ . Text.words

viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

xlmns_ :: Text -> Attribute
xlmns_ = makeAttribute "xlmns"

d_ :: Text -> Attribute
d_ = makeAttribute "d"

path_ :: Applicative m => [Attribute] -> HtmlT m ()
path_ = with (makeElementNoEnd "path")

-- | Navbar for the website. This will appear on
-- pretty much every page.
navbar :: Html ()
navbar = do
  nav_ [mkClasses_ "flex items-center justify-between flex-wrap bg-teal-500 p-6"] $ do
    div_ [mkClasses_ "flex items-center flex-shrink-0 text-white mr-6"] $ do
      img_ [src_ "logo.png", width_ "54", height_ "54", viewBox_ "0 0 54 54"]
      span_ [mkClasses_ "font-semibold text-xl tracking-tight"] "Tailwind CSS"
    div_ [mkClasses_ "block lg:hidden"] $
      button_ [mkClasses_ "flex items-center px-3 py-2 border rounded text-teal-200 border-teal-400 hover:text-white hover:border-white"] $
        svg_ [mkClasses_ "fill-current h-3 w-3", viewBox_ "0 0 20 20", xlmns_ "http://www.w3.org/2000/svg"] $ do
          title_ "Menu"
          path_ [d_ "M0 3h20v2H0V3zm0 6h20v2H0V9zm0 6h20v2H0v-2z"]
    div_ [mkClasses_ "w-full block flex-grow lg:flex lg:items-center lg:w-auto"] $ do
      div_ [mkClasses_ "text-sm lg:flex-grow"] $ do
        a_ [href_ "#", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 text-teal-200 hover:text-white mr-4"] "Something"
        a_ [href_ "#", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 text-teal-200 hover:text-white mr-4"] "Something"
        a_ [href_ "#", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 text-teal-200 hover:text-white"] "Something"
      div_ $
        a_ [href_ "#", mkClasses_ "inline-block text-sm px-4 py-2 leading-none border rounded text-white border-white hover:border-transparent hover:text-teal-500 hover:bg-white mt-4 lg:mt-0"] "Download"

-----------
-- PAGES --
-----------

indexHtml :: Html ()
indexHtml = do
  wrapBody navbar
