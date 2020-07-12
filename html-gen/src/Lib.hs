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
  , li_
  , nav_
  , script_
  , span_
  , svg_
  , title_
  , ul_
  -- ATTRIBUTES
  , alt_
  , charset_
  , class_
  , classes_
  , content_
  , height_
  , href_
  , id_
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
  Lucid.renderToFile "../dist/indexT.html" indexHtml

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

{-
<nav class="fixed top-0 w-full z-10 bg-white p-6 shadow">
  <!-- LARGE NAVBAR -->
  <div class="w-full flex flex-wrap items-center justify-between">
    <div class="lg:flex hidden text-lg">
      <span>
        <a href="#responsive-header" class="block mt-4 lg:inline-block lg:mt-0 text-blue hover:text-white mr-4">
          Blog
        </a>
        <a href="#responsive-header" class="block mt-4 lg:inline-block lg:mt-0 text-blue hover:text-white mr-4">
          Services
        </a>
        <a href="#responsive-header" class="block mt-4 lg:inline-block lg:mt-0 text-blue hover:text-white mr-4">
          Code
        </a>
        <a href="#responsive-header" class="block mt-4 lg:inline-block lg:mt-0 text-blue hover:text-white">
          FAQ
        </a>
      </span>
    </div>
    <div class="lg:flex lg:items-center hidden">
      <img src="logo.png" alt="logo" width="300" height="300" viewBox="0 0 250 250">
    </div>
    <div class="flex items-center lg:hidden">
      <img src="logo.png" alt="logo" width="200" height="200" viewBox="0 0 250 250">
    </div>
    <div class="lg:flex lg:items-center hidden text-lg">
      <button class="bg-blue hover:bg-green text-white font-semibold py-2 px-4 border border-gray-400 rounded shadow mr-8">
        Button
      </button>
      <button class="bg-blue hover:bg-green text-white font-semibold py-2 px-4 border border-gray-400 rounded shadow">
        Button
      </button>
    </div>
    <div class="block items-center lg:hidden text-lg">
      <button id="nav-toggle" class="flex flex-wrap items-center px-3 py-2 border rounded text-blue border-blue">
        <svg class="fill-current h-5 w-5" xmlns="http://www.w3.org/2000/svg"><title>Menu</title><path d="M0 3h20v2H0V3zm0 6h20v2H0V9zm0 6h20v2H0v-2z"></path></svg>
      </button>
    </div>
    <div id="nav-content" class="w-full items-center mt-2 z-20 hidden">
      <ul class="list-reset flex flex-col justify-end flex-1 items-center">
        <li>
          <a class="inline-block py-2 px-4 text-black no-underline" href="#">
            Blog
          </a>
        </li>
        <li>
          <a class="inline-block text-black no-underline hover:text-gray-800 hover:text-underline py-2 px-4" href="#">
            Services
          </a>
        </li>
        <li>
          <a class="inline-block text-black no-underline hover:text-gray-800 hover:text-underline py-2 px-4" href="#">
            Tools
          </a>
        </li>
        <li>
          <a class="inline-block text-black no-underline hover:text-gray-800 hover:text-underline py-2 px-4" href="#">
            FAQ
          </a>
        </li>
        <li>
          <button class="bg-blue hover:bg-green text-white font-semibold py-2 px-4 border border-gray-400 rounded shadow">
            Button
          </button>
        </li>
      </ul>
    </div>

  </div>
</nav>
-}

-- | Navbar for the website. This will appear on
-- pretty much every page.
navbar :: Html ()
navbar = do
  nav_ [mkClasses_ "fixed top-0 w-full z-10 bg-white p-6 shadow"] $ do
    div_ [mkClasses_ "w-full flex flex-wrap items-center justify-between"] $ do
      -- Contains nav links to other pages
      div_ [mkClasses_ "lg:flex hidden text-lg"] $ do
        span_ [] $ do
          a_
            [href_ "#", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 text-blue hover:text-white mr-4"]
            "Blog"
          a_
            [href_ "#", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 text-blue hover:text-white mr-4"]
            "Blog"
          a_
            [href_ "#", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 text-blue hover:text-white mr-4"]
            "Blog"
          a_
            [href_ "#", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 text-blue hover:text-white mr-4"]
            "Blog"
      -- Big logo for large and above
      div_ [mkClasses_ "lg:flex lg:items-center hidden"] $
        img_ [src_ "logo.png", alt_ "Wavi Labs logo", width_ "300", height_ "300", viewBox_ "0 0 300 300"]
      -- Small for less than large
      div_ [mkClasses_ "flex items-center lg:hidden"] $
        img_ [src_ "logo.png", alt_ "Wavi Labs logo", width_ "200", height_ "200", viewBox_ "0 0 200 200"]
      -- Random buttons for large navbar
      div_ [mkClasses_ "lg:flex lg:items-center hidden text-lg"] $ do
        button_
          [mkClasses_ "bg-blue hover:bg-green text-white font-semibold py-2 px-4 border border-gray-400 rounded shadow mr-4"]
          "Button"
        button_
          [mkClasses_ "bg-blue hover:bg-green text-white font-semibold py-2 px-4 border border-gray-400 rounded shadow"]
          "Button"
      -- Small hamburger icon for less than large
      div_ [mkClasses_ "block items-center lg:hidden text-lg"] $
        button_ [id_ "nav-toggle", mkClasses_ "flex flex-wrap items-center px-3 py-2 border rounded text-blue border-blue"] $
          svg_ [mkClasses_ "fill-current h-5 w-5", xlmns_ "http://www.w3.org/2000/svg"] $ do
            title_ "Menu"
            path_ [d_ "M0 3h20v2H0V3zm0 6h20v2H0V9zm0 6h20v2H0v-2z"]
      -- Hamburger content
      div_ [id_ "nav-content", mkClasses_ "w-full items-center mt-2 z-20 hidden"] $
        ul_ [mkClasses_ "list-reset flex flex-col justify-end flex-1 items-center"] $ do
          li_ $ a_ [href_ "#", mkClasses_ "inline-block py-2 px-4 text-black no-underline"] "Blog"
          li_ $ a_ [href_ "#", mkClasses_ "inline-block py-2 px-4 text-black no-underline"] "Blog"
          li_ $ a_ [href_ "#", mkClasses_ "inline-block py-2 px-4 text-black no-underline"] "Blog"
          li_ $ a_ [href_ "#", mkClasses_ "inline-block py-2 px-4 text-black no-underline"] "Blog"
          li_ $ button_ [mkClasses_ "bg-blue hover:bg-green text-white font-semibold py-2 px-4 border border-gray-400 rounded shadow"] "Button"
-----------
-- PAGES --
-----------

indexHtml :: Html ()
indexHtml = do
  wrapBody navbar
