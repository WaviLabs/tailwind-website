{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( someFunc
  ) where

import Data.Text (Text)
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
import Lucid.Base (HtmlT, Term, makeAttribute, makeElement, makeElementNoEnd, termRaw, with)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Lucid
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Highlighting as Highlighting

someFunc :: IO ()
someFunc = do
  Lucid.renderToFile "../dist/indexT.html" indexHtml
  Lucid.renderToFile "../dist/blogT.html" $ blogHtml []

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
  wrapBody $ do
    navbar

newtype BlogPostId = BlogPostId { unBlogPostId :: Int }
  deriving (Eq, Show)

-- Isomorphic apps with Haskell and PureScript

genBlogPostHtmlText :: BlogPostId -> IO Text
genBlogPostHtmlText (BlogPostId bpId) = do
  template <- getBlogPostTemplate
  markdown <- Text.readFile ("../blog/" <> (show bpId) <> ".md")
  result <- Pandoc.runIO $ do
    ast <- Pandoc.readMarkdown Pandoc.def { Pandoc.readerExtensions = Pandoc.pandocExtensions } markdown
    Pandoc.writeHtml5String Pandoc.def { Pandoc.writerExtensions = Pandoc.pandocExtensions, Pandoc.writerTemplate = Just template, Pandoc.writerHighlightStyle = Just Highlighting.pygments } ast
  html <- Pandoc.handleError result
  pure html
  where
    getBlogPostTemplate :: IO (Pandoc.Template Text)
    getBlogPostTemplate = do
      template <- Text.readFile "./blogPostTemplate.html" :: IO Text
      template' <- Pandoc.compileTemplate "./blogPostTemplate.html" template
      case template' of
        Left _ -> error "Pandoc threw an error while trying to parse template."
        Right template'' -> pure template''

writeBlogPostHtmlText :: BlogPostId -> Text -> IO ()
writeBlogPostHtmlText (BlogPostId bpId) html = do
  Text.writeFile ("../dist/blog/" <> show bpId <> ".html") html

blogHtml :: [BlogPost] -> Html ()
blogHtml bps = do
  wrapBody $ do
    navbar
    blogHeader
    blogCardsGrid bps
  where
    blogHeader :: Html ()
    blogHeader = undefined

    blogCardsGrid :: [BlogPost] -> Html ()
    blogCardsGrid bps = do
      undefined

    blogPostToCard :: BlogPost -> Html ()
    blogPostToCard BlogPost{..} = do
      undefined

    renderBlogPostTags :: BlogPost -> Html ()
    renderBlogPostTags (BlogPost _ _ _ _ tags) = undefined
