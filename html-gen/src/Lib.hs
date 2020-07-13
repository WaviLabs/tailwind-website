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
  , input_
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
  , placeholder_
  , src_
  , type_
  , width_
  )
import Lucid.Base (HtmlT, Term, makeAttribute, makeElement, makeElementNoEnd, termRaw, with)
import Text.Pandoc (MetaValue(..), Pandoc(..))

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Lucid
import qualified System.Directory as Directory
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Highlighting as Highlighting

someFunc :: IO ()
someFunc = do
  -- Render index page
  Lucid.renderToFile "../dist/indexT.html" indexHtml

  -- Number of posts is important for pagination
  numberOfMdFiles <- length <$> Directory.listDirectory "../blog-md/"
  -- Render blog page
  Lucid.renderToFile "../dist/blogT.html" $ blogHtml numberOfMdFiles


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

aria_label_ :: Text -> Attribute
aria_label_ = makeAttribute "aria-label"

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

----------------
-- INDEX PAGE --
----------------
indexHtml :: Html ()
indexHtml = do
  wrapBody $ do
    navbar

---------------
-- BLOG PAGE --
---------------

-- Generates the blog post page. Takes blog post ids; reads the corresponding
-- markdown and generates blog post cards and the blog posts. Because we are using templates
-- to generate cards and they are inside the html we have to split the blog card grid
-- container into top and bottom and place the text generated from the card html text generator
-- between. So blogHtml is going to be text instead of Html ().
-- Inserting html with javascript maybe best bet. Doing pagination anyway.
blogHtml:: Int -> Html ()
blogHtml numOfPosts = wrapBody $ do
  navbar
  blogHeader
  blogCardsGrid
  blogPagination numOfPosts
  -- TODO: footer
  where
    blogHeader :: Html ()
    blogHeader =
      div_ [mkClasses_ "container md:flex md:justify-between align-middle mx-auto pt-40 pb-10"] $ do
        h1_ [mkClasses_ "text-4xl text-blue font-bold"] "Wavi Labs Archive"
        div_ [mkClasses_ "flex items-center border-b border-teal-500 py-2"] $ do
          input_
            [ mkClasses_ "appearance-none bg-transparent border-none w-full text-gray-700 mr-3 py-1 px-2 leading-tight focus:outline-none"
            , type_ "text"
            , placeholder_ "Search"
            , aria_label_ "Search"
            ]
          button_
            [ mkClasses_ "flex-shrink-0 border-transparent border-4 text-teal-500 hover:text-teal-800 text-sm py-1 px-2 rounded"
            , type_ "button"
            ]
            "Search"

    blogCardsGrid :: Html ()
    blogCardsGrid = div_ [mkClasses_ "container flex px-auto mx-auto justify-center"] $
      div_ [mkClasses_ "grid grid-cols-1 sm:grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-10"] ""

    blogPagination :: Int -> Html ()
    blogPagination numOfPosts = undefined
      where
        numOfPages =
          if numOfPosts `mod` 12 == 0
          then numOfPosts `div` 12
          else (numOfPosts `div` 12) + 1

-- TODO: Isomorphic apps with Haskell and PureScript
newtype BlogPostId = BlogPostId { unBlogPostId :: Int }
  deriving (Eq, Show)

-- For generating the actual blog post that will be read.
genBlogPostHtmlText :: BlogPostId -> IO ()
genBlogPostHtmlText (BlogPostId bpid) = do
  -- Get markdown
  markdown <- Text.readFile ("../blog-md/" <> (show bpid) <> ".md")
  -- We want the meta data
  pandoc@(Pandoc meta _) <- do
    eitherPandoc <- Pandoc.runIO $
      Pandoc.readMarkdown
      Pandoc.def
      { Pandoc.readerExtensions = Pandoc.pandocExtensions }
      markdown
    Pandoc.handleError eitherPandoc
  -- Get the template for blog posts
  -- It uses meta variables in markdown block at top
  template <- getBlogPostTemplate

  -- Transform pandoc AST into HTML5 text
  eitherHtml <- Pandoc.runIO $ Pandoc.writeHtml5String
    Pandoc.def
    { Pandoc.writerExtensions = Pandoc.pandocExtensions
    , Pandoc.writerTemplate = Just template
    , Pandoc.writerHighlightStyle = Just Highlighting.pygments
    }
    pandoc

  -- Handle possible error
  html <- Pandoc.handleError eitherHtml

  -- One write to blog-pages
  Text.writeFile ("../dist/blog-pages/" <> (show bpid) <> ".html") html

  -- Now we use meta variable defined above to access meta information
  -- for getting tags
  let metaMap = Pandoc.unMeta meta
      maybeTags = Map.lookup "tags" metaMap
      -- Prob better as a maybeTagsText
      tagsText = case maybeTags of
          Just (MetaList metaValues) -> map (\(MetaString s) -> s) metaValues
          Just _ -> error "The tags field was found, but it is not a list as expected."
          Nothing -> error "The markdown I parsed didn't have the tags field."

  -- Another write to the corresponding tag folders. A post could be in many folders.
  mapM_
    (\tagText ->
        Text.writeFile
          ("../dist/blog-pages/"
            <> (Text.unpack tagText)
            <> "/"
            <> (show bpid)
            <> ".html"
          )
          html
    )
    tagsText
  where
    getBlogPostTemplate :: IO (Pandoc.Template Text)
    getBlogPostTemplate = do
      template <- Text.readFile "../dist/blog-html/blogPostTemplate.html" :: IO Text
      template' <- Pandoc.compileTemplate "../dist/blog-html/blogPostTemplate.html" template
      case template' of
        Left _ -> error
          "Pandoc threw an error while trying to parse template for blog post."
        Right template'' -> pure template''

genBlogPostHtmlCard :: BlogPostId -> IO ()
genBlogPostHtmlCard (BlogPostId bpid) = do
  template <- getBlogPostCardTemplate
  markdown <- Text.readFile ("../blog-md/" <> (show bpid) <> ".md")
  result <- Pandoc.runIO $ do
    ast <- Pandoc.readMarkdown Pandoc.def { Pandoc.readerExtensions = Pandoc.pandocExtensions } markdown
    Pandoc.writeHtml5String Pandoc.def { Pandoc.writerExtensions = Pandoc.pandocExtensions, Pandoc.writerTemplate = Just template, Pandoc.writerHighlightStyle = Just Highlighting.pygments } ast
  html <- Pandoc.handleError result
  Text.writeFile ("../dist/blog-cards/" <> (show bpid) <> ".html") html
  where
    getBlogPostCardTemplate :: IO (Pandoc.Template Text)
    getBlogPostCardTemplate = do
      template <- Text.readFile "../pandoc-templates/blogPostCardTemplate.html" :: IO Text
      template' <- Pandoc.compileTemplate "...pandoc-templates/blogPostCardTemplate.html" template
      case template' of
        Left _ -> error
          "Pandoc threw an error while trying to parse template for blog post card."
        Right template'' -> pure template''