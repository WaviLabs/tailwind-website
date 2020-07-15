{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- TODO: Write an markdown generator to test blog gen
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
  , xmlns_
  )
import Lucid.Base (HtmlT, Term, makeAttribute, makeElement, makeElementNoEnd, termRaw, with)
import Text.Pandoc (MetaValue(..), Pandoc(..), Inline(..))

import qualified Control.Monad as Monad
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

  let blogPostIDs = map BlogPostId [1..numberOfMdFiles]

  mapM_ genBlogPost blogPostIDs
  mapM_ genBlogCard blogPostIDs

  -- TODO: Implement better
  genBlogPages numberOfMdFiles

  -- Render blog page
  Lucid.renderToFile "../dist/blogT.html" $ blogHtml numberOfMdFiles

  print "DONE RUNNING !!!!!"

----------------
-- COMPONENTS --
----------------

-- | Wraps the HTML in the same ol' same base HTML
--   with all the header stuff.
wrapBody :: Text -> Html () -> Html ()
wrapBody jsFile innerHtml = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      title_ "Wavi Labs LLC"
    body_ innerHtml
    script_ [src_ $ jsFile <> ".js"] ("" :: Text)

polyline_ :: Term arg result => arg -> result
polyline_ = Lucid.term "polyline"

mkClasses_ :: Text -> Attribute
mkClasses_ = classes_ . Text.words

aria_label_ :: Text -> Attribute
aria_label_ = makeAttribute "aria-label"

viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

d_ :: Text -> Attribute
d_ = makeAttribute "d"

fill_ :: Text -> Attribute
fill_ = makeAttribute "fill"

stroke_ :: Text -> Attribute
stroke_ = makeAttribute "stroke"

stroke_width_ :: Text -> Attribute
stroke_width_ = makeAttribute "stroke-width"

stroke_linecap_ :: Text -> Attribute
stroke_linecap_ = makeAttribute "stroke-linecap"

stroke_linejoin_ :: Text -> Attribute
stroke_linejoin_ = makeAttribute "stroke-linejoin"

points_ :: Text -> Attribute
points_ = makeAttribute "points"

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
          svg_ [mkClasses_ "fill-current h-5 w-5", xmlns_ "http://www.w3.org/2000/svg"] $ do
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
  wrapBody "index" $ do
    navbar

---------------
-- BLOG PAGE --
---------------

-- Inserting html with javascript maybe best bet. Doing pagination anyway.
blogHtml:: Int -> Html ()
blogHtml numOfPosts = wrapBody "blog" $ do
  navbar
  blogHeader
  -- The actual cards html will be inserted into the grid
  -- via javascript
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
      div_ [id_ "blog-cards-grid", mkClasses_ "grid grid-cols-1 sm:grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-10"] ""

blogPagination :: Int -> Html ()
blogPagination numberOfPosts =
  div_ [mkClasses_ "flex flex-col items-center my-12"] $
    div_ [mkClasses_ "flex text-gray-700"] $ do
      button_ [id_ "prev-page", mkClasses_ "h-12 w-12 mr-1 flex justify-center items-center rounded-full bg-gray-200"] $
        svg_ [ xmlns_ "http://www.w3.org/2000/svg"
              , width_ "100%"
              , height_ "100%"
              , fill_ "none"
              , viewBox_ "0 0 24 24"
              , stroke_ "currentColor"
              , stroke_width_ "2"
              , stroke_linecap_ "round"
              , stroke_linejoin_ "round"
              , mkClasses_ "feather feather-chevron-left w-6 h-6"
              ] $ polyline_ [points_ "15 18 9 12 15 6"]
        ""
      div_ [id_ "pagination", mkClasses_ "flex h-12 font-medium rounded-full bg-gray-200"] $
        if numberOfPages > 5
        then do
          makeActivePageButton 1
          makePageButtonDots
          makePageButton 2
          makePageButton 3
          makePageButton 4
          makePageButtonDots
          makePageButton numberOfPages
        else makePageButtons numberOfPages
      button_ [id_ "next-page", mkClasses_ "h-12 w-12 mr-1 flex justify-center items-center rounded-full bg-gray-200"] $
        svg_ [xmlns_ "http://www.w3.org/2000/svg"
              , width_ "100%"
              , height_ "100%"
              , fill_ "none"
              , viewBox_ "0 0 24 24"
              , stroke_ "currentColor"
              -- stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-chevron-left w-6 h-6"
              , stroke_width_ "2"
              , stroke_linecap_ "round"
              , stroke_linejoin_ "round"
              , mkClasses_ "feather feather-chevron-left w-6 h-6"
              ] $ polyline_ [points_ "9 18 15 12 9 6"]
        ""
  where
    numberOfPages =
      if numberOfPosts `mod` 12 == 0
      then numberOfPosts `div` 12
      else (numberOfPosts `div` 12) + 1

    makePageButtons :: Int -> Html ()
    makePageButtons pages = do
      makeActivePageButton 1
      if pages > 1
      then mapM_ makePageButton [2..pages]
      else pure ()

    makeActivePageButton :: Int -> Html ()
    makeActivePageButton page = do
      div_
        [ href_ $ "./blog-pages/blog-page-" <> (Text.pack . show $ page) <> ".html"
        , mkClasses_ "w-12 md:flex justify-center items-center hidden  cursor-pointer leading-5 transition duration-150 ease-in  rounded-full bg-blue text-white"
        ]
        (Lucid.toHtml . show $ page)

    makePageButton :: Int -> Html ()
    makePageButton page =
      div_
        [ href_ $ "./blog-pages/blog-page-" <> (Text.pack . show $ page) <> ".html"
        , mkClasses_ "w-12 md:flex justify-center items-center hidden cursor-pointer leading-5 transition duration-150 ease-in rounded-full"
        ]
        (Lucid.toHtml . show $ page)

    makePageButtonDots :: Html ()
    makePageButtonDots =
      div_
        [mkClasses_ "w-12 md:flex justify-center items-center hidden cursor-pointer leading-5 transition duration-150 ease-in rounded-full"]
        "..."

-- TODO: Isomorphic apps with Haskell and PureScript
newtype BlogPostId = BlogPostId { unBlogPostId :: Int }
  deriving (Eq, Show)

-- For generating the actual blog post that will be read.
genBlogPost :: BlogPostId -> IO ()
genBlogPost (BlogPostId bpid) = do
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

  -- One write to blog-posts
  Text.writeFile ("../dist/blog-posts/" <> (show bpid) <> ".html") html

  -- Now we use meta variable defined above to access meta information
  -- for getting tags
  -- let metaMap = Pandoc.unMeta meta
  --     maybeTags = Map.lookup "tags" metaMap
  --     -- Prob better as a maybeTagsText
  --     tagsText = case maybeTags of
  --         Just (MetaList metaValues) -> map changeThisFuncSoon metaValues
  --         Just _ -> error "The tags field was found, but it is not a list as expected."
  --         Nothing -> error "The markdown I parsed didn't have the tags field."

  -- Another write to the corresponding tag folders. A post could be in many folders.
  -- This may not be actually necessary
  -- mapM_
  --   (\tagText ->
  --       Text.writeFile
  --         ("../dist/blog-posts/"
  --           <> (Text.unpack tagText)
  --           <> "/"
  --           <> (show bpid)
  --           <> ".html"
  --         )
  --         html
  --   )
  --   tagsText
  where
    changeThisFuncSoon :: MetaValue -> Text
    changeThisFuncSoon (MetaInlines [Str s]) = s
    changeThisFuncSoon _              = error "These tags aren't text"

    getBlogPostTemplate :: IO (Pandoc.Template Text)
    getBlogPostTemplate = do
      template <- Text.readFile "../pandoc-templates/blogPostTemplate.html" :: IO Text
      template' <- Pandoc.compileTemplate "../pandoc-templates/blogPostTemplate.html" template
      case template' of
        Left _ -> error
          "Pandoc threw an error while trying to parse template for blog post."
        Right template'' -> pure template''

genBlogCard :: BlogPostId -> IO ()
genBlogCard (BlogPostId bpid) = do
  template <- getBlogCardTemplate
  markdown <- Text.readFile ("../blog-md/" <> (show bpid) <> ".md")
  result <- Pandoc.runIO $ do
    ast <- Pandoc.readMarkdown Pandoc.def { Pandoc.readerExtensions = Pandoc.pandocExtensions } markdown
    Pandoc.writeHtml5String Pandoc.def { Pandoc.writerExtensions = Pandoc.pandocExtensions, Pandoc.writerTemplate = Just template, Pandoc.writerHighlightStyle = Just Highlighting.pygments } ast
  html <- Pandoc.handleError result
  Text.writeFile ("../dist/blog-cards/" <> (show bpid) <> ".html") html
  where
    getBlogCardTemplate :: IO (Pandoc.Template Text)
    getBlogCardTemplate = do
      template <- Text.readFile "../pandoc-templates/blogCardTemplate.html" :: IO Text
      template' <- Pandoc.compileTemplate "../pandoc-templates/blogCardTemplate.html" template
      case template' of
        Left _ -> error
          "Pandoc threw an error while trying to parse template for blog post card."
        Right template'' -> pure template''

-- This function aggregates all the cards and groups them on pages
genBlogPages :: Int -> IO ()
genBlogPages numberOfCards = loop 1 numberOfCards
  where
    loop :: Int -> Int -> IO ()
    loop page numberOfCards = do
      if numberOfCards >= 12
      then do
        pageHtml <- mconcatMapM getCardHtmlText [(((page-1) * 12) + 1)..(page * 12)]
        writePageHtmlText page pageHtml
        loop (page + 1) (numberOfCards - 12)
      else do
        pageHtml <- mconcatMapM getCardHtmlText [(((page-1) * 12) + 1)..(((page - 1) * 12) + numberOfCards)]
        writePageHtmlText page pageHtml

    -- | A version of 'mconcatMap' that works with a monadic predicate.
    mconcatMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
    mconcatMapM f = Monad.liftM mconcat . mapM f

    getCardHtmlText :: Int -> IO Text
    getCardHtmlText id =
      Text.readFile $ "../dist/blog-cards/" <> show id <> ".html"

    writePageHtmlText :: Int -> Text -> IO ()
    writePageHtmlText page html =
      Text.writeFile ("../dist/blog-pages/" <> show page <> ".html") html
