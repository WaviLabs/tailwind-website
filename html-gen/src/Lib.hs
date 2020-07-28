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
  , footer_
  , form_
  , head_
  , hr_
  , html_
  , h1_
  , h2_
  , h3_
  , h5_
  , img_
  , input_
  , label_
  , link_
  , li_
  , nav_
  , script_
  , span_
  , svg_
  , title_
  , ul_
  -- ATTRIBUTES
  , action_
  , alt_
  , charset_
  , class_
  , classes_
  , content_
  , for_
  , height_
  , href_
  , id_
  , lang_
  , meta_
  , method_
  , name_
  , placeholder_
  , rel_
  , src_
  , style_
  , tabindex_
  , target_
  , type_
  , value_
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
      -- <link href="https://fonts.googleapis.com/css2?family=Oswald&family=Recursive&display=swap" rel="stylesheet">
      link_ [href_ "https://fonts.googleapis.com/css2?family=Oswald&family=Recursive&display=swap", rel_ "stylesheet"]
      link_ [href_ "./styles.css", rel_ "stylesheet"]
    body_
      [mkClasses_ "bg-white dark:bg-dark font-sans text-blue leading-normal tracking-normal gradient"]
      innerHtml
    script_ [src_ $ jsFile <> ".js"] ("" :: Text)

polyline_ :: Term arg result => arg -> result
polyline_ = Lucid.term "polyline"

mkClasses_ :: Text -> Attribute
mkClasses_ = classes_ . Text.words

aria_label_ :: Text -> Attribute
aria_label_ = makeAttribute "aria-label"

aria_hidden_ :: Text -> Attribute
aria_hidden_ = makeAttribute "aria-hidden"

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

novalidate_ :: Attribute
novalidate_ = makeAttribute "novalidate" ""

-- | Navbar for the website. This will appear on
-- pretty much every page.
navbar :: Html ()
navbar = do
  nav_ [mkClasses_ "fixed bg-white dark:bg-dark top-0 w-full z-10 p-6 shadow"] $ do
    div_ [mkClasses_ "w-full flex flex-wrap items-center justify-between"] $ do
      -- Contains nav links to other pages
      div_ [mkClasses_ "lg:flex hidden text-lg"] $ do
        span_ [] $ do
          a_
            [href_ "./index.html", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 mr-4 hover:underline"]
            "Home"
          a_
            [href_ "./services.html", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 mr-4 hover:underline"]
            "Services"
          a_
            [href_ "./blogT.html", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 mr-4 hover:underline"]
            "Blog"
          a_
            [href_ "./faqT.html", mkClasses_ "block mt-4 lg:inline-block lg:mt-0 mr-4 hover:underline"]
            "FAQ"
      a_ [href_ "./index.html"] $ do
        -- Big logo for large and above
        div_ [mkClasses_ "lg:flex lg:items-center hidden"] $
          img_ [src_ "logo.png", alt_ "Wavi Labs logo", width_ "300", height_ "300", viewBox_ "0 0 300 300"]
        -- Small for less than large
        div_ [mkClasses_ "flex items-center lg:hidden"] $
          img_ [src_ "logo.png", alt_ "Wavi Labs logo", width_ "200", height_ "200", viewBox_ "0 0 200 200"]
      div_ [mkClasses_ "lg:flex lg:items-center hidden text-lg"] $ do
        label_ [for_ "toggleA", mkClasses_ "flex items-center cursor-pointer"] $ do
          div_ [mkClasses_ "mr-3 text-dark dark:text-white font-medium"] $
            img_ [mkClasses_ "fill-current", src_ "sun.svg", alt_ "sun"]
          div_ [class_ "relative"] $ do
            input_ [id_ "toggleA", type_ "checkbox", class_ "hidden"]
            div_ [mkClasses_ "toggle__line w-10 h-4 bg-gray-400 rounded-full shadow-inner"] ""
            div_ [mkClasses_ "toggle__dot absolute w-6 h-6 bg-white rounded-full shadow inset-y-0 left-0"] ""
          div_ [mkClasses_ "ml-3 text-dark dark:text-green font-medium"] $
            img_ [class_ "fill-current", src_ "moon.svg", alt_ "moon"]
      -- Small hamburger icon for less than large
      div_ [mkClasses_ "block items-center lg:hidden text-lg"] $
        button_ [id_ "nav-toggle", mkClasses_ "flex flex-wrap items-center px-3 py-2 border rounded text-blue border-blue"] $
          svg_ [mkClasses_ "fill-current h-5 w-5", xmlns_ "http://www.w3.org/2000/svg"] $ do
            title_ "Menu"
            path_ [d_ "M0 3h20v2H0V3zm0 6h20v2H0V9zm0 6h20v2H0v-2z"]
      -- Hamburger content
      div_ [id_ "nav-content", mkClasses_ "w-full items-center mt-2 z-20 hidden"] $
        ul_ [mkClasses_ "list-reset flex flex-col justify-end flex-1 items-center"] $ do
          li_ $ a_ [href_ "./index.html", mkClasses_ "inline-block py-2 px-4 no-underline"] "Home"
          li_ $ a_ [href_ "./blogT.html", mkClasses_ "inline-block py-2 px-4 no-underline"] "Blog"
          li_ $ a_ [href_ "./services.html", mkClasses_ "inline-block py-2 px-4 no-underline"] "Services"
          li_ $ a_ [href_ "./faqT.html", mkClasses_ "inline-block py-2 px-4 no-underline"] "FAQ"
          li_ $
            label_ [for_ "toggleB", mkClasses_ "flex items-center cursor-pointer"] $ do
              div_ [mkClasses_ "mr-3 text-dark dark:text-green font-medium"] $
                img_ [class_ "fill-current", src_ "sun.svg", alt_ "sun"]
              div_ [class_ "relative"] $ do
                input_ [id_ "toggleB", type_ "checkbox", class_ "hidden"]
                div_ [mkClasses_ "toggle__line w-10 h-4 bg-gray-400 rounded-full shadow-inner"] ""
                div_ [mkClasses_ "toggle__dot absolute w-6 h-6 bg-white rounded-full shadow inset-y-0 left-0"] ""
              div_ [mkClasses_ "ml-3 text-dark dark:text-green font-medium"] $
                img_ [class_ "fill-current", src_ "moon.svg", alt_ "moon"]

footer :: Html ()
footer =
  footer_ [mkClasses_ "bg-gray-100 dark:bg-dark_alt"] $
    div_ [mkClasses_ "container mx-auto px-6 pt-10 pb-6"] $
      div_ [mkClasses_ "flex flex-wrap"] $ do
        div_ [mkClasses_ "w-full md:w-1/4 text-center md:text-left"] $ do
          h3_ [mkClasses_ "dark:text-green"] "Links"
          ul_ [mkClasses_ "mb-4 text-dark dark:text-blue"] $ do
            li_ [mkClasses_ "mt-2"] $
              a_ [href_ "./faqT.html", mkClasses_ "hover:underline"] "FAQ"
            -- li_ [mkClasses_ "mt-2"] $
            --   a_ [href_ "#", mkClasses_ "hover:underline"] "Help"
            -- li_ [mkClasses_ "mt-2"] $
            --   a_ [href_ "#", mkClasses_ "hover:underline"] "Support"
        div_ [mkClasses_ "w-full md:w-1/4 text-center md:text-left"] $ do
          h3_ [mkClasses_ "dark:text-green"] "Legal"
          ul_ [mkClasses_ "mb-4 text-dark dark:text-blue"] $ do
            -- li_ [mkClasses_ "mt-2"] $
            --   a_ [href_ "#", mkClasses_ "hover:underline"] "Contract"
            li_ [mkClasses_ "mt-2"] $
              a_ [href_ "#", mkClasses_ "hover:underline"] "License"
        div_ [mkClasses_ "w-full md:w-1/4 text-center md:text-left"] $ do
          h3_ [mkClasses_ "dark:text-green"] "Social"
          ul_ [mkClasses_ "mb-4 text-dark dark:text-blue"] $ do
            li_ [mkClasses_ "mt-2"] $
              a_ [href_ "https://github.com/wavi-labs", mkClasses_ "hover:underline"] "GitHub"
            li_ [mkClasses_ "mt-2"] $
              a_ [href_ "https://www.instagram.com/wavi_labs", mkClasses_ "hover:underline"] "Instagram"
            li_ [mkClasses_ "mt-2"] $
              a_ [href_ "https://twitter.com/wavi_labs", mkClasses_ "hover:underline"] "Twitter"
        div_ [mkClasses_ "w-full md:w-1/4 text-center md:text-left"] $ do
          h3_ [mkClasses_ "dark:text-green"] "Company"
          ul_ [mkClasses_ "mb-4 text-dark dark:text-blue"] $ do
            li_ [mkClasses_ "mt-2"] $
              a_ [href_ "./blogT.html", mkClasses_ "hover:underline"] "Official Blog"
            li_ [mkClasses_ "mt-2"] $
              a_ [href_ "./index.html", mkClasses_ "hover:underline"] "About Us"
            li_ [mkClasses_ "mt-2"] $
              a_ [href_ "mailto:wavi.labs@gmail.com", mkClasses_ "hover:underline"] "Contact"

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
    div_ [mkClasses_ "bg-blue dark:bg-dark", style_ "height: 1000px;"] ("" :: Html ())
    footer

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
  blogMailchimpForm
  footer
  where
    blogHeader :: Html ()
    blogHeader =
      div_ [mkClasses_ "container md:flex md:justify-between align-middle mx-auto pt-40 pb-10"] $ do
        h2_ "Wavi Labs Archive"
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

    blogMailchimpForm :: Html ()
    blogMailchimpForm = do
      div_ [id_ "mc_embed_signup", mkClasses_ "container mx-auto px-4"] $ do
        form_
          [ action_ "https://wavi-labs.us17.list-manage.com/subscribe/post?u=e4b25a75af7f665ebcdf444d9&amp;id=991e220301"
          , method_ "post"
          , id_ "mc-embedded-subscribe-form"
          , name_ "mc-embedded-subscribe-form"
          , class_ "validate"
          , target_ "_blank"
          , novalidate_
          ] $ do
            div_ [id_ "mc_embed_signup_scroll", mkClasses_ "p-4 text-center"] $ do
              h1_ "Subscribe for updates and more!"
              div_ [mkClasses_ "mc-field-group w-full text-center pt-4"] $
                div_ [mkClasses_ "max-w-xl mx-auto p-1 pr-0 flex flex-wrap items-center"] $ do
                  label_
                    [ for_ "mce-EMAIL"
                    , mkClasses_ "flex-1 mt-4 block md:inline-block appearance-none"
                    ] $ h3_ "Email Address:"
                  input_
                    [ type_ "email"
                    , value_ ""
                    , name_ "EMAIL"
                    , mkClasses_ "required email flex-1 mt-4 appearance-none border border-gray-400 rounded shadow-md p-3 text-gray-600 mr-2 focus:outline-none"
                    , id_ "mce-EMAIL"
                    ]
              div_ [id_ "mce-responses", class_ "clear"] $ do
                div_ [class_ "response", id_ "mce-error-response", style_ "display:none"] ""
                div_ [class_ "response", id_ "mce-success-response", style_ "display:none"] ""
              div_
                [ style_ "position: absolute; left: -5000px;"
                , aria_hidden_ "true"
                ] $ input_ [type_ "text", name_ "b_e4b25a75af7f665ebcdf444d9_991e220301", tabindex_ "-1", value_ ""]
              div_ [mkClasses_ "clear mt-4"] $
                input_
                  [ type_ "submit"
                  , value_ "Catch The Wave"
                  , name_ "subscribe"
                  , id_ "mc-embedded-subscribe"
                  , mkClasses_ "btn text-blue hover:text-white dark-hover:text-dark text-4xl mt-8 mb-16"
                  ]

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
      div_ [id_ "pagination", mkClasses_ "flex h-12 font-medium rounded-full bg-gray-200"] $ do
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
      div_ [id_ "small-pagination", mkClasses_ "md:hidden flex h-12 font-medium rounded-full bg-gray-200"] $
        div_ [mkClasses_ "w-12 h-12 md:hidden flex justify-center items-center cursor-pointer leading-5 transition duration-150 ease-in rounded-full bg-green text-white"] "1"
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
