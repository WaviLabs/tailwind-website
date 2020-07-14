{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Test.QuickCheck (Arbitrary)
import Data.Text (Text)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Directory as Directory
import qualified Test.QuickCheck as Check


data Markdown = Markdown
  { markdownTitle :: Text
  , markdownDescription :: Text
  , markdownAuthor :: Text
  , markdownDay :: Int
  , markdownMonth :: Int
  , markdownYear :: Int
  , markdownTags :: [Text]
  , markdownContent :: Text
  } deriving (Eq, Show)

instance Arbitrary Markdown where
  arbitrary = do
    let randomText = Text.pack <$> (Check.listOf $ Check.elements "abcdefghijkglmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    markdownTitle <- randomText
    markdownDescription <- randomText
    markdownAuthor <- randomText
    markdownDay <- Check.arbitrarySizedNatural
    markdownMonth <- Check.arbitrarySizedNatural
    markdownYear <- Check.arbitrarySizedNatural
    n <- Check.choose (1, 4)
    markdownTags <- take n <$> Check.listOf randomText
    markdownContent <- randomText

    pure Markdown{..}
{-
---
id: 1
title: Mini Pearl 1
description: This is the first Mini Pearl.
author: Rashad Gover
date: 11/07/20
tags:
  - mini-pearl
  - functional-programming
  - theory
---

# Mini Pearl 1

-}
tShow :: Show a => a -> Text
tShow = Text.pack . show

writeMarkdown :: Int -> Markdown -> IO ()
writeMarkdown id Markdown{..} = do
  let path = "../blog-md/" <> show id <> ".md"

  Text.writeFile path $ "---\n"
  Text.appendFile path $ "id: " <> tShow id <> "\n"
  Text.appendFile path $ "title: " <> markdownTitle <> "\n"
  Text.appendFile path $ "description: " <> markdownDescription <> "\n"
  Text.appendFile path $ "author: " <> markdownAuthor <> "\n"
  Text.appendFile path $
    "date: "
    <> tShow markdownDay
    <> "/"
    <> tShow markdownMonth
    <> "/"
    <> tShow markdownYear
    <> "\n"
  Text.appendFile path $
    "tags:\n" <> (Text.concat $ map (\t -> "  - " <> t <> "\n") markdownTags)
  Text.appendFile path $ "---\n"

main :: IO ()
main = do
  loop 1 66
  where
    loop start end =
      if start == end
      then do
        pure ()
      else do
        testMd <- Check.generate Check.arbitrary :: IO Markdown
        writeMarkdown start testMd
        loop (start+1) end
