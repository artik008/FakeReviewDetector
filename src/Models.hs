{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Models where

import           Data.Aeson        hiding (defaultOptions)
import           Data.Time
import           GHC.Generics      (Generic)
import           Text.HTML.TagSoup


data ReviewTag =
    ReviewTagList String [Attribute String] [ReviewTag]
  | ReviewTagOpen String [Attribute String]
  | ReviewTagClose String
  | ReviewTagSimple String
  deriving (Show, Eq, Read, ToJSON, FromJSON, Generic)


type URL = String
type Rate = Int
type Persons = Int

data Karma =
    Karma Float
  | Unknown
  deriving (Show, Eq, Read, ToJSON, FromJSON, Generic)

data Review = Review 
  { reviewURL          :: URL
  , reviewText         :: String
  , reviewEval         :: Rate
  , reviewRecom        :: Persons
  , reviewCommentCount :: Persons
  , reviewAuthor       :: Maybe Author
  , reviewAuthorURL    :: URL
  , reviewProduct      :: Product
  } deriving (Show, Eq, Read, ToJSON, FromJSON, Generic)

data Author = Author 
  { authorURL          :: URL
  , authorReviews      :: [Review]
  , authorReputation   :: Int
  , authorKarma        :: Karma
  , authorRegistration :: UTCTime
  , authorLastActive   :: UTCTime
  , authorCommentCount :: Int
  , authorSubscr       :: Persons
  } deriving (Show, Eq, Read, ToJSON, FromJSON, Generic)

data Product = Product 
  { productURL     :: URL
  , productAuthRep :: [(Rate, Karma)]
  } deriving (Show, Eq, Read, ToJSON, FromJSON, Generic)


data ProcessedTextInfo = ProcessedTextInfo 
  { ptAvgWordsInSent :: Float
  , ptWordCount :: Float
  , ptAvgWordLength :: Float
  , ptPunctuation :: [(String, Int)]
  , ptSpeechParts :: [(String, Int)]  
  } deriving (Show, Eq, Read, ToJSON, FromJSON, Generic)
