module Request where

import           Control.Concurrent
import           Data.List          (find)
import           Data.Maybe
import           Data.Time
import           Network.Curl
import           System.Directory
import qualified System.IO.Strict   as S
import           Text.HTML.TagSoup
import           Text.Read
import           Text.StringLike

import           Models
import           Utils

parseWebPage :: URL -> IO [Tag String]
parseWebPage url = do
  headers <- read <$> readFile "headers" :: IO [String]
  let codedURL = url
  putStrLn codedURL
  response <- infiniteResponse codedURL (defaultOptions headers)
  return $ parseTags response :: IO [Tag String]

infiniteResponse :: URL -> [CurlOption] -> IO String
infiniteResponse url headers = do
  threadDelay 3000000
  (code, response) <- curlGetString url headers
  case code of
    CurlOK -> do
      writeFile "result" $ show response
      return response
    _ -> do
      putStrLn $ show code
      _ <- getLine
      infiniteResponse url headers



parseReviewStructure :: Bool -> URL -> IO Review
parseReviewStructure rvType url = do
  isReviewScaned <- findReview (urlToName url) rvType
  case isReviewScaned of
    Just r -> return r
    Nothing -> do
      tagList <- parseWebPage url
      reviewAuthorLink <- return $ getUrlFromReview tagList "profile"
      author <-
        if rvType
          then Just <$> getReviewAuthor reviewAuthorLink
          else return Nothing
      rvProduct <- getReviewProduct (getUrlFromReview tagList "reviews")
      let review = Review {
           reviewURL          = getUrlFromReview tagList "review_"
         , reviewText         = getReviewText tagList
         , reviewEval         = fromJust $ getReviewEval tagList
         , reviewRecom        = fromJust $ getReviewRecom tagList
         , reviewCommentCount = fromJust $ getReviewCommentCount tagList
         , reviewAuthor       = author
         , reviewAuthorURL    = reviewAuthorLink
         , reviewProduct      = rvProduct
        }
      if rvType
        then writeFile ("full_reviews/" <> urlToName url) $ show review
        else writeFile ("short_reviews/" <> urlToName url) $ show review
      return review

findReview :: FilePath -> Bool -> IO (Maybe Review)
findReview fname rvType = do
  isExistF <- doesFileExist ("full_reviews/" <> fname)
  isExistS <- doesFileExist ("short_reviews/" <> fname)
  if (rvType && isExistF) || ((not rvType) && isExistS)
    then do
      r <- readMaybe <$> readFile full_name
      return r
    else if (not rvType) && isExistF
         then do
           mr <- readMaybe <$> readFile ("full_reviews/" <> fname)
           case mr of
             Just r  -> return $ Just r { reviewAuthor = Nothing }
             Nothing -> return Nothing
         else return Nothing
  where
    full_name =
      if rvType
        then "full_reviews/" <> fname
        else "short_reviews/" <> fname

-- | Get fields from review page

-- * URLs
getUrlFromReview :: [Tag String] -> String -> String
getUrlFromReview tags str = fromJust $ findBySubstring str $
  map (castString.(fromAttrib "content").head)
    (sections (findOpenTagByAttrib "meta" ("itemprop","url")) tags)


checkURLTag :: Tag String -> Bool
checkURLTag = \x -> case x of
  TagOpen "meta" t -> elem ("itemprop","url") t
  _                -> False

-- * Review's text
getReviewText :: [Tag String] -> String
getReviewText = innerText.(intoTag "div" ("class","review-body description"))

-- * Review Evaluation
getReviewEval :: [Tag String] -> Maybe Rate
getReviewEval tags = read <$> fromAttrib "title" <$>
  find (findOpenTagByAttrib "abbr" ("class","rating")) tags

-- * Review Recomendations
getReviewRecom :: [Tag String] -> Maybe Int
getReviewRecom tags = getNumbers <$> fromAttrib "title" <$>
  find (findOpenTagByAttrib "span" ("class","review-btn review-yes"))
    tags

-- * Review Commentaries count
getReviewCommentCount :: [Tag String] -> Maybe Int
getReviewCommentCount tags = getNumbers <$> fromAttrib "title" <$>
  find
    (findOpenTagByAttrib
      "a"
      ("class","review-btn review-comments tooltip-left hover-brace")
    )
    tags


-- | Get fields from author page


-- * Author
getReviewAuthor :: URL -> IO Author
getReviewAuthor url = do
  tagList <- parseWebPage url
  aRegTime <- getAuthorRegistration tagList
  aLastActiveTime <- getAuthorLastActive tagList
  let authorReviewsURL =
        "/?search_text=" <>
        (reverse $ takeWhile (/='/') $ reverse url) <>
        "&us=1"
  authorReviewsTagList <-
    parseWebPage $ "https://otzovik.com" <> authorReviewsURL
  reviews <- getAthorReviews authorReviewsTagList authorReviewsURL
  karma <- getAuthorKarma url
  return $ Author {
    authorURL          = url
  , authorReviews      = reviews
  , authorReputation   = getAuthorReputation tagList
  , authorKarma        = karma
  , authorRegistration = aRegTime
  , authorLastActive   = aLastActiveTime
  , authorCommentCount = getAuthorCommentCount tagList
  , authorSubscr       = getAuthorSubscr tagList
  }


-- * Author reviews
getAthorReviews :: [Tag String] -> URL -> IO [Review]
getAthorReviews tagList url = do
  tagLists <-
    if pages tagList == 1
      then (return [])
      else sequence $
        map
          (\x -> parseWebPage $
                   "https://otzovik.com" <> url <> "&page=" <> show x
          )
          [2..(pages tagList)]
  getAuthorsReviewsFromPage (concat $ tagList:tagLists)
  where
    pages t = case findLastPage of
      Just p  -> p
      Nothing -> 1 + (length $
        filter
          (findOpenTagByAttrib "a" ("class","pager-item nth"))
          (intoTag "div" ("class","pager") t)
        )
    findLastPage = getNumbers <$> fromAttrib "title" <$>
      find
        (findOpenTagByAttrib "a" ("class","pager-item last tooltip-top"))
        tagList

getAuthorsReviewsFromPage :: [Tag String] -> IO [Review]
getAuthorsReviewsFromPage tags = do
  let reviewURLs = map (\x -> fromAttrib "href" x) $
        filter
          (findOpenTagByAttrib "a" ("class","review-btn review-read-link"))
          tags
  sequence $ map (parseReviewStructure False) reviewURLs


-- * Author reputation
getAuthorReputation :: [Tag String] -> Int
getAuthorReputation tags = read $
  innerText $
    takeWhile (not.(isTagCloseName "div")) $
      dropWhile (not.onlyKarma) tags :: Int
  where
    onlyKarma t = or $ map (\x -> isKarma x t) ["-1", "0", "1"]
    isKarma s = findOpenTagByAttrib "div" ("class","karma"<>s)

-- * Author registration
getAuthorRegistration :: [Tag String] -> IO UTCTime
getAuthorRegistration = getAuthorTime "Регистрация:"

-- * Author last activation
getAuthorLastActive :: [Tag String] -> IO UTCTime
getAuthorLastActive = getAuthorTime "Активность:"

-- * Author time
getAuthorTime :: String -> [Tag String] -> IO UTCTime
getAuthorTime field = toUTCTime.(findFieldInTable "table_1" field)

-- * Author Commentaries count
getAuthorCommentCount :: [Tag String] -> Int
getAuthorCommentCount tags = fromMaybe 0 $ readMaybe $ innerText $
      intoTag "a" ("title","Прочитать комментарии автора") tags

-- * Author subscribers
getAuthorSubscr :: [Tag String] -> Int
getAuthorSubscr = read.(findFieldInTable "table_2" "Подписчиков:")

-- * Author karma
getAuthorKarma :: URL -> IO Karma
getAuthorKarma url = do
  fauthorsRating <- lines <$> S.readFile "authors_rating"
  let authorsRating = map (\x -> read x :: (URL, Karma)) fauthorsRating
  return $ fromMaybe Unknown $ lookup url authorsRating



-- | Get fields from product page


-- * Product
getReviewProduct :: URL -> IO Product
getReviewProduct url = do
  rates <- getProductAuthRep url
  return $ Product {
    productURL     = url
  , productAuthRep = rates
  }

-- * Product authors' reputation
getProductAuthRep :: URL -> IO [(Rate, Karma)]
getProductAuthRep url = do
  tagList <- parseWebPage url
  tagLists <-
    if pages tagList == 1
      then (return [])
      else sequence $
        map (\x -> parseWebPage (url <> show x <> "/")) [2..(pages tagList)]
  sequence $ map
    (\(link, rate) -> do
      karma <- getAuthorKarma ("https://otzovik.com" <> link)
      return (rate, karma)
    )
    (concat $ map getAuthorsFromProductPage (tagList:tagLists))
  where
    pages t = 1 + (length $ filter
      (findOpenTagByAttrib "a" ("class","pager-item nth"))
      (intoTag "div" ("class","pager") t))

-- * Product authors' rates
getAuthorsFromProductPage :: [Tag String] -> [(URL, Rate)]
getAuthorsFromProductPage tags =
  zip
    (map (getRTAttrib "href") $ catMaybes $
      map (findReviewTag (ReviewTagOpen "a" [("class","user-login")])) l)
    (map (getNumbers.(getRTAttrib "title")) $ catMaybes $ map
      (
        findReviewTag
        (
          ReviewTagList
            "div"
            [("class","product-rating tooltip-right")]
            []
        )
      )
      l)
  where
    l = map getRTList $ getRTList
      (
        toReviewTagByName
          "div"
          ("class","review-list-2 review-list-chunk")
          tags
      )


-- | Fing tag
findOpenTagByAttrib
  :: String
  -> Attribute String
  -> Tag String
  -> Bool
findOpenTagByAttrib tagName tagAttr tag = case tag of
  TagOpen tName t -> elem tagAttr t && tagName == tName
  _               -> False

intoTag :: String -> (String, String) -> [Tag String] -> [Tag String]
intoTag tag attrib tags = takeWhile (not.(isTagCloseName tag)) $
  dropWhile (not.(findOpenTagByAttrib tag attrib)) tags

toReviewTagByName :: String -> Attribute String -> [Tag String] -> ReviewTag
toReviewTagByName tName attrib tags =
  ReviewTagList tName [attrib] $ toReviewTag tName 1 bs
  where
    (_:bs) = dropWhile (not.(findOpenTagByAttrib tName attrib)) tags

toReviewTag :: String -> Int -> [Tag String] -> [ReviewTag]
toReviewTag _ _ [] = []
toReviewTag tName 1 (t:tags) = case t of
  TagOpen "div" attrib ->
    (
      ReviewTagList "div" attrib (toReviewTag "div" 1 tags)
    :
      toReviewTag tName 2 tags
    )
  TagOpen newName attrib ->
    (ReviewTagOpen newName attrib: toReviewTag tName 1 tags)
  TagClose newName  ->
    if tName == newName
      then []
      else (ReviewTagClose newName: toReviewTag tName 1 tags)

  TagText str       -> (ReviewTagSimple str: toReviewTag tName 1 tags)
  TagComment str    -> (ReviewTagSimple str: toReviewTag tName 1 tags)
  TagWarning str    -> (ReviewTagSimple str: toReviewTag tName 1 tags)
  TagPosition r c   ->
    (
      ReviewTagSimple  ("position" <> show r <> "_" <> show c)
    :
      toReviewTag tName 1 tags
    )
toReviewTag tName i (t:tags) = case t of
  TagOpen "div" _ -> toReviewTag tName (i+1) tags
  TagClose "div"  -> toReviewTag tName (i-1) tags
  _               -> toReviewTag tName i tags


findReviewTag :: ReviewTag -> [ReviewTag] -> Maybe ReviewTag
findReviewTag _ [] = Nothing
findReviewTag rtag (t:ts) =
  if eqRT rtag t
    then Just t
    else case t of
      ReviewTagList _ _ l -> listToMaybe $
        catMaybes [findReviewTag rtag l, findReviewTag rtag ts]
      _ -> findReviewTag rtag ts
  where
    eqRT (ReviewTagList n1 a1 _) (ReviewTagList n2 a2 _) =
      n1 == n2 && (and $ map (\x -> elem x a2) a1)
    eqRT (ReviewTagOpen n1 a1) (ReviewTagOpen n2 a2) =
      n1 == n2 && (and $ map (\x -> elem x a2) a1)
    eqRT t1 t2 = t1 == t2



findFieldInTable :: String -> String -> [Tag String] -> String
findFieldInTable tName field tags =
  (dropWhile (\x -> (rmSpaces x)/=field) tableText)!!1
  where
    tableText = filter (\x -> rmSpaces x/=[]) $
      lines $ innerText $ intoTag "table" ("class",tName) tags
    rmSpaces = filter (/=' ')




defaultOptions :: [String] -> [CurlOption]
defaultOptions = \x -> [CurlHttpHeaders x]
