{-# LANGUAGE RecordWildCards #-}
module Detector where

import           Data.Maybe
import           Data.Time
import qualified System.IO.Strict as S
import           Text.Show.Pretty (ppShow)
import           Text.Read        (readEither)
import           Data.String.Conversions

import qualified Turtle as T

import           Models
import           Request
import           Utils
import           AuthorModel
import           TextModel


countWithTeacher :: FilePath -> FilePath  -> IO ()
countWithTeacher teacher tester = do
  res <- teach teacher
  putStrLn $ show res
  countNew tester res


countNew :: FilePath -> ([Float], [Float]) -> IO ()
countNew testpath borders = do
  putStrLn "Count:"
  testers <- lines <$> readFile testpath
  mapM_ (\x -> countOneNew x borders) testers  


countOneNew :: URL -> ([Float], [Float]) -> IO ()
countOneNew url (borderBad, borderGood) = do 
  (_, karma, res) <- countAuthorKarma url
  let badDist  = countDist borderBad res
      goodDist = countDist borderGood res
  printRes karma $ if badDist > goodDist then "не ложный" else "ложный"
  where
    countDist b1 b2 = sqrt $ sum $ zipWith (\x y -> (x - y)^2) b1 b2 
    printRes k r = putStrLn $
                   "Результат: URL = " <> url <>
                   ", Карма автора: " <> (show k) <>
                   ", Отзыв " <> r



teach :: FilePath -> IO ([Float], [Float])
teach fpath = do
  putStrLn "Train:"
  teachers <- map words <$> lines <$> readFile fpath
  res <- sequence $ map countAuthorKarma (map head teachers)
  putStrLn $ ppShow res
  return $ p (\t -> countCenter t teachers res) "0" "1"
  where
    p f x y = (f x, f y) 

countCenter :: String -> [[String]] -> [(URL, Karma, [Float])] -> [Float]
countCenter t teachers res = 
  map 
    (\x -> x/(fromIntegral ltt)) $
    foldl 
      (\x y -> zipWith (+) x y) 
      (head tRes) 
      (tail tRes)
  where
    tTeachers = map (\x -> x!!0) $ filter (\x -> x!!1 == t) teachers
    tRes = map (\(_, _, x) -> x) $ filter (\x -> elem (fst3 x) tTeachers) res
    fst3 (x, _, _) = x
    ltt = length tTeachers


test :: IO ()
test = do
  urls <- lines <$> readFile "test_urls"
  res <- sequence $ map countAuthorKarma urls
  putStrLn $ ppShow res

countAuthorKarma :: URL -> IO (URL, Karma, [Float])
countAuthorKarma url = do
  r@Review{..} <- parseReviewStructure True url
  (_, resTest) <-
    T.procStrict "./processText.py" [T.fromString reviewText] T.stdin
  now <- getCurrentTime
  let Author{..} = fromJust $ reviewAuthor
      eptInfo  = readEither (cs resTest) :: Either String ProcessedTextInfo
      valList = [
          countValReviewCount
            (length authorReviews)
            authorRegistration
            authorLastActive
        , countValActive authorRegistration authorLastActive now
        , countValReputation authorReputation
        , countValSocial authorCommentCount authorSubscr
        , countValReviewsLength authorReviews
        , countValAvgRate authorReviews
        , countValProductRate
            reviewEval
            (productAuthRep reviewProduct)
        ]
        ++
        case eptInfo of
          Right ptInfo -> textKarmaAnalyze reviewText ptInfo
          Left _err -> []
      result = (
                  reviewAuthorURL
                ,
                  Karma $ 1 - (sum valList)/(fI $ length valList)
                )
      Just rA = reviewAuthor
  writeFile
    ("full_reviews/" <> urlToName url) $
    show $ r{reviewAuthor = Just $ rA{authorKarma = snd result}}
  putStrLn $ show (url, snd result, valList)
  addToAuthorsList result
  return (url, snd result, valList)


addToAuthorsList :: (URL, Karma) -> IO ()
addToAuthorsList res = do
  fauthorsRating <- lines <$> S.readFile "authors_rating"
  let authorsRating = map (\x -> read x :: (URL, Karma)) fauthorsRating
      newRating = (res:filter (\x -> fst x /= fst res) authorsRating)
  writeFile "authors_rating" $ unlines $ map show newRating








