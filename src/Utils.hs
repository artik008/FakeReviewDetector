module Utils where

import           Data.ByteString         (breakSubstring)
import           Data.Char               (isDigit)
import           Data.String.Conversions
import           Data.Time
import           Text.HTML.TagSoup

import           Models

getRTList :: ReviewTag -> [ReviewTag]
getRTList (ReviewTagList _ _ l) = l
getRTList _                     = []

getRTAttrib :: String -> ReviewTag -> String
getRTAttrib attr (ReviewTagList _ attrT _) = checkAttr attr attrT
getRTAttrib attr (ReviewTagOpen _ attrT)   = checkAttr attr attrT
getRTAttrib _ _                            = ""

checkAttr :: String -> [Attribute String] -> String
checkAttr a1 a2 = case filter (\(x,_) -> x == a1) a2 of
  []        -> ""
  ((_,v):_) -> v



-- | Substrings
findBySubstring :: String -> [String] -> Maybe String
findBySubstring substr urls =
  if right /= [] then Just (head right) else Nothing
  where
    right = filter (isSubstring substr) urls

isSubstring :: String -> String -> Bool
isSubstring sub str = snd (breakSubstring (cs sub) (cs str)) /= ""


toUTCTime :: String -> IO UTCTime
toUTCTime t = do
  nowDay <- utctDay <$> getCurrentTime
  let (nowyear,_,_) = toGregorian nowDay
      day =
        if isSubstring "сегодня" t
        then nowDay
        else
          if isSubstring "вчера" t
          then addDays (-1) nowDay
          else parseDate nowyear dayList
      time = case (span (/=':')) <$> findBySubstring ":" timeList of
        Just (h, (':':m)) -> (toDiffTime h)*3600 + (toDiffTime m)*60
        _                 -> 0
  return $ UTCTime day time
  where
    timeList = words t
    dayList = filter (not.(isSubstring ":")) timeList
    parseDate ny dL =
      fromGregorian (year ny dL) (getMonth $ dL!!1) (read $ head dL)
    year ny dL = if length dL == 3 then read $ dL!!2 else ny
    toDiffTime = secondsToDiffTime.read


urlToName :: URL -> FilePath
urlToName url =
  reverse $ takeWhile (/='/') $ drop 1 $ dropWhile (/='.') $ reverse url

getMonth :: String -> Int
getMonth s = case s of
  "янв" -> 1
  "фев" -> 2
  "мар" -> 3
  "апр" -> 4
  "май" -> 5
  "июн" -> 6
  "июл" -> 7
  "авг" -> 8
  "сен" -> 9
  "окт" -> 10
  "ноя" -> 11
  "дек" -> 12
  _     -> 1


getNumbers :: String -> Int
getNumbers list = read [x | x <- list, isDigit x] :: Int

fI :: Integral a => a -> Float
fI = fromIntegral

