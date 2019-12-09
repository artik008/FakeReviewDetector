{-# LANGUAGE RecordWildCards #-}
module AuthorModel where

import           Data.Time

import           Models
import           Utils


countValReviewCount :: Int -> UTCTime -> UTCTime -> Float
countValReviewCount revs start end = if reviewFreq > 1 then 1.0 else reviewFreq
  where
    authorWeeks = (diffDays (utctDay end) (utctDay start)) `div` 7
    reviewFreq  = (fI authorWeeks)/(fI revs) -- 1/(revs/weeks)

countValActive :: UTCTime -> UTCTime -> UTCTime -> Float
countValActive reg lastAct now =
  firstVal * ((fI activeDays)/(fI authorDays))
  where
    authorDays = diffDays (utctDay now) (utctDay reg)
    activeDays = diffDays (utctDay lastAct) (utctDay reg)
    firstVal =
      if authorDays >= 90
        then 1.0
        else (fI authorDays)/90.0


countValReputation :: Int -> Float
countValReputation rep
  | rep <= 0 = 0.0
  | rep < 1000 = 0.5 + (fI rep)/2000
  | otherwise = 0.5


countValSocial :: Int -> Persons -> Float
countValSocial comments subscr = (commVal + subVal)/2
  where
    commVal = countWithCheckZero comments
    subVal  = countWithCheckZero subscr
    countWithCheckZero x = if x == 0 then 0.5 else 1 - 1/(fI x)


countValReviewsLength :: [Review] -> Float
countValReviewsLength arevs =
  if avgReviewLen > 200
    then 0.5 + (100/avgReviewLen)
    else 0.5 * (avgReviewLen/200)
  where
    avgReviewLen =
      (fI (sum $ map (length.words.reviewText) arevs))
      /
      (fI (length arevs))


countValAvgRate :: [Review] -> Float
countValAvgRate arevs = 1.0 - (modAvgRate/2)*(1.0 - 1/(fI $ length arevs))
  where
    avgRate = (fI (sum $ map reviewEval arevs))
      /
      (fI (length arevs))
    modAvgRate =
      if avgRate > 3
        then avgRate - 3
        else 3 - avgRate


countValProductRate :: Rate -> [(Rate, Karma)] -> Float
countValProductRate _ [_] = 0.0
countValProductRate rate productRates =
  if length goodRates > 0
  then 1 - (abs (fI rate - avgRate))/4.0
  else (abs (fI rate - avgRate))/4.0
  where
    goodRates = filter checkGood productRates
    avgRate =
      if length goodRates > 0
        then (fI (sum $ map fst goodRates))/(fI (length goodRates))
        else (fI (sum $ map fst productRates))/(fI (length productRates))

checkGood :: (Rate, Karma) -> Bool
checkGood (_, Karma y) = y <= 0.5
checkGood (_, Unknown) = True






