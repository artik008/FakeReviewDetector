{-# LANGUAGE RecordWildCards #-}
module TextModel where

import           Models


textKarmaAnalyze :: String ->ProcessedTextInfo -> [Float]
textKarmaAnalyze text ptInfo = 
  freqUnoGramms text ++ 
  freqBiGramms text ++
  freqPunctuation ptInfo ++
  freqSpeechParts ptInfo ++
  map (\x -> x ptInfo)
  [
    freqSentWordCount
  , freqWordLength
  , freqWordCount

  -- , freqTriGramms

  -- , freqWordsForms

  -- , freqBadWords 

  -- , semanticVal
  -- , sintacticVal
  ]


freqSentWordCount :: ProcessedTextInfo -> Float
freqSentWordCount ProcessedTextInfo{..} = 1.0 - 1.0/ptAvgWordsInSent

freqWordLength :: ProcessedTextInfo -> Float
freqWordLength ProcessedTextInfo{..} = 1.0 - 1.0/ptAvgWordLength

freqWordCount :: ProcessedTextInfo -> Float
freqWordCount ProcessedTextInfo{..} = 1.0 - 1.0/ptWordCount

freqUnoGramms :: String -> [Float]
freqUnoGramms text = map (\x -> (fromIntegral x)/l) counts
  where
    unoSet = ['о', 'а', 'е', 'и', 'н', 'т', 'с']
    counts = map (\x -> length $ filter (== x) text) unoSet
    l = fromIntegral $ length text

freqBiGramms :: String -> [Float]
freqBiGramms text = map (\x -> (fromIntegral x)/l) counts
  where
    biSet = ["ст","но","ен","то"
            ,"на","ов","ни","ра"
            ,"во","ко","пр","ос"
            ,"по","ре","ро","та"
            ,"од","ол","ом","он"
            ,"ор","от","ли","ка"
            ,"ер","ет","го","ал"
            ,"ан"
            ]
    counts = map (\x -> countBiGramm x text 0) biSet
    l = (fromIntegral $ length text)/2.0

countBiGramm :: String -> String -> Int -> Int
countBiGramm _ [] c = c 
countBiGramm x [_] c = countBiGramm x [] c
countBiGramm x (ts: tss: lt) c = 
  if x == [ts, tss]
  then countBiGramm x (tss:lt) (c + 1)
  else countBiGramm x (tss:lt) c

-- freqTriGramms :: ProcessedTextInfo -> Float
-- freqTriGramms ProcessedTextInfo{..} = 0.5

-- Do with Python script
freqSpeechParts :: ProcessedTextInfo -> [Float]
freqSpeechParts ProcessedTextInfo{..} = map (\x -> (fromIntegral x)/l) counts
  where
    spSet = [ ["NOUN"]
            , ["ADJF", "ADJS"]
            , ["VERB", "INFN"]
            , ["PRTF", "PRTS"]
            , ["GRND"]
            , ["NUMR"]
            , ["ADVB"]
            , ["CONJ"]
            , ["INTJ"]
            ]
    counts = map (\x -> countSPs x) spSet
    countSPs x = 
      foldl (\y t -> y + (snd t)) 0 $
      filter (\y -> elem (fst y) x) ptSpeechParts
    l = fromIntegral $ sum $ map snd ptSpeechParts

-- Do with Python script
freqPunctuation :: ProcessedTextInfo -> [Float]
freqPunctuation ProcessedTextInfo{..} = map (\x -> (fromIntegral x)/l) counts
  where
    spSet = [".", ",", "!", "?", "(", ")"]
    counts = map (\x -> countPuncts x) spSet
    countPuncts x = 
      foldl (\y t -> y + (snd t)) 0 $
      filter (\y -> (fst y) == x) ptPunctuation
    l = fromIntegral $ sum $ map snd ptPunctuation


-- freqWordsForms :: ProcessedTextInfo -> Float
-- freqWordsForms ProcessedTextInfo{..} = 0.5

-- freqBadWords :: ProcessedTextInfo -> Float
-- freqBadWords ProcessedTextInfo{..} = 0.5

-- -- Do with Python script ???
-- semanticVal :: ProcessedTextInfo -> Float
-- semanticVal ProcessedTextInfo{..} = 0.5

-- -- Do with Python script ???
-- sintacticVal :: ProcessedTextInfo -> Float
-- sintacticVal ProcessedTextInfo{..} = 0.5



