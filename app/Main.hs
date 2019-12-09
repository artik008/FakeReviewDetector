module Main where

import           Detector (countWithTeacher)

import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then putStrLn "Empty args. To run program use: FakeReviewDetector-exe <path_to_teacher> <path_to_tester>"
    else countWithTeacher (args!!0) (args!!1)
