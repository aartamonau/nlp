import Control.Monad (forM_)

import Data.Function (on)
import Data.List (foldl', maximumBy)
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio (Ratio, (%))

import System.Environment (getArgs)

type Word = String
type Tag = String
type TrainData = [(Word, Tag)]
type TestData = [Word]

trainDataPath :: FilePath
trainDataPath = "gene.train"

parseTrainData :: String -> TrainData
parseTrainData = catMaybes . map parseLine . lines
  where parseLine "" = Nothing
        parseLine line = let [w, t] = words line in Just (w, t)

readTrainData :: IO TrainData
readTrainData = fmap parseTrainData $ readFile trainDataPath

wordCounts :: TrainData -> Map Word Int
wordCounts = foldl' k Map.empty
  where k m (w, _) = Map.insertWith (+) w 1 m

emission :: TrainData -> Map Word (Map Tag (Ratio Int))
emission tdata = Map.map (Map.mapWithKey mapTag) wordTagCounts
  where mapTag t count | Just tagCount <- Map.lookup t tagCounts = count % tagCount
                       | otherwise = error "impossible"

        wordTagCounts = foldl' k Map.empty tdata
          where k m (w, t) = Map.alter (f t) w m

                f t (Just m) = Just $ Map.insertWith (+) t 1 m
                f t Nothing   = f t (Just Map.empty)

        tagCounts = foldl' k Map.empty tdata
          where k m (_, t) = Map.insertWith (+) t 1 m

preprocessTrainData :: TrainData -> TrainData
preprocessTrainData dta = map k dta
  where counts = wordCounts dta
        k (w, t)
          | Just count <- Map.lookup w counts =
            let w' = if count < 5 then "_RARE_" else w
            in (w', t)
          | otherwise = error "impossible"

readTestData :: FilePath -> IO TestData
readTestData = fmap lines . readFile

main :: IO ()
main = do
  [testPath] <- getArgs

  trainData <- fmap preprocessTrainData readTrainData
  testData <- readTestData testPath

  let es = emission trainData
  let rareEs = Map.lookup "_RARE_" es

  forM_ testData $ \word -> do
    if nonEmpty word
      then do
        let Just wordTags =
              case Map.lookup word es of
                Nothing -> rareEs
                wt@(Just _) -> wt
        let wordTagsList = Map.toList wordTags
        let likelyTag = fst $ maximumBy (compare `on` snd) wordTagsList
        putStrLn $ word ++ " " ++ likelyTag

      else putStrLn ""

  where nonEmpty "" = False
        nonEmpty _  = True
