{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forM_)

import Data.Char (isNumber, isUpper)
import Data.Function (on)
import Data.List (foldl', maximumBy)
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.Map.Lazy as LazyMap

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio (Ratio, (%))

import System.Environment (getArgs)

type Word = String
data Tag = Tag String | Start | Stop
         deriving (Eq, Ord, Show)
type TrainData = [[(Word, Tag)]]
type TestData = [[Word]]

trainDataPath :: FilePath
trainDataPath = "gene.train"

sentences :: String -> [[String]]
sentences = go [] . lines
  where go [] []       = []
        go r  []       = [reverse r]
        go r ("" : ls) = reverse r : go [] ls
        go r (l : ls)  = go (l : r) ls

parseTrainData :: String -> TrainData
parseTrainData = map (map parseLine) . sentences
  where parseLine line = let [w, t] = words line in (w, Tag t)

readTrainData :: IO TrainData
readTrainData = fmap parseTrainData $ readFile trainDataPath

wordCounts :: TrainData -> Map Word Int
wordCounts = foldl' k Map.empty . concat
  where k m (w, _) = Map.insertWith (+) w 1 m

emission :: TrainData -> Map (Word, Tag) (Ratio Integer)
emission (concat -> tdata) = Map.mapWithKey f wordTagCounts
  where f (_, t) count | Just tagCount <- Map.lookup t tagCounts = count % tagCount
                       | otherwise = error "emission: impossible"

        wordTagCounts = foldl' k Map.empty tdata
          where k m wt = Map.insertWith (+) wt 1 m

        tagCounts = foldl' k Map.empty tdata
          where k m (_, t) = Map.insertWith (+) t 1 m

qs :: TrainData -> Map (Tag, Tag, Tag) (Ratio Integer)
qs tdata = Map.fromList $ map p $ Map.toList trigrams
  where p ([ca, cb, cc], v) = ((ca, cb, cc), v % count)
          where Just count = Map.lookup [ca, cb] bigrams
        p _ = error "qs: impossible"

        bigrams = Map.mapKeysWith (+) init trigrams
        trigrams = foldl' (processSentence 3) Map.empty tdata'

        tdata' = map (map snd) tdata

        processSentence k m = foldl' f m . grams (replicate (k - 1) Start)
          where f m' x = Map.insertWith (+) x 1 m'

        grams ctx [] = [ctx ++ [Stop]]
        grams ctx (x : xs) = gram : grams (tail gram) xs
          where gram = ctx ++ [x]

tags :: TrainData -> Set Tag
tags = foldl' (flip Set.insert) Set.empty . concatMap (map snd)

wrds :: TrainData -> Set Word
wrds = foldl' (flip Set.insert) Set.empty . concatMap (map fst)

viterbi :: TrainData -> [Word] -> [(Word, String)]
viterbi trainData testWords = zip testWords (goTags n ru rv [])
  where ((n, ru, rv), vmap) = viterbiMap trainData testWords

        goTags k u v r | k == 1 = unwrap v : r
                       | k > 1  = goTags (k - 1) w u (unwrap v : r)
                       | otherwise = error "viterbi.goTags: impossible"
          where (_, w) = fromJust $ LazyMap.lookup (k, u, v) vmap

        unwrap (Tag t) = t
        unwrap _ = error "viterbi.unwrap: impossible"

viterbiMap :: TrainData -> [Word] -> ((Int, Tag, Tag),
                                      LazyMap.Map (Int, Tag, Tag) (Ratio Integer, Tag))
viterbiMap trainData testWords = ((n, ru, rv), pis)
  where e = emission trainData
        q = qs trainData

        ws = wrds trainData
        ts = Set.toList $ tags trainData

        n = length testWords
        (ru, rv) = snd $ maximumBy cmp [(p, (u, v)) | u <- tagsOfIx (n - 1),
                                                      v <- tagsOfIx n,
                                                      let p1 = getPi n u v,
                                                      let p2 = getQ Stop u v,
                                                      let p = p1 * p2]
          where cmp = compare `on` fst

        tagsOfIx ix | ix == -1 ||
                      ix == 0 = [Start]
                    | ix > 0 = ts
                    | otherwise = error $ "bad index: " ++ show ix

        pis = LazyMap.fromList $
                [((0, Start, Start), (1, error "Start backpointer used"))] ++
                 concat [[((k, u, v), p) | u <- tagsOfIx (k - 1),
                                           v <- tagsOfIx k,
                                           let p = go k xk u v]
                      | (k, xk) <- zip [1..] testWords]
        go k xk u v = maximumBy cmp [(p, w) | w <- tagsOfIx (k - 2),
                                              let p1 = getPi (k - 1) w u,
                                              let p2 = getQ v w u,
                                              let p3 = getE xk v,
                                              let p = p1 * p2 * p3]
          where cmp = compare `on` fst

        getE w t | Set.member w ws = Map.findWithDefault 0 (w, t) e
                 | otherwise = getE (rareClass w) t

        getQ v w u = Map.findWithDefault 0 (w, u, v) q

        getPi k u v = fst $ Map.findWithDefault (0, undefined) (k, u, v) pis

preprocessTrainData :: TrainData -> TrainData
preprocessTrainData tdata = map (map k) tdata
  where counts = wordCounts tdata
        k (w, t)
          | Just count <- Map.lookup w counts =
            let w' = if count < 5 then rareClass w else w
            in (w', t)
          | otherwise = error "preprocessTrainData: impossible"

rareClass :: Word -> Word
rareClass w | any isNumber w   = "__NUMERIC__"
            | all isUpper w    = "__ALL_UPPER__"
            | isUpper (last w) = "__LAST_CAPITAL__"
            | otherwise        = "__RARE__"

readTestData :: FilePath -> IO TestData
readTestData = fmap sentences . readFile

main :: IO ()
main = do
  [testPath] <- getArgs

  trainData <- fmap preprocessTrainData readTrainData
  testData <- readTestData testPath

  forM_ testData $ \sentence -> do
    let r = viterbi trainData sentence
    forM_ r $ \(w, t) -> do
      putStrLn $ w ++ " " ++ t
    putStrLn ""
