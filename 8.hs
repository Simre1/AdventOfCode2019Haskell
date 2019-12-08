{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortBy, transpose)

imageLength = 25
imageWidth = 6

solve2 :: IO ()
solve2 = do
  layers <- toLayers <$> openFile
  let mergedImage = foldl mergePixel ('2') <$> transpose layers
  putStrLn $ intersperseEvery 25 '\n' mergedImage


solve1 :: IO ()
solve1 = do
  bestImage <- chooseBestLayer . toLayers <$> openFile
  case bestImage of
    Nothing -> print "Input did not have any layers"
    Just layer -> print $ calc1sAnd2s layer

intersperseEvery :: Int -> a -> [a] -> [a]
intersperseEvery _ _ [] = []
intersperseEvery a s list = intersperseEvery' 0 list
  where
    intersperseEvery' _ [] = []
    intersperseEvery' c (x:xs)
      | c == a = s:x:intersperseEvery' 1 xs
      | otherwise = x:intersperseEvery' (succ c) xs

mergePixel :: Char -> Char -> Char
mergePixel p1 p2
  | p1 == ('2') = p2
  | otherwise = p1

calc1sAnd2s :: String -> Int
calc1sAnd2s layer =
  let p1 = filter (\a -> a ==  ( '1')) layer
      p2 = filter (\a -> a ==  ( '2')) layer
  in length p1 * length p2

openFile :: IO String
openFile = readFile "8.data"

toLayers :: String -> [String]
toLayers str
  | length str >= 25*6 = take (25*6) str:toLayers (drop (25*6) str)
  | otherwise = []

chooseBestLayer :: [String] -> Maybe String
chooseBestLayer [] = Nothing
chooseBestLayer images = Just . snd . head $ sortBy fewestZeroes $ (amountOfZeroes <$> images) `zip` images
  where amountOfZeroes = foldl (\amount a -> amount + if a == ( ( '0')) then 1 else 0) 0
        fewestZeroes (a,_) (b,_) = a `compare` b
