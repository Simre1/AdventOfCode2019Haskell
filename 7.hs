import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.List (permutations)
import Data.Maybe (catMaybes)
import IntCodeComputer
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Data.IORef
import Control.Monad (forever)

openFile :: IO [Int]
openFile = do
  instructions <- splitOn "," <$> readFile "7.data"
  pure $ tryRead <$> instructions
  where tryRead x = case readMaybe x of
          Just i -> (i :: Int)
          Nothing -> error $ "I could not parse:" ++ x

solve1 :: IO ()
solve1 = do
  amplifier <- openFile
  outputs <- fmap catMaybes $ sequenceA $ trySequentialAmplifiers amplifier <$> possiblePhaseSettings1
  print $ maximum outputs

solve2 :: IO ()
solve2 = do
  amplifier <- openFile
  outputs <- sequenceA $ tryFeedbackAmplifiers amplifier <$> possiblePhaseSettings2
  print $ maximum outputs


possiblePhaseSettings1 :: [(Int,Int,Int,Int,Int)]
possiblePhaseSettings1 = toTuple <$> permutations [0,1,2,3,4]
  where toTuple [a,b,c,d,e] = (a,b,c,d,e)

possiblePhaseSettings2 :: [(Int,Int,Int,Int,Int)]
possiblePhaseSettings2 = toTuple <$> permutations [9,8,7,6,5]
  where toTuple [a,b,c,d,e] = (a,b,c,d,e)


trySequentialAmplifiers :: [Int] -> (Int,Int,Int,Int,Int) -> IO (Maybe [Int])
trySequentialAmplifiers amplifier (p0,p1,p2,p3,p4) = do
  output <- try $ amplify p0 [0] >>= amplify p1 >>= amplify p2 >>= amplify p3 >>= amplify p4
  case output of
    Left a -> print (a::SomeException) *> pure Nothing
    Right a -> pure $ Just a
  where amplify p input = executeIntCode amplifier (p:input)

tryFeedbackAmplifiers :: [Int] -> (Int,Int,Int,Int,Int) -> IO (Int)
tryFeedbackAmplifiers amplifier (p0,p1,p2,p3,p4) = do
  let amplify = executeIntCodeWithQueue amplifier
  lastVal <- newIORef 0
  (async1, get1, feed1) <- amplify
  (async2, get2, feed2) <- amplify
  (async3, get3, feed3) <- amplify
  (async4,get4, feed4) <- amplify
  (async5, get5, feed5) <- amplify
  do
    async $ forever $ get1 >>= feed2
    async $ forever $ get2 >>= feed3
    async $ forever $ get3 >>= feed4
    async $ forever $ get4 >>= feed5
    async $ forever $ get5 >>= (\a -> writeIORef lastVal a *> feed1 a)
  feed1 p0
  feed1 0
  feed2 p1
  feed3 p2
  feed4 p3
  feed5 p4
  wait async5
  readIORef lastVal
