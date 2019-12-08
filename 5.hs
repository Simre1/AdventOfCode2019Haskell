{-# LANGUAGE BangPatterns #-}
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Text.Read
import Debug.Trace

solve1 :: IO ()
solve1 = do
  opcode <- M.fromList . ([0..] `zip`) <$> openFile
  print $ (\oc -> executeOpcode ([1],[]) oc 0) opcode

solve2 :: IO ()
solve2 = do
  opcode <- M.fromList . ([0..] `zip`) <$> openFile
  print . snd $ (\oc -> executeOpcode ([5],[]) oc 0) opcode



openFile :: IO [Int]
openFile = do
  instructions <- splitOn "," <$> readFile "5.data"
  pure $ tryRead <$> instructions
  where tryRead x = case readMaybe x of
          Just i -> (i :: Int)
          Nothing -> error $ "I could not parse:" ++ x

executeOpcode :: ([Int], [Int]) -> M.Map Int Int -> Int -> (M.Map Int Int, [Int])
executeOpcode (input, output) !opcode position =
  case M.lookup position opcode of
    Nothing -> error "Could not find this position"
    (Just x) -> case (traceShowId x) of
      99999 -> (opcode, output)
      99 -> (opcode, output)
      1 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1 + position) opcode
        position2 <- M.lookup (2 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup position2 opcode
        pure $ executeOpcode (input, output) (M.insert position3 (val1 + val2) opcode) (position + 4)
      101 -> fromMaybe' (opcode, output) $ do
        position2 <- M.lookup (2 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup (1 + position) opcode
        val2 <- M.lookup position2 opcode
        pure $ executeOpcode (input, output) (M.insert position3 (val1 + val2) opcode) (position + 4)
      1001 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup (2 + position) opcode
        pure $ executeOpcode (input, output) (M.insert position3 (val1 + val2) opcode) (position + 4)
      1101 -> fromMaybe' (opcode, output) $ do
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup (1 + position) opcode
        val2 <- M.lookup (2 + position) opcode
        pure $ executeOpcode (input, output) (M.insert position3 (val1 + val2) opcode) (position + 4)
      2 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1 + position) opcode
        position2 <- M.lookup (2 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup position2 opcode
        pure $ executeOpcode (input, output) (M.insert position3 (val1 * val2) opcode) (position + 4)
      102 -> fromMaybe' (opcode, output) $ do
        position2 <- M.lookup (2 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup (1 + position) opcode
        val2 <- M.lookup position2 opcode
        pure $ executeOpcode (input, output) (M.insert position3 (val1 * val2) opcode) (position + 4)
      1002 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup (2 + position) opcode
        pure $ executeOpcode (input, output) (M.insert position3 (val1 * val2) opcode) (position + 4)
      1102 -> fromMaybe' (opcode, output) $ do
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup (1 + position) opcode
        val2 <- M.lookup (2 + position) opcode
        pure $ executeOpcode (input, output) (M.insert position3 (val1 * val2) opcode) (position + 4)
      3 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1+position) opcode
        val1 <- listToMaybe input
        pure $ executeOpcode (drop 1 $ input, output) (M.insert position1 (val1) opcode) (position + 2)
      4 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1+position) opcode
        val1 <- M.lookup position1 opcode
        pure $ executeOpcode (input, output ++ [val1]) opcode (position + 2)
      104 -> fromMaybe' (opcode, output) $ do
        val1 <- M.lookup (position+1) opcode
        pure $ executeOpcode (input, output ++ [val1]) opcode (position + 2)
      5 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1+position) opcode
        position2 <- M.lookup (2+position) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup position2 opcode
        let position3 = if (val1 /= 0)
                          then val2
                          else (position + 3)
        pure $ executeOpcode (input, output ++ [val1]) opcode position3
      6 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1+position) opcode
        position2 <- M.lookup (2+position) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup position2 opcode
        let position3 = if (val1 == 0)
                          then val2
                          else (position + 3)
        pure $ executeOpcode (input, output ++ [val1]) opcode position3
      1005 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (position+1) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup (2+position) opcode
        position3 <- if (val1 /= 0)
          then pure val2
          else Just (position + 3)
        pure $ executeOpcode (input, output ++ [val1]) opcode position3
      1006 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (position+1) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup (2+position) opcode
        position3 <- if (val1 == 0)
          then pure val2
          else Just (position + 3)
        pure $ executeOpcode (input, output ++ [val1]) opcode position3
      1105 -> fromMaybe' (opcode, output) $ do
        val1 <- M.lookup (1+position) opcode
        val2 <- M.lookup (2+position) opcode
        position2 <- if (val1 /= 0)
          then pure val2
          else Just (position + 3)
        pure $ executeOpcode (input, output ++ [val1]) opcode position2
      1106 -> fromMaybe' (opcode, output) $ do
        val1 <- M.lookup (1+position) opcode
        val2 <- M.lookup (2+position) opcode
        position2 <- if (val1 == 0)
          then pure val2
          else Just (position + 3)
        pure $ executeOpcode (input, output ++ [val1]) opcode position2
      105 -> fromMaybe' (opcode, output) $ do
        val1 <- traceShowId $ M.lookup (1+position) opcode
        position2 <- traceShowId $ M.lookup (2+position) opcode
        position3 <- if (val1 /= 0)
          then traceShowId $ M.lookup position2 opcode
          else Just (position + 3)
        pure $ executeOpcode (input, output ++ [val1]) opcode position3
      106 -> fromMaybe' (opcode, output) $ do
        val1 <- M.lookup (1+position) opcode
        position2 <- M.lookup (2+position) opcode
        position3 <- if (val1 == 0)
          then M.lookup position2 opcode
          else Just (position + 3)
        pure $ executeOpcode (input, output ++ [val1]) opcode position3
      7 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1 + position) opcode
        position2 <- M.lookup (2 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup position2 opcode
        let val3 = if val1 < val2
                     then 1
                     else 0
        pure $ executeOpcode (input, output) (M.insert position3 val3 opcode) (position + 4)
      107 -> fromMaybe' (opcode, output) $ do
        position2 <- M.lookup (2 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup (1 + position) opcode
        val2 <- M.lookup position2 opcode
        let val3 = if val1 < val2
                     then 1
                     else 0
        pure $ executeOpcode (input, output) (M.insert position3 val3 opcode) (position + 4)
      1007 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup (2 + position) opcode
        let val3 = if val1 < val2
                     then 1
                     else 0
        pure $ executeOpcode (input, output) (M.insert position3 val3 opcode) (position + 4)
      1107 -> fromMaybe' (opcode, output) $ do
          position3 <- M.lookup (3 + position) opcode
          val1 <- M.lookup (1 + position) opcode
          val2 <- M.lookup (2 + position) opcode
          let val3 = if val1 < val2
                       then 1
                       else 0
          pure $ executeOpcode (input, output) (M.insert position3 val3 opcode) (position + 4)
      8 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1 + position) opcode
        position2 <- M.lookup (2 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup position2 opcode
        let val3 = if val1 == val2
                     then 1
                     else 0
        pure $ executeOpcode (input, output) (M.insert position3 val3 opcode) (position + 4)
      108 -> fromMaybe' (opcode, output) $ do
        position2 <- M.lookup (2 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup (1 + position) opcode
        val2 <- M.lookup position2 opcode
        let val3 = if val1 == val2
                     then 1
                     else 0
        pure $ executeOpcode (input, output) (M.insert position3 (val3) opcode) (position + 4)
      1008 -> fromMaybe' (opcode, output) $ do
        position1 <- M.lookup (1 + position) opcode
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup position1 opcode
        val2 <- M.lookup (2 + position) opcode
        let val3 = if val1 == val2
                     then 1
                     else 0
        pure $ executeOpcode (input, output) (M.insert position3 (val3) opcode) (position + 4)
      1108 -> fromMaybe' (opcode, output) $ do
        position3 <- M.lookup (3 + position) opcode
        val1 <- M.lookup (1 + position) opcode
        val2 <- M.lookup (2 + position) opcode
        let val3 = if val1 == val2
                     then 1
                     else 0
        pure $ executeOpcode (input, output) (M.insert position3 (val3) opcode) (position + 4)
      x ->  error $ "Oops. Could not parse that opcode " ++ show (x, position, opcode)

fromMaybe' :: Show a => a -> Maybe a -> a
fromMaybe' a Nothing = error $ "Some things went out of bounds.."
fromMaybe' _ (Just a) = a
