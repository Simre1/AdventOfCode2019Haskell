import qualified Data.Set as S
import Data.Maybe (isJust)
import Data.Foldable (foldl')
import Data.List

solve1 :: IO ()
solve1 = do
  let numbers = show <$> generateNumbersBetween puzzleInput
      solutions = filter filterNumber numbers
  print $ length solutions
  where filterNumber num = shouldFilterIncreasingNumbers num && shouldFilterDoubleOccurence num


solve2 :: IO ()
solve2 = do
  let numbers = show <$> generateNumbersBetween puzzleInput
      solutions = filter filterNumber numbers
  print $ length solutions
  where filterNumber num = shouldFilterIncreasingNumbers num && shouldFilterExactDoubleOccurence num


puzzleInput :: (Int,Int)
puzzleInput = (382345,843167)

-- between 382345-843167
-- It is a six-digit number.
-- The value is within the range given in your puzzle input.
-- Two adjacent digits are the same (like 22 in 122345).
-- Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

generateNumbersBetween :: (Int, Int) -> [Int]
generateNumbersBetween (min,max) = [min..max]

shouldFilterIncreasingNumbers :: String -> Bool
shouldFilterIncreasingNumbers = isJust . foldl' check (Just '0')
  where check previous current = case previous of
          Nothing -> Nothing
          (Just x) -> if x <= current
                        then Just current
                        else Nothing

shouldFilterDoubleOccurence :: String -> Bool
shouldFilterDoubleOccurence i = 6 /= S.size (S.fromList i)

shouldFilterExactDoubleOccurence :: String -> Bool
shouldFilterExactDoubleOccurence i = or $ (==2) . length <$> (group i)
