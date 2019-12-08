import Data.Char
import Data.List
import qualified Data.Set as S
import Debug.Trace

solve1 :: IO ()
solve1 = do
  (line1, line2) <- getPuzzleInput
  let positions1 = directionsToPositionList (0,0) line1
      positions2 = directionsToPositionList (0,0) line2
      intersections = findIntersection positions1 positions2
      nearestToOrigin = S.take 1 $ S.map (\(x,y) -> abs x + abs y) intersections
  print nearestToOrigin

solve2 :: IO ()
solve2 = do
  (line1, line2) <- getPuzzleInput
  let positions1 = directionsToPositionList (0,0) line1
      positions2 = directionsToPositionList (0,0) line2
      intersections = findIntersection positions1 positions2
      shortestPath1 = fmap (countSteps (0,0) line1) $ S.toList $ intersections
      shortestPath2 = fmap (countSteps (0,0) line2) $ S.toList $ intersections
      stepsNeeded = sort $ zipWith (+) shortestPath1 shortestPath2
  print stepsNeeded

testPuzzleInput :: IO ([Direction], [Direction])
testPuzzleInput = pure
  (toDirections "R75,D30,R83,U83,L12,D49,R71,U7,L72",toDirections "U62,R66,U55,R34,D71,R55,D58,R83")

countSteps :: Position -> [Direction] -> Position -> Int
countSteps _ [] _ = 0
countSteps start (d:ds) goal
  | start == goal = 0
  | otherwise = succ $ countSteps (goDirection start d) ds goal

getPuzzleInput :: IO ([Direction], [Direction])
getPuzzleInput = do
  (data1, data2) <- break (=='\n') <$> readFile "3.data"
  let line1 = toDirections data1
      line2 = toDirections $ tail $ init $ data2
  pure (line1, line2)

type Position = (Int, Int)

data Direction = U | D | L | R deriving (Show)

toDirections  :: String -> [Direction]
toDirections [] = []
toDirections (',':xs) = toDirections xs
toDirections (c:x:y:z:rest)
  | isNumber c = error "Parsing failed! Got number where char was expected."
  | not (isNumber y) = replicate (read (x:[])) (toDirection c) ++ toDirections (y:z:rest)
  | not (isNumber z) = replicate (read (x:y:[])) (toDirection c) ++ toDirections (z:rest)
  | otherwise = replicate (read (x:y:z:[])) (toDirection c) ++ toDirections rest
  where toDirection c = case c of
          'U' -> U
          'D' -> D
          'L' -> L
          'R' -> R
          otherwise -> error "Parsing failed! Only U, D, L or R chars are allowed."

toDirections (c:x:[]) = toDirections (c:'0':'0':x:[])
toDirections (c:x:y:[]) = toDirections (c:'0':x:y:[])

directionsToPositionList :: Position -> [Direction] -> S.Set Position
directionsToPositionList _ [] = mempty
directionsToPositionList position (direction:rest) =
  let newPosition = goDirection position direction
  in S.insert newPosition $ directionsToPositionList newPosition rest

goDirection :: Position -> Direction -> Position
goDirection (x,y) direction = case direction of
  U -> (x, succ y)
  D -> (x, pred y)
  L -> (pred x, y)
  R -> (succ x, y)

findIntersection :: S.Set Position -> S.Set Position -> S.Set Position
findIntersection positions1 positions2
  | null positions1 = mempty
  | S.member (S.elemAt 0 positions1) positions2 = S.insert (S.elemAt 0 positions1) $ findIntersection (S.drop 1 positions1) positions2
  | otherwise = findIntersection (S.drop 1 positions1) positions2
