import Data.List.Split (splitOn)
import Data.List (sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Graph.Immutable as G
import Debug.Trace

type Planet = String

type Orbit = [Planet]

type PlanetMap = M.Map Planet Orbit


type OrbitDepth = Int

solve1 :: IO ()
solve1 = do
  planetMap <- toPuzzle1 <$> getPuzzleInput
  print $ countOrbits planetMap 0 "COM"

solve2 :: IO ()
solve2 = do
  planetMap <- toPuzzle2 <$> getPuzzleInput
  print $ findAmountOfJumps planetMap

countOrbits :: PlanetMap -> OrbitDepth -> Planet -> Int
countOrbits planetMap orbitDepth rootPlanet =
  orbitDepth + childrenOrbits
  where
    childrenOrbits :: Int
    childrenOrbits = fromMaybe 0 $ do
      orbitingPlanets <- M.lookup rootPlanet planetMap
      pure $ sum $ countOrbits planetMap (succ orbitDepth) <$> orbitingPlanets

findAmountOfJumps :: PlanetMap -> Int
findAmountOfJumps planetMap =
  let youAncestors = getAncestors "YOU"
      santaAncestors = getAncestors "SAN"
      differentAncestors =
        S.difference
        (S.union santaAncestors youAncestors)
        (S.intersection youAncestors santaAncestors)
  in length differentAncestors
  where
    getAncestors :: Planet -> S.Set Planet
    getAncestors planet =
      let immediateAncestors = fromMaybe mempty $ S.fromList <$> M.lookup planet planetMap
          successiveAncestors = foldMap getAncestors immediateAncestors
      in immediateAncestors <> successiveAncestors


toPuzzle2 :: [String] -> (M.Map Planet Orbit)
toPuzzle2 input = toMap input
  where
    toMap :: Orbit -> M.Map Planet Orbit
    toMap = foldr addEdge mempty
      where
        addEdge :: Planet -> M.Map Planet Orbit -> M.Map Planet Orbit
        addEdge edgeString edgeMap =
          let [from, to] = splitOn ")" edgeString
              add x = case x of
                Just a -> pure $ from:a
                Nothing -> pure [from]
          in M.alter add to edgeMap

toPuzzle1 :: [String] -> (M.Map Planet Orbit)
toPuzzle1 input = toMap input
  where
    toMap :: Orbit -> M.Map Planet Orbit
    toMap = foldr addEdge mempty
      where
        addEdge :: Planet -> M.Map Planet Orbit -> M.Map Planet Orbit
        addEdge edgeString edgeMap =
          let [from, to] = splitOn ")" edgeString
              add x = case x of
                Just a -> pure $ to:a
                Nothing -> pure [to]
          in M.alter add from edgeMap

getPuzzleInput :: IO [String]
getPuzzleInput = lines <$> readFile "6.data"

testInput :: IO [String]
testInput = pure $ [("COM)B") ,("B)C") ,("C)D") ,("D)E") ,("E)F") ,("B)G") ,("G)H") ,("D)I") ,("E)J") ,("J)K") ,("K)L") ,("K)YOU") ,("I)SAN")]
