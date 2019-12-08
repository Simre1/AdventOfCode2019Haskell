import Data.List.Split
import Data.Sequence as S
import Data.Maybe

main :: IO ()
main = do
  opcode <- S.fromList <$> openFile
  let (noun, verb) = fromJust $ guess opcode 0 0
  print (noun, verb, 100*noun + verb)

guess :: Seq Int -> Int -> Int -> Maybe (Int, Int)
guess opcode noun verb
  | calculateWithNounAndVerb opcode noun verb == wantedResult = pure (noun, verb)
  | verb == 98 = guess opcode (succ noun) 0
  | noun == 98 && verb == 98 = Nothing
  | otherwise = guess opcode noun (succ verb)

wantedResult :: Int
wantedResult = 19690720

calculateWithNounAndVerb :: Seq Int -> Int -> Int -> Int
calculateWithNounAndVerb opcode noun verb = fromJust $ S.lookup 0 $ executeOpcode (update 2 verb $ update 1 noun opcode) 0

openFile :: IO [Int]
openFile = do
  fmap read . splitOn "," <$> readFile "2.data"

executeOpcode :: Seq Int -> Int -> Seq Int
executeOpcode opcode position =
  case S.lookup position opcode of
    Nothing -> opcode
    (Just x) -> case x of
      99 -> opcode
      1 -> fromMaybe opcode $ do
        position1 <- S.lookup (1 + position) opcode
        position2 <- S.lookup (2 + position) opcode
        position3 <- S.lookup (3 + position) opcode
        val1 <- S.lookup position1 opcode
        val2 <- S.lookup position2 opcode
        pure $ executeOpcode (update position3 (val1 + val2) opcode) (position + 4)
      2 -> fromMaybe opcode $ do
        position1 <- S.lookup (1 + position) opcode
        position2 <- S.lookup (2 + position) opcode
        position3 <- S.lookup (3 + position) opcode
        val1 <- S.lookup position1 opcode
        val2 <- S.lookup position2 opcode
        pure $ executeOpcode (update position3 (val1 * val2) opcode) (position + 4)
