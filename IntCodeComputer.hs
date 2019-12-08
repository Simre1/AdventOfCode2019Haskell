module IntCodeComputer where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List (foldl', unfoldr)
import Control.Monad (when)
import Control.Monad.Trans.State (StateT, gets, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import Text.Read (readMaybe)
import qualified Data.Array as A
import qualified Data.Array.MArray as A
import qualified Data.Array.IO as A
import qualified Control.Concurrent.KazuraQueue as Q
import Control.Concurrent.Async (async, Async)

data ExecutionState = ExecutionState
  { intCode :: A.IOArray Int Int
  , pointer :: Int
  , input :: IO Int
  , output :: Int -> IO ()
  }

type Execution a = StateT ExecutionState IO a

data Mode = Position | Immediate

plusPointer :: Execution ()
plusPointer = modify plusPointer'
  where plusPointer' (executionState@ExecutionState{pointer=pointer}) = executionState{pointer=pointer+1}

getValue :: Mode -> Execution Int
getValue mode = case mode of
  Immediate -> do
    position <- gets pointer
    intCodeArray <- gets intCode
    plusPointer
    lift $ A.readArray intCodeArray position
  Position -> do
    positionPosition <- gets pointer
    intCodeArray <- gets intCode
    valuePosition <- lift $ A.readArray intCodeArray positionPosition
    plusPointer
    lift $ A.readArray intCodeArray valuePosition

getInput :: Execution Int
getInput = do
  getInput' <- gets (input)
  lift getInput'

putOutput :: Int -> Execution ()
putOutput a = do
  putOutput' <- gets (output)
  lift $ putOutput' a

setPointer :: Int -> Execution ()
setPointer newPointer = modify setPointer'
  where setPointer' executionState = executionState{pointer=newPointer}

addOp :: Int -> Int -> Int -> Execution ()
addOp x y position = do
  intCodeArray <- gets intCode
  lift $ A.writeArray intCodeArray position (x + y)

multOp :: Int -> Int -> Int -> Execution ()
multOp x y position = do
  intCodeArray <- gets intCode
  lift $ A.writeArray intCodeArray position (x * y)

inputOp :: Int -> Execution ()
inputOp position = do
  intCodeArray <- gets intCode
  val <- getInput
  lift $ A.writeArray intCodeArray position val

outputOp :: Int -> Execution ()
outputOp val = do
  putOutput val

jumpOp :: Bool -> Int -> Execution ()
jumpOp shouldJump newPointer = when shouldJump $ setPointer newPointer

writeIf :: Bool -> Int -> Int -> Int -> Execution ()
writeIf bool i1 i2 position = do
  intCodeArray <- gets intCode
  lift $ A.writeArray intCodeArray position i
  where i = if bool then i1 else i2

executeIntCode :: [Int] -> [Int] -> IO ([Int])
executeIntCode intCode' input' = do
  (inputQueue) <- Q.newQueue
  (outputQueue) <- Q.newQueue
  foldMap (Q.writeQueue inputQueue) input'
  executeIntCodeWithInteractiveIO intCode' (Q.readQueue inputQueue) (Q.writeQueue outputQueue)
  genOutputList outputQueue

  where
    genOutputList outputQueue = do
      maybeElem <- Q.tryReadQueue outputQueue
      case maybeElem of
        Just a -> (a:) <$> genOutputList outputQueue
        Nothing -> pure []

      --intCodeList <- sequenceA $ unfoldr arrayToList (0,array)
    -- arrayToList (position, array)
    --   | position < length intCode' = pure $ (A.readArray array position, (position + 1, array))
    --   | otherwise = Nothing

executeIntCodeWithQueue :: [Int] -> IO (Async (), IO Int, (Int -> IO ()))
executeIntCodeWithQueue intCode' = do
  (inputQueue) <- Q.newQueue
  (outputQueue) <- Q.newQueue
  a <- async $ executeIntCodeWithInteractiveIO intCode' (Q.readQueue inputQueue) (Q.writeQueue outputQueue)
  pure (a, Q.readQueue outputQueue, Q.writeQueue inputQueue)



executeIntCodeWithInteractiveIO :: [Int] -> IO Int -> (Int -> IO ()) -> IO ()
executeIntCodeWithInteractiveIO intCode' ioInput ioOutput = do
  array <- A.newArray (0,length intCode') 0
  snd $ foldl' (loadIntCode array) (0,pure ()) intCode'
  (_,s) <- runStateT executionLoop $
    ExecutionState array 0 ioInput ioOutput
  pure ()
  where
    loadIntCode array (position, action) value =
      (succ position, action *> A.writeArray array position value)


normaliseCode :: Int -> [Char]
normaliseCode int =
  let notNormalised = show int
  in normalise notNormalised
  where
    normalise number
      | length number == 1 = replicate 4 '0' ++ number
      | number == "99" = "99999"
      | length number == 3 = '0':'0':number
      | length number == 4 = '0':number
      | length number /= 5 = error $ "Normalising of " ++ number ++ " failed!"
      | otherwise = number

executionLoop :: Execution ()
executionLoop = do
  [o0,m1',m0',a1,a0] <- normaliseCode <$> getValue Immediate
  let m1 = toMode m1'
      m0 = toMode m0'
  case (a1:a0:[]) of
    "01" -> do
      n0 <- getValue m0
      n1 <- getValue m1
      pos <- getValue Immediate
      addOp n0 n1 pos
      executionLoop
    "02" -> do
      n0 <- getValue m0
      n1 <- getValue m1
      pos <- getValue Immediate
      multOp n0 n1 pos
      executionLoop
    "03" -> do
      pos <- getValue Immediate
      inputOp pos
      executionLoop
    "04" -> do
      val <- getValue m0
      outputOp val
      executionLoop
    "05" -> do
      conditionVal <- getValue m0
      pointerVal <- getValue m1
      jumpOp (conditionVal /= 0) pointerVal
      executionLoop
    "06" -> do
      conditionVal <- getValue m0
      pointerVal <- getValue m1
      jumpOp (conditionVal == 0) pointerVal
      executionLoop
    "07" -> do
      conditionVal0 <- getValue m0
      conditionVal1 <- getValue m1
      pos <- getValue Immediate
      writeIf (conditionVal0 < conditionVal1) 1 0 pos
      executionLoop
    "08" -> do
      conditionVal0 <- getValue m0
      conditionVal1 <- getValue m1
      pos <- getValue Immediate
      writeIf (conditionVal0 == conditionVal1) 1 0 pos
      executionLoop
    "99" -> pure ()
    a -> error $ a ++ " is not a valid opcode!"

errorHead :: [a] -> a
errorHead [] = error "Program tried to take more input than given."
errorHead (a:_) = a

toMode :: Char -> Mode
toMode '1' = Immediate
toMode '0' = Position
toMode a = error $ show a ++ " is not a valid mode!"
