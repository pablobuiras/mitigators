module Mitigator where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.IO

-- Schedules

type Time = Double

data Schedule a = Predict a (Schedule a)
                deriving (Show)

takeS :: Int -> Schedule a -> [a]
takeS 0 _ = []
takeS n (Predict x xs) = x : takeS (n-1) xs

thenS :: [a] -> Schedule a -> Schedule a
thenS xs s = foldr Predict s xs

periodic :: a -> Schedule a
periodic q = Predict q (periodic q)

alternate :: Schedule a -> Schedule a -> Schedule a
alternate (Predict q s1) s2 = Predict q (alternate s2 s1)

-- Mitigators

sizeCond = id
(..>) = (,)

epoch :: Schedule a -> Mitigator a
epoch s = M s []

infix 9 ..>
infixl 8 |>, |>>

type Transition a = (Int -> Bool, Mitigator a)

(|>) :: Mitigator a -> Transition a -> Mitigator a
m |> t = m { transitions = transitions m ++ [Left t] }

(|>>) :: Mitigator a -> Transition a -> Mitigator a
m |>> t = m { transitions = transitions m ++ [Right t] }

data Mitigator a = M { schedule :: Schedule a
                     , transitions :: [Either (Transition a) (Transition a)]
                     }


-- Example mitigator policy: 'Fast doubling'

fd :: Int -> Mitigator Int
fd q = epoch (periodic q) |> sizeCond (==0) ..> fd (2*q)

-- Really quick mitigator implementation.

type Channel a = MVar [a]

mitigate :: Show a =>
            (Channel a -> IO ()) -- computation to mitigate
            -> Mitigator Int     -- (time) mitigation policy
            -> (Time -> Double)  -- bound on info leakage
            -> IO ()
mitigate m mit b =
  do ch <- newMVar []
     hSetBuffering stdin NoBuffering
     tid <- forkIO (mitigate' ch mit)
     m ch `finally` killThread tid
    where mitigate' ch mit = 
            do let Predict q _ = schedule mit
               threadDelay q
               m' <- adapt mit ch
               r <- modifyMVar ch (\buf -> case buf of [] -> return (buf,Nothing); x:xs -> return (xs,Just x))
               case r of
                 Nothing -> putStrLn "deadline missed :("
                 Just x -> putStrLn $ "output : " ++ show x
               let Predict q' _ = schedule m'
               putStrLn $ "Quantum is: " ++ show q
               mitigate' ch m'
          adapt mit ch =
            do size <- fmap length (readMVar ch)
               let trs = dropWhile (not . fst) $ 
                         map g $ transitions mit
                   g (Left (cond,m)) = (cond size, m)
                   g (Right (cond,m)) = (cond size && adaptiveCondition, m)
                   adaptiveCondition = False -- REFINE: depends on time and number of epochs
               case trs of
                 [] -> return mit
                 (_,m):_ -> return m
                 
output :: Channel a -> a -> IO ()
output ch x = modifyMVar ch (\buf -> return (buf++[x],()))

interactiveMitigate mit = mitigate (\ch -> forever (getChar >> output ch ())) mit id

-- Idea: Instead of expressing the mitigated computation as a function
-- of type Channel a -> IO () directly, use a reactive program.
-- Examples: reactive-banana, pipes, conduits, iteratees or yampa.

-- test interactive
test0 = interactiveMitigate $ fd 1000000 -- 1 sec initial quantum