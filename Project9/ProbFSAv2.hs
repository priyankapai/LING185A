module ProbFSAv2 where

---------------------------------------------------------------

import qualified Data.Map as Map

-- A useful helper for debugging with Map. Feel free to ignore 
-- the implementation of this.
printMap :: (Show k, Show v) => Map.Map k v -> IO ()
printMap = putStr . unlines . map show . Map.toList

---------------------------------------------------------------

type State = Int

type ProbFSA = ([(State,Double)],                   -- start distribution
                [(State,Double)],                   -- ending probabilities
                [((State,State),Double)],           -- transition probabilities
                [(((State,State),String),Double)],  -- emissions
                [State])                            -- all states

-- This is a probabilistic version of a grammar from Assignment #4.
pfsa1 :: ProbFSA
pfsa1 = (   -- start distribution
            [(1,1.0)] ,
            -- end probabilities
            [(5,0.5)] ,
            -- transition probabilities
            [((1,2), 0.3), ((1,3), 0.7),
             ((2,3), 1.0),
             ((3,4), 1.0),
             ((4,4), 0.4),
             ((4,5), 0.6),
             ((5,1), 0.5)] ,
            -- emission probabilities
            [(((1,2),"these"), 0.5), (((1,2),"some"), 0.5),
             (((1,3),"they"), 0.4), (((1,3),"these"), 0.6),
             (((2,3),"dogs"), 0.3), (((2,3),"buffalo"), 0.7),
             (((3,4),"buffalo"), 0.6), (((3,4),"damaged"), 0.4),
             (((4,4),"damaged"), 0.7), (((4,4),"nice"), 0.3),
             (((4,5),"unicorns"), 0.8), (((4,5),"stuff"), 0.2),
             (((5,1),"and"),0.1)],
            [1,2,3,4,5]
        )

-- This is the same as pfsa2 from Assignment #8.
pfsa2 :: ProbFSA
pfsa2 = (   -- start distribution
            [(100,1.0)] ,
            -- end probabilities
            [(400,0.5)] ,
            -- transition probabilities
            [((100,200), 0.4), ((100,300), 0.6),
             ((200,400), 1.0),
             ((300,200), 0.3), ((300,400), 0.7),
             ((400,300), 0.5)] ,
            -- emission probabilities
            [(((100,200),"c"), 1.0), 
             (((100,300),"a"), 0.7), (((100,300),"b"), 0.3), 
             (((200,400),"a"), 0.2), (((200,400),"d"), 0.8), 
             (((300,200),"c"), 0.6), (((300,200),"d"), 0.4), 
             (((300,400),"b"), 0.5), (((300,400),"c"), 0.5), 
             (((400,300),"b"), 0.3), (((400,300),"d"), 0.7)] ,
            [100,200,300,400]
        )

-- This is the same as pfsa3 from Assignment #8.
pfsa3 :: ProbFSA
pfsa3 = (   -- start distribution
            [(10,1.0)] ,
            -- end probabilities
            [(40,0.5)] ,
            -- transition probabilities
            [((10,20), 0.4), ((10,30), 0.6),
             ((20,40), 1.0),
             ((30,40), 1.0),
             ((40,10), 0.5)] ,
            -- emission probabilities
            [(((10,20),"a"), 0.2), (((10,20),"b"), 0.8), 
             (((10,30),"a"), 0.7), (((10,30),"c"), 0.3), 
             (((20,40),"c"), 0.6), (((20,40),"d"), 0.4), 
             (((30,40),"b"), 0.1), (((30,40),"d"), 0.9), 
             (((40,10),"e"), 1.0)] ,
            [10,20,30,40]
        )

-- A radically ambiguous grammar.
pfsa4 :: ProbFSA
pfsa4 = (   -- start distribution
            [(10,0.6), (20,0.4)] ,
            -- end probabilities
            [(10,0.2), (20,0.1)] ,
            -- transition probabilities
            [((10,10), 0.2), ((10,20), 0.6), 
             ((20,10), 0.7), ((20,20), 0.2)] ,
            -- emission probabilities
            [(((10,10),"a"), 0.2), (((10,10),"b"), 0.8), 
             (((10,20),"a"), 0.3), (((10,20),"b"), 0.7), 
             (((20,10),"a"), 0.6), (((20,10),"b"), 0.4), 
             (((20,20),"a"), 0.5), (((20,20),"b"), 0.5)] ,
            [10,20]
        )

--------------------------------------------------
-- Utility functions for getting information from grammars.

probLookup :: (Eq a) => [(a,Double)] -> a -> Double
probLookup []           key = 0.0
probLookup ((x,y):rest) key = if key == x then y else probLookup rest key

allStates :: ProbFSA -> [State]
allStates (starting,ending,transitions,emissions,states) = states

startProb :: ProbFSA -> State -> Double
startProb (starting,ending,transitions,emissions,states) = probLookup starting

endProb :: ProbFSA -> State -> Double
endProb (starting,ending,transitions,emissions,states) = probLookup ending

trProb :: ProbFSA -> State -> State -> Double
trProb (starting,ending,transitions,emissions,states) st1 st2 = probLookup transitions (st1,st2)

emProb :: ProbFSA -> (State,State) -> String -> Double
emProb (starting,ending,transitions,emissions,states) (st1,st2) str = probLookup emissions ((st1,st2),str)

-------------------------------------------------------------------------
-- Simple recursive definition of backward probabilities, like HW9

naiveBackward :: ProbFSA -> [String] -> State -> Double
naiveBackward g []     st = endProb g st
naiveBackward g (w:ws) st =
    sum (map (\next -> trProb g st next * emProb g (st,next) w * naiveBackward g ws next) (allStates g))

-------------------------------------------------------------------------
-- Now a dynamic programming / memoization approach to calculating 
-- backward probabilities.

-- A name for the specific Map type that we will use to represent 
-- a table of backward probabilities.
type BackwardTable = Map.Map ([String],State) Double

fastBackward :: ProbFSA -> [String] -> State -> Double
fastBackward g sent st =
    Map.findWithDefault 0 (sent,st) (buildBackwardTable g sent)

buildBackwardTable :: ProbFSA -> [String] -> BackwardTable
buildBackwardTable g sent =
    fillCellsBackward g Map.empty (cellsToFill g sent)

cellsToFill :: ProbFSA -> [String] -> [([String],State)]
cellsToFill g sent = [(suffix,state) | suffix <- suffixes sent, state <- allStates g]

-- Returns all the suffixes of a list, in longest-to-shortest order.
suffixes :: [a] -> [[a]]
suffixes xs = [drop n xs | n <- reverse [0 .. length xs]]

-- Gradually fills in the cells of the table, in the order specified by 
-- the list that is its third argument.
fillCellsBackward :: ProbFSA -> BackwardTable  -> [([String],State)] -> BackwardTable
fillCellsBackward g tbl [] = tbl
fillCellsBackward g tbl ((sentenceChunk,st):rest) =
    -- First calculate the probability for (sentenceChunk,st)
    let result =
            case sentenceChunk of
            [] -> endProb g st
            (w:ws) -> let probBackward = \ys -> \st -> Map.findWithDefault 0 (ys,st) tbl in
                      sum (map (\next -> trProb g st next * emProb g (st,next) w * probBackward ws next) (allStates g))
    in
    -- Now add the calculated probability to the table, if nonzero
    let updatedTbl =
            if (result > 0) then
                Map.insert (sentenceChunk,st) result tbl
            else
                tbl
    in
    -- Continue on with the rest of the cells to be filled
    fillCellsBackward g updatedTbl rest

