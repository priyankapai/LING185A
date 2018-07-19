module ProbFSA where

type State = Int

type ProbFSA = ([(State,Double)],                   -- start distribution
                [(State,Double)],                   -- ending probabilities
                [((State,State),Double)],           -- transition probabilities
                [(((State,State),String),Double)],  -- emissions
                [State])                            -- all states

data StrucDesc = End State | Step State String StrucDesc deriving Show

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

-- This is the grammar on page 3 of the handout from class this week.
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

-- This is the grammar on page 5 of the handout from class this week.
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

--------------------------------------------------

-- Discussed in class on Wed 2/28
probForward :: ProbFSA -> [String] -> State -> Double
probForward pfsa output st =
    if output == [] then
        startProb pfsa st
    else
        let (ws,w) = (init output, last output) in
        sum (map (\prev -> probForward pfsa ws prev * trProb pfsa prev st * emProb pfsa (prev,st) w) (allStates pfsa))

--------------------------------------------------
--------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your new code below.

probBackward :: ProbFSA -> [String] -> State -> Double
probBackward pfsa output st =
    if output == [] then
        endProb pfsa st
    else
        let (ws,w) = (head output, tail output) in
        sum (map (\next -> trProb pfsa st next * emProb pfsa (st,next) ws * probBackward pfsa w next) (allStates pfsa))

viterbiProbForward :: ProbFSA -> [String] -> State -> Double
viterbiProbForward pfsa output st =
    if output == [] then
        startProb pfsa st
    else
        let (ws,w) = (init output, last output) in
        maximum (map (\prev -> viterbiProbForward pfsa ws prev * trProb pfsa prev st * emProb pfsa (prev,st) w) (allStates pfsa))

viterbiPairForward :: ProbFSA -> [String] -> State -> (State,Double)
viterbiPairForward pfsa output st =
    if output == [] then
        (undefined, startProb pfsa st)
    else 
       let (ws,w) = (init output, last output) in 
       viterbiPairHelper (zip (map (\prev -> viterbiProbForward pfsa ws prev * trProb pfsa prev st * emProb pfsa (prev,st) w) (allStates pfsa)) (allStates pfsa))

viterbiPairHelper ::(Ord a) => [(Double,a)] -> (a,Double)
viterbiPairHelper lst = 
    let result = maximum lst in
    (snd result, fst result)
    
viterbiProbBackward :: ProbFSA -> [String] -> State -> Double
viterbiProbBackward pfsa output st =
    if output == [] then 
       endProb pfsa st
    else 
       let (ws,w) = (head output, tail output) in
       maximum (map (\next -> trProb pfsa st next * emProb pfsa (st,next) ws * viterbiProbBackward pfsa w next) (allStates pfsa))

---------- Functions for viterbiStrucDesc -----------

viterbiStrucHelper :: ProbFSA -> [String]-> State -> State
viterbiStrucHelper pfsa output st =
    if output == [] then
       undefined
    else
       let (ws,w) = (head output, tail output) in
       fst (viterbiPairHelper (zip (map (\next -> trProb pfsa st next * emProb pfsa (st,next) ws * viterbiProbBackward pfsa w next) (allStates pfsa)) (allStates pfsa)))
       

viterbiStrucDesc :: ProbFSA -> [String] -> State -> (StrucDesc,Double)
viterbiStrucDesc pfsa output st = 
    if output == [] then 
      ((End st), 0.0)
    else 
      (((viterbiBuildStruc pfsa output st)), (viterbiProbBackward pfsa output st))

viterbiBuildStruc :: ProbFSA -> [String] -> State -> StrucDesc
viterbiBuildStruc pfsa [ws] st = (Step st ws (End (viterbiStrucHelper pfsa [ws] st)))
viterbiBuildStruc pfsa (ws:w) st = (Step st ws (viterbiBuildStruc pfsa w (viterbiStrucHelper pfsa (ws:w) st)))


