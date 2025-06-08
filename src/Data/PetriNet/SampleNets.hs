module Data.PetriNet.SampleNets where
import Data.List
import qualified Data.Map as Map
import Data.PetriNet
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|))
import Data.MetadataTracingQueue as MTQ
import Data.MetadataTracingQueue (MTQ)
import Data.Tree


genericTestToken0 :: (String, Integer)
genericTestToken0 = ("foo", 2)

rez1 = sortOn fst [("foo",   MTQ.fromList [(Node 1 [ Node 2 [] , Node 3 []], 2), (Node 1 [], 5)]),
                                                ("bar", MTQ.fromList [(Node 1 [], 2)])]
rez2 = sortOn fst [("foo",   MTQ.fromList [(Node 1 [], 2), (Node 1 [Node 4 []], 1)]),
                                                ("bar", MTQ.fromList [(Node 1 [], 2)]),
                                                ("baz", MTQ.fromList [(Node 16 [], 5), (Node 55 [Node 4 []], 10)])]

mytest :: Integer -> [(String, MTQ Integer)]
mytest x = rez1 

genericMDTestToken1 :: TokensMD Integer         --foo 7 bar 2
genericMDTestToken1  = TokensMD $ Map.fromList [("foo",   MTQ.fromList [(Node 1 [ Node 2 [] , Node 3 []], 3), (Node 1 [], 4)]),
                                                ("bar", MTQ.fromList [(Node 1 [], 2)])]

genericMDTestToken2 :: TokensMD Integer         --foo 10 bar 4 baz 15
genericMDTestToken2  = TokensMD $ Map.fromList [("foo",   MTQ.fromList [(Node 1 [ Node 2 [] , Node 3 []], 2), (Node 1 [], 7), (Node 1 [Node 4 []], 1)]),
                                                ("bar", MTQ.fromList [(Node 1 [], 4)]),
                                                ("baz", MTQ.fromList [(Node 16 [], 5), (Node 55 [Node 4 []], 10)])]
genericTokensMDtuple :: (TokensMD Integer, TokensMD Integer)
genericTokensMDtuple =  (TokensMD $ Map.fromList[("bar", MTQ.fromList [(Node 1 [], 2)])], genericMDTestToken2)


genericTokensMDtupleEmpty :: (TokensMD Integer, TokensMD Integer)
genericTokensMDtupleEmpty =  (TokensMD Map.empty, genericMDTestToken2)

genericTokensMDtupleEmptySnd :: (TokensMD Integer, TokensMD Integer)
genericTokensMDtupleEmptySnd =  (TokensMD $ Map.fromList[("bar", MTQ.fromList [(Node 1 [], 4)]),
                                                ("baz", MTQ.fromList [(Node 16 [], 5)])], TokensMD Map.empty)

genericTestToken1 :: Tokens
genericTestToken1 = Tokens $ Map.fromList [("foo", 7), ("bar", 2)]


genericTestToken2 :: Tokens
genericTestToken2 = Tokens $ Map.fromList [("foo", 10), ("bar", 4), ("baz", 10)]

genericTestToken3 :: Tokens
genericTestToken3 = Tokens $ Map.fromList [("foo", 1), ("bar", 4), ("baz", 15)]

testNet0 :: PetriNet Integer
testNet0 =
    PetriNet
        { places = ["P1", "P2"]
        , transitions = ["T1"]
        , inputArcs = [InputArc{placeI = "P1", transitionI = "T1", tokenI = Tokens $ Map.fromList [("x", 1)]}]
        , outputArcs = [OutputArc{transitionO = "T1", placeO = "P2", tokenO = Tokens $ Map.fromList [("x", 1)]}]
        , marking =
            Marking $
                Map.fromList
                    [ ("P1", TokensMD $ Map.fromList [("x",  MTQ.fromList [(Node 1 [], 1)] )])
                    , ("P2", TokensMD $ Map.fromList [])
                    ]
        }
{-
    

testNet0fired :: PetriNet metadata
testNet0fired =
    PetriNet
        { places = ["P1", "P2"]
        , transitions = ["T1"]
        , inputArcs = [InputArc{placeI = "P1", transitionI = "T1", tokenI = Tokens $ Map.fromList [("x", 1)]}]
        , outputArcs = [OutputArc{transitionO = "T1", placeO = "P2", tokenO = Tokens $ Map.fromList [("x", 1)]}]
        , marking =
            Marking $
                Map.fromList
                    [ ("P1", Tokens $ Map.fromList [])
                    , ("P2", Tokens $ Map.fromList [("x", 1)])
                    ]
        } -}

testNet1 :: PetriNet Integer
testNet1 =
    PetriNet
        { places = ["P1", "P2", "P3", "P4", "P5"]
        , transitions = ["T1", "T2"]
        , inputArcs =
            [ InputArc{placeI = "P1", transitionI = "T1", tokenI = Tokens $ Map.fromList [("x", 2)]}
            , InputArc{placeI = "P2", transitionI = "T1", tokenI = Tokens $ Map.fromList [("x", 1), ("y", 2)]}
            , InputArc{placeI = "P3", transitionI = "T2", tokenI = Tokens $ Map.fromList [("x", 1)]}
            ]
        , outputArcs =
            [ OutputArc{transitionO = "T1", placeO = "P3", tokenO = Tokens $ Map.fromList [("x", 3)]}
            , OutputArc{transitionO = "T2", placeO = "P4", tokenO = Tokens $ Map.fromList [("x", 1)]}
            , OutputArc{transitionO = "T2", placeO = "P5", tokenO = Tokens $ Map.fromList [("y", 1)]}
            ]
        , marking =
            Marking $
                Map.fromList
                    [ ("P1", TokensMD $ Map.fromList [("x", MTQ.fromList[(Node 14 [Node 1 [], Node 2[]], 3)] )])
                    , ("P2", TokensMD $ Map.fromList [("x", MTQ.fromList[(Node 2 [],1)])
                                                    , ("y",MTQ.fromList[(Node 2 [Node 5 []], 2)] )
                                                    , ("z", MTQ.fromList[(Node 3[], 1)])])
                    , ("P3", TokensMD $ Map.fromList [])
                    , ("P4", TokensMD $ Map.fromList [])
                    , ("P5", TokensMD $ Map.fromList [])
                    ]
        }
