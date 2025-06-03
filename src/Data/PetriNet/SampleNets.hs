module Data.PetriNet.SampleNets where

import Data.PetriNet 
import qualified Data.Map as Map 

genericTestToken0 :: (String, Integer)
genericTestToken0 = ("foo", 2)

genericTestToken1 :: Tokens
genericTestToken1 = Tokens $ Map.fromList [("foo", 3), ("bar", 4)]

genericTestToken2 :: Tokens
genericTestToken2 = Tokens $ Map.fromList [("foo", 10), ("bar", 4), ("baz", 10)]

genericTestToken3 :: Tokens
genericTestToken3 = Tokens $ Map.fromList[("foo", 1), ("bar", 4), ("baz", 15)]



testNet0 :: PetriNet
testNet0 = PetriNet {
    places = ["P1", "P2"], 
    transitions = ["T1"],
    inputArcs = [InputArc {placeI = "P1", transitionI = "T1", tokenI = Tokens $ Map.fromList[("x",1)]}],
    outputArcs = [OutputArc {transitionO = "T1", placeO = "P2", tokenO = Tokens $ Map.fromList[("x",1)]}],
    marking = Marking $ Map.fromList [
        ("P1", Tokens $ Map.fromList[("x",1)]),
        ("P2", Tokens $ Map.fromList[])
        ]
}

testNet0fired :: PetriNet
testNet0fired = PetriNet {
    places = ["P1", "P2"], 
    transitions = ["T1"],
    inputArcs = [InputArc {placeI = "P1", transitionI = "T1", tokenI = Tokens $ Map.fromList[("x",1)]}],
    outputArcs = [OutputArc {transitionO = "T1", placeO = "P2", tokenO = Tokens $ Map.fromList[("x",1)]}],
    marking = Marking $ Map.fromList [
        ("P1", Tokens $ Map.fromList[]),
        ("P2", Tokens $ Map.fromList[("x",1)])
        ]
}

testNet1 :: PetriNet
testNet1 = PetriNet {
    places = ["P1", "P2", "P3", "P4", "P5"], 
    transitions = ["T1", "T2"],
    inputArcs = [
        InputArc {placeI = "P1", transitionI = "T1", tokenI = Tokens $ Map.fromList[("x",2) ]},
        InputArc {placeI = "P2", transitionI = "T1", tokenI = Tokens $ Map.fromList[ ("x",1) , ("y",2) ]},
        InputArc {placeI = "P3", transitionI = "T2", tokenI = Tokens $ Map.fromList[("x",1)]}
    ],
    outputArcs = [
        OutputArc { transitionO = "T1", placeO = "P3", tokenO = Tokens $ Map.fromList[("x",3)]},
        OutputArc { transitionO = "T2", placeO = "P4", tokenO = Tokens $ Map.fromList[("x",1)]},
        OutputArc { transitionO = "T2", placeO = "P5", tokenO = Tokens $ Map.fromList[("y",1)]}
    ],
    marking = Marking $ Map.fromList[
        ("P1", Tokens $ Map.fromList[("x",3)]),
        ("P2", Tokens $ Map.fromList[("x",1), ("y", 2), ("z",1)]),
        ("P3", Tokens $ Map.fromList[]),
        ("P4", Tokens $ Map.fromList[]),
        ("P5", Tokens $ Map.fromList[])
    ]

}