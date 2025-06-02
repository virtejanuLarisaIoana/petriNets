module Data.PetriNet.SampleNets where

import Data.PetriNet 

genericTestToken1 :: Tokens
genericTestToken1 = Tokens [Token 3 "foo", Token 4 "bar"]

genericTestToken2 :: Tokens
genericTestToken2 = Tokens [Token 10 "foo", Token 4 "bar", Token 10 "baz"]

genericTestToken3 :: Tokens
genericTestToken3 = Tokens [Token 1 "foo", Token 4 "bar", Token 15 "baz"]



testNet0 :: PetriNet
testNet0 = PetriNet {
    places = ["P1", "P2"], 
    transitions = ["T1"],
    inputArcs = [InputArc {placeI = "P1", transitionI = "T1", tokenI = Tokens [Token 1 "x"]}],
    outputArcs = [OutputArc {transitionO = "T1", placeO = "P2", tokenO = Tokens [Token 1 "x"]}],
    marking = [
        ("P1", Tokens [Token 1 "x"]),
        ("P2", Tokens [])
        ]
}

testNet0fired :: PetriNet
testNet0fired = PetriNet {
    places = ["P1", "P2"], 
    transitions = ["T1"],
    inputArcs = [InputArc {placeI = "P1", transitionI = "T1", tokenI = Tokens [Token 1 "x"]}],
    outputArcs = [OutputArc {transitionO = "T1", placeO = "P2", tokenO = Tokens [Token 1 "x"]}],
    marking = [
        ("P1", Tokens []),
        ("P2", Tokens [Token 1 "x"])
        ]
}

testNet1 :: PetriNet
testNet1 = PetriNet {
    places = ["P1", "P2", "P3", "P4", "P5"], 
    transitions = ["T1", "T2"],
    inputArcs = [
        InputArc {placeI = "P1", transitionI = "T1", tokenI = Tokens [ Token 2 "x" ]},
        InputArc {placeI = "P2", transitionI = "T1", tokenI = Tokens [ Token 1 "x" , Token 2 "y" ]},
        InputArc {placeI = "P3", transitionI = "T2", tokenI = Tokens [ Token 1 "x"]}
    ],
    outputArcs = [
        OutputArc { transitionO = "T1", placeO = "P3", tokenO = Tokens [Token 3 "x"]},
        OutputArc { transitionO = "T2", placeO = "P4", tokenO = Tokens [Token 1 "x"]},
        OutputArc { transitionO = "T2", placeO = "P5", tokenO = Tokens [Token 1 "y"]}
    ],
    marking = [
        ("P1", Tokens [Token 3 "x"]),
        ("P2", Tokens [Token 1 "x", Token 2 "y", Token 1 "z"]),
        ("P3", Tokens []),
        ("P4", Tokens []),
        ("P5", Tokens [])
    ]

}