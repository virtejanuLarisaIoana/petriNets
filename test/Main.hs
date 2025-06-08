module Main (main) where

import Data.Tree
import qualified Data.Map as Map
import Data.List 
import Data.Maybe
import qualified Data.MetadataTracingQueue as MTQ
import Data.MetadataTracingQueue (MTQ, push)
import Data.PetriNet.SampleNets
import Data.PetriNet {-
    InputArc (InputArc, placeI, tokenI, transitionI),
    Marking (Marking),
    OutputArc (OutputArc, placeO, tokenO, transitionO),
    PetriNet (inputArcs, marking, outputArcs),
    Tokens (Tokens),
    TokensMD (TokensMD), 
    --fireTransition,
    isEnabled,
    --markingInputArcListSubtract,
    --markingInputArcSubtract,
    --markingOutputArcAddition,
    --markingOutputArcsListAddition,
    --markingTokensAddition,
    --markingTokensSubtract,
    --multipleTokensAddition,
    postSetArcs,
    preSetArcs,
    --singleTokenAddition,
    --tokensSatisfied,
    --tokensSubtract,
    tokensShiftSingle,
    --transitionPostSetAddition,
    --transitionPresetSubtract,
 )
import Data.PetriNet.SampleNets (
    genericTestToken0,
    genericTestToken1,
    genericTestToken2,
    genericTestToken3,
    testNet0,
    testNet0fired,
    testNet1,-}
 
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Petri Net Tests" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ --tokensSatisfiedTests
        --, presetTests
        --, postsetTests
        --, enabledTransitionTests
        firingTests
        --, mtqTests
        ]
{-
tokensSatisfiedTests :: TestTree
tokensSatisfiedTests =
    testGroup
        "Tokens Satisfied Tests"
        [ testCase "Generic tokens satisfied" $
            assertBool
                "Tokens not satisfied!"
                (tokensSatisfied genericTestToken1 genericTestToken2)
        , testCase "Generic tokens unsatisfied" $
            assertBool
                "Tokens are actually satisfied!"
                (not (tokensSatisfied genericTestToken2 genericTestToken3))
                -- testCase "Atomic Net P1 satisfies T1 tokens" $ assertBool "Tokens not satisfied!" (tokensSatisfied )
        ]

presetTests :: TestTree
presetTests =
    testGroup
        "Preset Tests"
        [ testCase "Atomic Net Preset" $
            preSetArcs testNet0 "T1"
                @?= [ InputArc
                        { placeI = "P1"
                        , transitionI = "T1"
                        , tokenI = Tokens $ Map.fromList [("x", 1)]
                        }
                    ]
        , testCase "Simple Net Preset on T1" $
            preSetArcs testNet1 "T1"
                @?= [ InputArc{placeI = "P1", transitionI = "T1", tokenI = Tokens $ Map.fromList [("x", 2)]}
                    , InputArc{placeI = "P2", transitionI = "T1", tokenI = Tokens $ Map.fromList [("x", 1), ("y", 2)]}
                    ]
        , testCase "Simple Net Preset on T2" $
            preSetArcs testNet1 "T2"
                @?= [InputArc{placeI = "P3", transitionI = "T2", tokenI = Tokens $ Map.fromList [("x", 1)]}]
        ]

postsetTests :: TestTree
postsetTests =
    testGroup
        "Postset Tests"
        [ testCase "Atomic Net Postset" $
            postSetArcs testNet0 "T1"
                @?= [ OutputArc
                        { transitionO = "T1"
                        , placeO = "P2"
                        , tokenO = Tokens $ Map.fromList [("x", 1)]
                        }
                    ]
        , testCase "Simple Net Postset on T1" $
            postSetArcs testNet1 "T1"
                @?= [ OutputArc
                        { transitionO = "T1"
                        , placeO = "P3"
                        , tokenO = Tokens $ Map.fromList [("x", 3)]
                        }
                    ]
        , testCase "Simple Net Postset on T2" $
            postSetArcs testNet1 "T2"
                @?= [ OutputArc
                        { transitionO = "T2"
                        , placeO = "P4"
                        , tokenO = Tokens $ Map.fromList [("x", 1)]
                        }
                    , OutputArc{transitionO = "T2", placeO = "P5", tokenO = Tokens $ Map.fromList [("y", 1)]}
                    ]
        ]

enabledTransitionTests :: TestTree
enabledTransitionTests =
    testGroup
        "Enabled Transition Tests"
        [ testCase "Atomic Net Enabled T1" $ isEnabled testNet0 "T1" @?= Just True
        , testCase "Simple Net Enabled T1" $ isEnabled testNet1 "T1" @?= Just True
        -- T2??
        ]
-}
firingTests :: TestTree
firingTests =
    testGroup
        "Firing Transitions Tests"
        [ tokenDelitionTests, tokenAdditionTests {-
        --, tokenAdditionTests
        --, testCase "Atomic Net T1 Fired" $ fireTransition testNet0 "T1" @?= Just testNet0fired
        , testCase "Simple Net T1 Fired" $
            fireTransition testNet1 "T1"
                @?= Just
                    testNet1
                        { marking =
                            Marking $
                                Map.fromList
                                    [ ("P1", Tokens $ Map.fromList [("x", 1)])
                                    , ("P2", Tokens $ Map.fromList [("z", 1)])
                                    , ("P3", Tokens $ Map.fromList [("x", 3)])
                                    , ("P4", Tokens $ Map.fromList [])
                                    , ("P5", Tokens $ Map.fromList [])
                                    ]
                        }
        , testCase "Simple Net T2 Fired" $
            fireTransition
                (Data.Maybe.fromMaybe testNet1 (fireTransition testNet1 "T1"))
                "T2"
                @?= Just
                    testNet1
                        { marking =
                            Marking $
                                Map.fromList
                                    [ ("P1", Tokens $ Map.fromList [("x", 1)])
                                    , ("P2", Tokens $ Map.fromList [("z", 1)])
                                    , ("P3", Tokens $ Map.fromList [("x", 2)])
                                    , ("P4", Tokens $ Map.fromList [("x", 1)])
                                    , ("P5", Tokens $ Map.fromList [("y", 1)])
                                    ]
                        }-}
        ]

tokenDelitionTests :: TestTree
tokenDelitionTests = testGroup "Token deletion tests" [singleTokenShift, listOfTokensShifting, markingTokensShiftingTests, tokensShiftedFromAnArc, tokensShiftedFromListOfArcs, transitionPresetShiftTest]

singleTokenShift :: TestTree
singleTokenShift =
    testGroup
        "Substract single token of a type"
        [ testCase "Generic Tokens Shifting on empty list" $ tokensShiftSingle "foo" 5  (TokensMD (Map.fromList []), genericMDTestToken1) @?= (
                                    TokensMD ( Map.fromList [ ("foo",   MTQ.fromList [(Node 1 [ Node 2 [] , Node 3 []], 3), (Node 1 [], 2)])]),
                                    TokensMD ( Map.fromList [ ("foo",   MTQ.fromList [(Node 1 [], 2)]), ("bar", MTQ.fromList [(Node 1 [], 2)])]) ),
        testCase "Generic Tokens Shifting already an element in the list" $ tokensShiftSingle "foo" 5 (TokensMD (Map.fromList [("foo", MTQ.fromList [(Node 1 [], 2)])]), genericMDTestToken1) @?= (
                                    TokensMD ( Map.fromList [ ("foo",   MTQ.fromList [(Node 1 [ Node 2 [] , Node 3 []], 3), (Node 1 [], 2), (Node 1 [], 2)])]),
                                    TokensMD ( Map.fromList [ ("foo",   MTQ.fromList [(Node 1 [], 2)]), ("bar", MTQ.fromList [(Node 1 [], 2)])]) )
        ]

listOfTokensShifting :: TestTree 
listOfTokensShifting = testGroup "Shift the corresponding tokens from two lists" [
    testCase "Generic tokens-tokens shifting on empty list" $ tokensShift genericTokensMDtupleEmpty genericTestToken1 @?=
                    (
                        TokensMD $ Map.fromList (rez1), 
                        TokensMD $ Map.fromList (rez2)
                    ),
    testCase "Generic tokens-tokens shifting on non-empty list" $ tokensShift genericTokensMDtuple genericTestToken1 @?= (
        TokensMD $ Map.fromList [("bar", MTQ.fromList [(Node 1 [], 2), (Node 1 [], 2) ]), ("foo",   MTQ.fromList [(Node 1 [ Node 2 [] , Node 3 []], 2), (Node 1 [], 5)])],
        TokensMD $ Map.fromList [("bar", MTQ.fromList [(Node 1 [], 2)]), ("baz", MTQ.fromList [(Node 16 [], 5), (Node 55 [Node 4 []], 10)]), ("foo", MTQ.fromList [(Node 1 [], 2), (Node 1 [Node 4 []], 1)])]
    )

    ]




markingTokensShiftingTests :: TestTree
markingTokensShiftingTests =
    testGroup
        "Shift a set of tokens from a given marking at a given place"
        [ testCase "Atomic net empty" $ markingTokensShift (TokensMD Map.empty , marking testNet0) "P1" (Tokens (Map.fromList [("x", 1)])) @?=
                                    ( TokensMD $ Map.fromList [("x",  MTQ.fromList [(Node 1 [], 1)] )]
                                    , Marking $ Map.fromList
                                                [ ("P1", TokensMD $ Map.empty)
                                                , ("P2", TokensMD $ Map.empty)] ),
            testCase "Simple net shift 2 x tokens from P1 empty" $ markingTokensShift (TokensMD Map.empty, marking testNet1) "P1"(Tokens $ Map.fromList [("x", 2)]) @?=
                ( TokensMD $ Map.fromList [("x", MTQ.fromList [(Node 14 [Node 1 [], Node 2[]], 2)])]
                , Marking $ Map.fromList
                    [ ("P1", TokensMD $ Map.fromList [("x", MTQ.fromList [(Node 14 [Node 1 [], Node 2[]], 1)])])
                    , ("P2", TokensMD $ Map.fromList [("x", MTQ.fromList[(Node 2 [],1)])
                                                    , ("y",MTQ.fromList[(Node 2 [Node 5 []], 2)])
                                                    , ("z", MTQ.fromList[(Node 3[], 1)])])
                    , ("P3", TokensMD Map.empty)
                    , ("P4", TokensMD Map.empty)
                    , ("P5", TokensMD Map.empty)
                    ]
                ),
            testCase "Simple net shift 1 x, 2 y from P2 after P1 shift" $   let (tokens1, marking1) = markingTokensShift (TokensMD Map.empty, marking testNet1) "P1" (Tokens $ Map.fromList [("x", 2)])
                    in markingTokensShift (tokens1, marking1) "P2" (Tokens $ Map.fromList [("x", 1), ("y", 2)]) @?= 
                    ( TokensMD $ Map.fromList
                        [ ("x", MTQ.fromList [(Node 2 [], 1), (Node 14 [Node 1 [], Node 2[]], 2)])
                        , ("y", MTQ.fromList [(Node 2 [Node 5 []], 2)])
                        ]
                    , Marking $ Map.fromList
                        [ ("P1", TokensMD $ Map.fromList [("x", MTQ.fromList [(Node 14 [Node 1 [], Node 2[]], 1)])])
                        , ("P2", TokensMD $ Map.fromList [("z", MTQ.fromList[(Node 3[], 1)])])
                        , ("P3", TokensMD Map.empty)
                        , ("P4", TokensMD Map.empty)
                        , ("P5", TokensMD Map.empty)
                        ]
                    )
        ]

tokensShiftedFromAnArc :: TestTree
tokensShiftedFromAnArc =
    testGroup
        "Shift a set of tokens from a marking given an Input Arc"
        [ testCase "Atomic net" $ markingInputArcShift (TokensMD Map.empty, marking testNet0) InputArc{placeI = "P1", transitionI = "T1", tokenI = Tokens $ Map.fromList [("x", 1)]} @?= 
                    ( TokensMD $ Map.fromList [("x",  MTQ.fromList [(Node 1 [], 1)] )]
                                    , Marking $ Map.fromList
                                                [ ("P1", TokensMD $ Map.empty)
                                                , ("P2", TokensMD $ Map.empty)] )
        ]


tokensShiftedFromListOfArcs :: TestTree
tokensShiftedFromListOfArcs =
    testGroup
        "Shift tokens and update marking for a list of arcs"
        [ testCase "Simple net P1 P2 arcs" $ markingInputArcListShift (marking testNet1) [ InputArc "P1" "T1" (Tokens $ Map.fromList [("x", 2)]), InputArc "P2" "T1" (Tokens $ Map.fromList [("x", 1), ("y", 2)])] @?=
        ( TokensMD $ Map.fromList
            [ ("x", MTQ.fromList [(Node 2 [], 1), (Node 14 [Node 1 [], Node 2 []], 2) ])
            , ("y", MTQ.fromList [(Node 2 [Node 5 []], 2) ])]
        , Marking $ Map.fromList
            [ ("P1", TokensMD $ Map.fromList [("x", MTQ.fromList [ (Node 14 [Node 1 [], Node 2 []], 1)])])
            , ("P2", TokensMD $ Map.fromList [("z", MTQ.fromList [(Node 3 [], 1)])])
            , ("P3", TokensMD Map.empty)
            , ("P4", TokensMD Map.empty)
            , ("P5", TokensMD Map.empty)
        ]
    )
            ]

transitionPresetShiftTest :: TestTree 
transitionPresetShiftTest = testGroup "Shift tokens for firing a certain transition" [
    testCase "Simple net T1 fired" $ transitionPresetShift testNet1 "T1" @?= ( TokensMD $ Map.fromList
        [("x", MTQ.fromList[(Node 2 [], 1), (Node 14 [Node 1 [], Node 2 []], 2)])
        ,("y", MTQ.fromList[(Node 2 [Node 5 []], 2) ]) ]
    , testNet1
        { marking = Marking $ Map.fromList
            [ ("P1", TokensMD $ Map.fromList[("x", MTQ.fromList[(Node 14 [Node 1 [], Node 2 []], 1)])])
            , ("P2", TokensMD $ Map.fromList[("z", MTQ.fromList[(Node 3 [], 1)])])
            , ("P3", TokensMD Map.empty)
            , ("P4", TokensMD Map.empty)
            , ("P5", TokensMD Map.empty)
            ]
        }
    )
    ]

tokenAdditionTests :: TestTree
tokenAdditionTests = testGroup "Shifting tokens for the output arcs"[singleTokenAdditionTest, tokensShiftedAdditionTests, markingTokensPostsetShiftTests, fullTestCaseFiring]

singleTokenAdditionTest :: TestTree
singleTokenAdditionTest = testGroup "Adding a single token to a new Tokens <list>"[
    testCase "Generic token, non-empty second Tokens" $ tokenPostShiftSingle  222 "foo" 3 genericTokensMDtuple @?=
        (TokensMD $ Map.fromList[("bar", MTQ.fromList [(Node 1 [], 2)])], 
            TokensMD $ Map.fromList [ ("bar", MTQ.fromList [(Node 1 [], 4)]),
                ("baz", MTQ.fromList [(Node 16 [], 5), (Node 55 [Node 4 []], 10)]),
                ("foo", MTQ.fromList [(Node 1 [ Node 2 [] , Node 3 []], 2), (Node 1 [], 7), (Node 1 [Node 4 []], 1), (Node 222 [Node 1[]], 3)])] ),
    testCase "Generic token, empty second Tokens" $ tokenPostShiftSingle 333 "foo" 100 genericTokensMDtupleEmptySnd @?=
        (
            TokensMD $ Map.fromList[("bar", MTQ.fromList [(Node 1 [], 4)]), ("baz", MTQ.fromList [(Node 16 [], 5)])],
            TokensMD $ Map.fromList[("foo", MTQ.fromList[(Node 333 [Node 1 [], Node 16 []], 100)])]
        )
    ]
tokensShiftedAdditionTests :: TestTree
tokensShiftedAdditionTests = testGroup "Adding multiple tokens to a new Tokens <list>" [
    testCase "Generic tokens" $ tokensPostShift 444 (TokensMD $ Map.fromList [ ("bar", MTQ.fromList [(Node 1 [], 4)])] , TokensMD $ Map.empty) genericTestToken1 @?= 
        (TokensMD $ Map.fromList [ ("bar", MTQ.fromList [(Node 1 [], 4)])] 
        ,TokensMD $ Map.fromList[("bar", MTQ.fromList [(Node 444 [Node 1 []], 2)]), ("foo", MTQ.fromList [(Node 444 [Node 1 []], 7)])])
    ]

markingTokensPostsetShiftTests :: TestTree
markingTokensPostsetShiftTests = testGroup "Update marking after postShifting for one  place"[
    testCase "Simple net marking" $ markingTokensPostShift 555 (TokensMD $ Map.fromList [("x",  MTQ.fromList [(Node 1 [], 1)])], marking testNet0) "P2" (Tokens (Map.fromList [("x", 1)])) @?=
        (
            TokensMD $ Map.fromList [("x",  MTQ.fromList [(Node 1 [], 1)])],
            Marking $ Map.fromList
                    [ ("P1", TokensMD $ Map.fromList [("x",  MTQ.fromList [(Node 1 [], 1)] )])
                    , ("P2", TokensMD $ Map.fromList [("x", MTQ.fromList[(Node 555 [Node 1 []], 1)])])]
                    )
    ]

fullTestCaseFiring :: TestTree 
fullTestCaseFiring = testGroup "Firing on polimorfic metadata nets"[
    testCase "Atomic net" $ transitionPostSetShift 1111 testNet0 "T1" @?= testNet0{marking = Marking $
                Map.fromList
                    [ ("P1", TokensMD $ Map.empty )
                    , ("P2", TokensMD $ Map.fromList [("x",  MTQ.fromList [(Node 1111 [Node 1 []], 1)])])
                    ]},
    testCase "Simple net fired of T1" $ transitionPostSetShift 7777 testNet1 "T1" @?= testNet1{marking = Marking $
                Map.fromList[
                    ("P1", TokensMD $ Map.fromList [("x", MTQ.fromList[(Node 14 [Node 1 [], Node 2[]], 1)] )])
                    , ("P2", TokensMD $ Map.fromList [("z", MTQ.fromList[(Node 3[], 1)])])
                    , ("P3", TokensMD $ Map.fromList [("x", MTQ.fromList[(Node 7777 [Node 2 [] ,Node 14 [Node 1 [], Node 2[]], Node 3[]], 3)])])
                    , ("P4", TokensMD $ Map.fromList [])
                    , ("P5", TokensMD $ Map.fromList [])
                ]

    }rootLabel = 7777, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 14, subForest = [Node {rootLabel = 1, subForest = []},Node {rootLabel = 2, subForest = []}]},Node {rootLabel = 2, subForest = [Node {rootLabel = 5, subForest = []}]}]},3)]})]})
    }rootLabel = 7777, subForest = [Node {rootLabel = 14, subForest = [Node {rootLabel = 1, subForest = []},Node {rootLabel = 2, subForest = []}]},Node {rootLabel = 3, subForest = []}]},3)]}

{-}


fullTokenSubstractionFiredTransition :: TestTree
fullTokenSubstractionFiredTransition =
    testGroup
        "Delete corresponding tokens for firing"
        [ testCase "Atomic net" $ transitionPresetSubtract testNet0 "T1" @?= testNet0{marking = Marking (Map.fromList [("P1", Tokens $ Map.fromList []), ("P2", Tokens $ Map.fromList [])])}
        ]

tokenAdditionTests :: TestTree
tokenAdditionTests = testGroup "Tokens addition tests" [singleTokenAdditionTest, tokenListAddition, markingTokensAdditionTests, tokenAdditionFromAnOutputArc, tokenAdditionFromListOfArcs, fullTokenAdditionFiredTransition]

singleTokenAdditionTest :: TestTree
singleTokenAdditionTest =
    testGroup
        "Single token addition"
        [ testCase "Generic token addition" $ singleTokenAddition "foo" 2 genericTestToken1 @?= Tokens (Map.fromList [("foo", 5), ("bar", 4)])
        ]

tokenListAddition :: TestTree
tokenListAddition =
    testGroup
        "Adding corresponding tokens from two <<lists>>"
        [ testCase "Generic tokens-tokens addition" $ multipleTokensAddition genericTestToken1 genericTestToken2 @?= Tokens (Map.fromList [("foo", 13), ("bar", 8), ("baz", 10)])
        ]

markingTokensAdditionTests :: TestTree
markingTokensAdditionTests =
    testGroup
        "Add a set of tokens from a given place from a marking"
        [ testCase "Atomic net" $ markingTokensAddition (marking testNet0) "P2" (Tokens (Map.fromList [("x", 1)])) @?= Marking (Map.fromList [("P1", Tokens $ Map.fromList [("x", 1)]), ("P2", Tokens $ Map.fromList [("x", 1)])])
        -- needs the rest cases
        ]

tokenAdditionFromAnOutputArc :: TestTree
tokenAdditionFromAnOutputArc =
    testGroup
        "Adds a set of tokens from a given Output Arc to the marking"
        [ testCase "Atomic net" $ markingOutputArcAddition (marking testNet0) OutputArc{transitionO = "T1", placeO = "P2", tokenO = Tokens $ Map.fromList [("x", 1)]} @?= Marking (Map.fromList [("P1", Tokens $ Map.fromList [("x", 1)]), ("P2", Tokens $ Map.fromList [("x", 1)])])
        , testCase "Simple Net fst output arc" $
            markingOutputArcAddition (marking testNet1) OutputArc{transitionO = "T1", placeO = "P3", tokenO = Tokens $ Map.fromList [("x", 3)]}
                @?= Marking
                    ( Map.fromList
                        [ ("P1", Tokens $ Map.fromList [("x", 3)])
                        , ("P2", Tokens $ Map.fromList [("x", 1), ("y", 2), ("z", 1)])
                        , ("P3", Tokens $ Map.fromList [("x", 3)])
                        , ("P4", Tokens $ Map.fromList [])
                        , ("P5", Tokens $ Map.fromList [])
                        ]
                    )
        , testCase "Simple Net 2nd output arc" $
            markingOutputArcAddition (marking testNet1) OutputArc{transitionO = "T2", placeO = "P4", tokenO = Tokens $ Map.fromList [("x", 1)]}
                @?= Marking
                    ( Map.fromList
                        [ ("P1", Tokens $ Map.fromList [("x", 3)])
                        , ("P2", Tokens $ Map.fromList [("x", 1), ("y", 2), ("z", 1)])
                        , ("P3", Tokens $ Map.fromList [])
                        , ("P4", Tokens $ Map.fromList [("x", 1)])
                        , ("P5", Tokens $ Map.fromList [])
                        ]
                    )
        ]

tokenAdditionFromListOfArcs :: TestTree
tokenAdditionFromListOfArcs =
    testGroup
        "Add tokens and update marking for a list of arcs"
        [ testCase "Atomic net" $ markingOutputArcsListAddition (marking testNet0) (outputArcs testNet0) @?= Marking (Map.fromList [("P1", Tokens $ Map.fromList [("x", 1)]), ("P2", Tokens $ Map.fromList [("x", 1)])])
        , testCase "Simple net" $
            markingOutputArcsListAddition (marking testNet1) (outputArcs testNet1)
                @?= Marking
                    ( Map.fromList
                        [ ("P1", Tokens $ Map.fromList [("x", 3)])
                        , ("P2", Tokens $ Map.fromList [("x", 1), ("y", 2), ("z", 1)])
                        , ("P3", Tokens $ Map.fromList [("x", 3)])
                        , ("P4", Tokens $ Map.fromList [("x", 1)])
                        , ("P5", Tokens $ Map.fromList [("y", 1)])
                        ]
                    )
        ]

fullTokenAdditionFiredTransition :: TestTree
fullTokenAdditionFiredTransition =
    testGroup
        "Add coresponding tokens for firing"
        [ testCase "Atomic net" $
            transitionPostSetAddition testNet0 "T1"
                @?= testNet0
                    { marking =
                        Marking $
                            Map.fromList
                                [ ("P1", Tokens $ Map.fromList [("x", 1)])
                                , ("P2", Tokens $ Map.fromList [("x", 1)])
                                ]
                    }
        , testCase "Simple net T1 fired" $
            transitionPostSetAddition testNet1 "T1"
                @?= testNet1
                    { marking =
                        Marking $
                            Map.fromList
                                [ ("P1", Tokens $ Map.fromList [("x", 3)])
                                , ("P2", Tokens $ Map.fromList [("x", 1), ("y", 2), ("z", 1)])
                                , ("P3", Tokens $ Map.fromList [("x", 3)])
                                , ("P4", Tokens $ Map.fromList [])
                                , ("P5", Tokens $ Map.fromList [])
                                ]
                    }
        , testCase "Simple net T2 fired" $
            transitionPostSetAddition testNet1 "T2"
                @?= testNet1
                    { marking =
                        Marking $
                            Map.fromList
                                [ ("P1", Tokens $ Map.fromList [("x", 3)])
                                , ("P2", Tokens $ Map.fromList [("x", 1), ("y", 2), ("z", 1)])
                                , ("P3", Tokens $ Map.fromList [])
                                , ("P4", Tokens $ Map.fromList [("x", 1)])
                                , ("P5", Tokens $ Map.fromList [("y", 1)])
                                ]
                    }
        ]
mtqTests :: TestTree
mtqTests =
    testGroup "Metadata Tracing Queue Tests" $
        [ testCase "Empty Queue has length 0" $ MTQ.lengthQ MTQ.empty @?= 0
        , testCase "fromList" $ MTQ.fromList [("foo", 3), ("bar", 5)] @?=
            MTQ.push "bar" 5 (MTQ.push "foo" 3 MTQ.empty)
        , testGroup "MTQ.take" $ 
            [   
            testCase "take 0 returns an empty queue" $ MTQ.take 0 mtqSample1 @?= MTQ.empty
            , testCase "take 1 mtqSample1" $ MTQ.take 1 mtqSample1 @?= MTQ.fromList [("foo",1)]
            , testCase "take 3 mtqSample1" $ MTQ.take 3 mtqSample1 @?= MTQ.fromList [("foo",3)]
            , testCase "take 5 mtqSample1" $ MTQ.take 5 mtqSample1 @?= MTQ.fromList [("foo",3), ("bar",2)]
            , testCase "take all mtqSample1" $ MTQ.take (MTQ.lengthQ mtqSample1) mtqSample1 @?= mtqSample1
            ]
        , testGroup "MTQ.drop" $     
            [   
            testCase "drop 0 returns the queue" $ MTQ.drop 0 mtqSample1 @?= mtqSample1
            , testCase "drop 1 mtqSample1" $ MTQ.drop 1 mtqSample1 @?= MTQ.fromList [("foo",2), ("bar",10), ("qux",15)]
            , testCase "drop 3 mtqSample1" $ MTQ.drop 3 mtqSample1 @?= MTQ.fromList [("bar",10), ("qux",15)]
            , testCase "drop 5 mtqSample1" $ MTQ.drop 5 mtqSample1 @?= MTQ.fromList [("bar",8), ("qux", 15)]
            , testCase "drop all mtqSample1" $ MTQ.drop (MTQ.lengthQ mtqSample1) mtqSample1 @?= MTQ.empty
            ]
        , testGroup "MTQ.split" $
            [ testCase "split 0" $ MTQ.split 0 mtqSample1 @?= (MTQ.empty, mtqSample1)
            , testCase "split 1 mtqSample1" $ MTQ.split 1 mtqSample1 @?= 
                (MTQ.fromList [("foo",1)], MTQ.fromList [("foo",2), ("bar",10), ("qux",15)])
            , testCase "split 3 mtqSample1" $ MTQ.split 3 mtqSample1 @?= 
                (MTQ.fromList [("foo",3)], MTQ.fromList [("bar",10), ("qux",15)])
            , testCase "split 5 mtqSample1" $ MTQ.split 5 mtqSample1 @?= 
                (MTQ.fromList [("foo",3),("bar",2)], MTQ.fromList [("bar",8), ("qux", 15)])
            , testCase "split all mtqSample1" $ MTQ.split (MTQ.lengthQ mtqSample1) mtqSample1 @?= (mtqSample1, MTQ.empty)
            ]
        ]
       where
        mtqSample1 :: MTQ String
        mtqSample1 = MTQ.fromList [("foo", 3), ("bar", 10), ("qux", 15)]
-}