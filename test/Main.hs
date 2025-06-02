module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.PetriNet 
import Data.PetriNet.SampleNets
import Data.Maybe


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Petri Net Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [tokensSatisfiedTests, presetTests, postsetTests, enabledTransitionTests, firingTests]

tokensSatisfiedTests :: TestTree 
tokensSatisfiedTests = testGroup "Tokens Satisfied Tests" [
    testCase "Generic tokens satisfied" $ assertBool "Tokens not satisfied!" (tokensSatisfied genericTestToken1 genericTestToken2),
    testCase "Generic tokens unsatisfied" $ assertBool "Tokens are actually satisfied!" (not (tokensSatisfied genericTestToken2 genericTestToken3))
    --testCase "Atomic Net P1 satisfies T1 tokens" $ assertBool "Tokens not satisfied!" (tokensSatisfied )
    ]

presetTests :: TestTree
presetTests = testGroup "Preset Tests" [
    testCase "Atomic Net Preset" $ preSet testNet0 "T1" @?= [InputArc {placeI = "P1", transitionI = "T1", tokenI = Tokens [Token 1 "x"]}],
    testCase "Simple Net Preset on T1" $ preSet testNet1 "T1" @?= [InputArc {placeI = "P1", transitionI = "T1", tokenI = Tokens [Token {numberT = 2, typeT = "x"}]},InputArc {placeI = "P2", transitionI = "T1", tokenI = Tokens [Token {numberT = 1, typeT = "x"},Token {numberT = 2, typeT = "y"}]}],
    testCase "Simple Net Preset on T2" $ preSet testNet1 "T2" @?= [InputArc {placeI = "P3", transitionI = "T2", tokenI = Tokens [Token {numberT = 1, typeT = "y"}]}]
    ]

postsetTests :: TestTree
postsetTests = testGroup "Postset Tests" [
    testCase "Atomic Net Postset" $ postSet testNet0 "T1" @?= [OutputArc {transitionO = "T1", placeO = "P2", tokenO = Tokens [Token 1 "x"]}],
    testCase "Simple Net Postset on T1" $ postSet testNet1 "T1" @?=  [OutputArc {transitionO = "T1", placeO = "P3", tokenO = Tokens [Token {numberT = 3, typeT = "x"}]}],
    testCase "Simple Net Postset on T2" $ postSet testNet1 "T2" @?= [OutputArc {transitionO = "T2", placeO = "P4", tokenO = Tokens [Token {numberT = 1, typeT = "x"}]},OutputArc {transitionO = "T2", placeO = "P5", tokenO = Tokens [Token {numberT = 1, typeT = "y"}]}]
    ]

enabledTransitionTests :: TestTree 
enabledTransitionTests = testGroup "Enabled Transition Tests" [
    testCase "Atomic Net Enabled T1" $ isEnabled testNet0 "T1" @?= Just True ,
    testCase "Simple Net Enabled T1" $ isEnabled testNet1 "T1" @?= Just True
    --T2?? 
    ]

firingTests :: TestTree
firingTests = testGroup "Firing Transitions Tests" [
    testCase "Atomic Net T1 Fired" $ fireTransition testNet0 "T1" @?= Just testNet0fired, 
    testCase "Simple Net T1 Fired" $ fireTransition testNet1 "T1" @?= Just testNet1 { marking = [
        ("P1", Tokens [Token 1 "x"]), 
        ("P2", Tokens [Token 1 "z"]),
        ("P3", Tokens [Token 3 "x"]),
        ("P4", Tokens []),
        ("P5", Tokens [])]}, 
    testCase "Simple Net T2 Fired" $ fireTransition (Data.Maybe.fromMaybe testNet1 (fireTransition testNet1 "T1")) "T2" @?= Just testNet1 { marking = [
        ("P1", Tokens [Token 1 "x"]), 
        ("P2", Tokens [Token 1 "z"]),
        ("P3", Tokens [Token 2 "x"]),
        ("P4", Tokens [Token 1 "x"]),
        ("P5", Tokens [Token 1 "y"])
    ]}
    ]

