module Data.PetriNet.SampleNets where
import Data.List
import qualified Data.Map as Map
import Data.PetriNet
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|))
import Data.MetadataTracingQueue as MTQ
import Data.MetadataTracingQueue (MTQ)
import Data.Tree
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar
import Data.Time (addUTCTime, nominalDay)


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



seitanFactory :: PetriNet (Transition, UTCTime)
seitanFactory =
    PetriNet
        { places = ["P-GlutenFactory", "P-RawIngredients", "P-FilteringStation", "P-WaterTank", "P-PackageManufacturer", "P-PackageStorage", "P-Staging", "P-Finished", "P-StoreA", "P-StoreB"]
        , transitions = [initalTransition,"T-BuyGluten", "T-FilterWater", "T-BuyPackaging", "T-Mix", "T-Pack", "T-Cook", "T-Distribute"]
        , inputArcs =
            [ InputArc{placeI = "P-GlutenFactory", transitionI = "T-BuyGluten", tokenI = Tokens $ Map.fromList [("Gluten-kg", 100)]}
            , InputArc{placeI = "P-FilteringStation", transitionI = "T-FilterWater", tokenI = Tokens $ Map.fromList [("Water-liters", 100)]}
            , InputArc{placeI = "P-PackageManufacturer", transitionI = "T-BuyPackaging", tokenI = Tokens $ Map.fromList [("Packaging-piece", 2000)]}
            , InputArc{placeI = "P-RawIngredients", transitionI = "T-Mix", tokenI = Tokens $ Map.fromList [("Gluten-kg", 25)]}
            , InputArc{placeI = "P-WaterTank", transitionI = "T-Mix", tokenI = Tokens $ Map.fromList [("Water-liters", 25)]}
            , InputArc{placeI = "P-PackageStorage", transitionI = "T-Pack", tokenI = Tokens $ Map.fromList [("Packaging-piece", 30)]}
            , InputArc{placeI = "P-Staging", transitionI = "T-Pack", tokenI = Tokens $ Map.fromList [("unpackedDough-piece", 25)]}
            , InputArc{placeI = "P-Staging", transitionI = "T-Cook", tokenI = Tokens $ Map.fromList [("packedDough-piece", 50)]}
            , InputArc{placeI = "P-Finished", transitionI = "T-Distribute", tokenI = Tokens $ Map.fromList [("Seitan-piece", 50)]}
            ]
        , outputArcs =
            [ OutputArc{transitionO = "T-BuyGluten", placeO = "P-RawIngredients", tokenO = Tokens $ Map.fromList [("Gluten-kg", 100)]}
            , OutputArc{transitionO = "T-FilterWater", placeO = "P-WaterTank", tokenO = Tokens $ Map.fromList [("Water-liters", 1)]}
            , OutputArc{transitionO = "T-BuyPackaging", placeO = "P-PackageStorage", tokenO = Tokens $ Map.fromList [("Packaging-piece", 2000)]} 
            , OutputArc{transitionO = "T-Mix", placeO = "P-Staging", tokenO = Tokens $ Map.fromList [("unpackedDough-piece", 50)]}
            , OutputArc{transitionO = "T-Pack", placeO = "P-Staging", tokenO = Tokens $ Map.fromList [("packedDough-piece", 25)]}
            , OutputArc{transitionO = "T-Cook", placeO = "P-Finished", tokenO = Tokens $ Map.fromList [("Seitan-piece", 50)]}
            , OutputArc{transitionO = "T-Distribute", placeO = "P-StoreA", tokenO = Tokens $ Map.fromList [("Seitan-piece", 25)]}
            , OutputArc{transitionO = "T-Distribute", placeO = "P-StoreB", tokenO = Tokens $ Map.fromList [("Seitan-piece", 25)]}
            ]
        , marking =
            Marking $
                Map.fromList
                    [ ("P-GlutenFactory", TokensMD $ Map.fromList [("Gluten-kg", MTQ.fromList[(Node (initalTransition, time1) [], 3000)] )])
                    , ("P-RawIngredients", TokensMD $ Map.fromList[])
                    , ("P-FilteringStation", TokensMD $ Map.fromList [("Water-liters", MTQ.fromList[(Node (initalTransition, time2) [], 1000)])])
                    , ("P-WaterTank", TokensMD $ Map.fromList [])
                    , ("P-PackageManufacturer", TokensMD $ Map.fromList [("Packaging-piece", MTQ.fromList[(Node (initalTransition, time3) [], 5000)])])
                    , ("P-PackageStorage", TokensMD $ Map.fromList [])
                    , ("P-Staging", TokensMD $ Map.fromList [])
                    , ("P-Finished", TokensMD $ Map.fromList [])
                    , ("P-StoreA", TokensMD $ Map.fromList [])
                    , ("P-StoreB", TokensMD $ Map.fromList [])
                    ]
        }

seitanOrder :: [(Transition, UTCTime)]
seitanOrder = [(buyGluten, addUTCTime (60 * 60) time1 ), (filterWater, addUTCTime (60 * 60) time2), (buyPackaging, addUTCTime (60 * 60) time3), (mix, addUTCTime (60 * 60 * 2 ) time2), (pack, addUTCTime (60 * 60 * 3 ) time2), (pack, addUTCTime (60 * 60 * 3.5 ) time2), (cook, addUTCTime (60 * 60 * 4 ) time2), (distribute, addUTCTime (60 * 60 * 5 ) time2)]

buyGluten :: Transition
buyGluten = "T-BuyGluten"

filterWater ::Transition
filterWater = "T-FilterWater"

buyPackaging ::Transition
buyPackaging="T-BuyPackaging"

mix::Transition 
mix= "T-Mix"

pack :: Transition
pack= "T-Pack"

cook :: Transition
cook = "T-Cook"

initalTransition :: Transition
initalTransition = "T1"

distribute :: Transition 
distribute = "T-Distribute"


-- 8:00  (UTC), 9 june 2025
time1 :: UTCTime
time1 = UTCTime (fromGregorian 2025 6 9) (secondsToDiffTime (8 * 3600))


-- 9:00  (UTC), 9 june 2025
time2 :: UTCTime
time2 = UTCTime (fromGregorian 2025 6 9) (secondsToDiffTime (9 * 3600))

-- 13:00 (1 PM) UTC, 8 june 2025
time3 :: UTCTime
time3 = UTCTime (fromGregorian 2025 6 8) (secondsToDiffTime (13 * 3600))