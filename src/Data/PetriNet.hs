module Data.PetriNet where

import Data.Foldable (foldl')
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Maybe
import Data.MetadataTracingQueue (MTQ)
import Data.MetadataTracingQueue as MTQ

type Place = String
type Transition = String

newtype Tokens = Tokens {tokenMap :: Map String Integer}
    deriving (Show, Eq)

-- type Tokens = Map String Int

data InputArc = InputArc
    { placeI :: Place
    , transitionI :: Transition
    , tokenI :: Tokens
    }
    deriving (Show, Eq)

data OutputArc = OutputArc
    { transitionO :: Transition
    , placeO :: Place
    , tokenO :: Tokens
    }
    deriving (Show, Eq)

newtype Marking = Marking {getMarking :: Map Place Tokens}
    deriving (Show, Eq)

data PetriNet = PetriNet
    { places :: [Place]
    , transitions :: [Transition]
    , inputArcs :: [InputArc] -- minusW = preset arcs
    , outputArcs :: [OutputArc] -- plusW = postset arcs
    , marking :: Marking -- list of tokens as we can have multiple types of tokens, different numbers
    }
    deriving (Show, Eq)

-- we are checking if a specific transtion is enabled
-- we have an input arc from P1 to T1 that carries x number of tokens of type y. => we check if P1 holds enough token of the needed types in the present marking
isEnabled :: PetriNet -> Transition -> Maybe Bool
-- check to see if the transition is part of our net in the if clause because pattern matching it would result in never reaching the
isEnabled (PetriNet places tranz inputArcs outArcs marking) t = if t `elem` tranz then Just (go xs) else Nothing
  where
    -- ex valid inputArc: p1, t, [3(=needed ) whatever, 2 triangle, 1 circle] => we have to check in the marking if p1 has at least 3 tokens of type watever, 1 token of type circle...
    -- xs is the list of inputArcs corresponding to the transition
    xs :: [InputArc]
    xs = [arc | arc <- inputArcs, transitionI arc == t]

    -- InputArcs=[(p1, t1, [2x, 1y]), (p2, t1, [1x])]// marking = [(p1, [2x, 1z]), (p3, [3y, 1z])]
    go :: [InputArc] -> Bool
    go [] = True
    go (arc : arcs) = tokensSatisfied (tokenI arc) (fromMaybe (Tokens Map.empty) (getMarking marking !? placeI arc)) && go arcs

-- Returns true like
--    x = [(3, foo), (4, bar)]
--    y = [(16, foo), ( 4, bar), (10, baz)]
--  tokensSatisfied x y == true
--
-- Will error if `y` is missing some token that exists in `x`
tokensSatisfied :: Tokens -> Tokens -> Bool
tokensSatisfied (Tokens m1) (Tokens m2) = Map.foldlWithKey f True m1
  where
    f :: Bool -> String -> Integer -> Bool
    f b tokenType tokenAmount = case m2 !? tokenType of
        Nothing -> False -- left map contains key that doesn't exist in right map
        Just i -> (tokenAmount <= i) && b

{-
    (go (Map.toList m1) (Map.toList m2))
    where
        go :: [(String, Integer)] -> [(String, Integer)] -> Bool
        go ([]) _ = True
        go ((current : needed)) (theOtherTokens) =
            numberT current <= (lookup (typeT current) (map (\t -> ( numberT t, typeT t)) theOtherTokens))
            && tokensSatisfied (Tokens needed) (Tokens theOtherTokens)
-}

-- list of enabled transitions
enabledTransitions :: PetriNet -> [Transition]
enabledTransitions net@(PetriNet places tranz inputArcs outArcs marking) =
    [ t | t <- tranz, case isEnabled net t of
                        Just True -> True
                        Nothing -> False
    ]

-- | The list of arcs that connect the transition to its preset
preSetArcs :: PetriNet -> Transition -> [InputArc]
preSetArcs net@(PetriNet places tranz inputArcs outArcs marking) t = [arc | arc <- inputArcs, transitionI arc == t, t `elem` tranz]

-- | The list of arcs that connect the transition to its postset
postSetArcs :: PetriNet -> Transition -> [OutputArc]
postSetArcs net@(PetriNet places tranz inputArcs outArcs marking) t = [arc | arc <- outArcs, transitionO arc == t, t `elem` tranz]

fireTransition :: PetriNet -> Transition -> Maybe PetriNet
fireTransition net@(PetriNet places tranz inputArcs outArcs marking) t =
    case isEnabled net t of
        Just True -> Just (transitionPostSetAddition (transitionPresetSubtract net t) t)
        _ -> Nothing

{- | Deletes the single token type and amount from the map
This is a fold function for Map.foldrWithKey'

Psuedo Example:
    tokensSubtractSingle "foo" 4 [("foo", 10), ("bar", 20)] == [("foo", 6), ("bar", 20)]

WARNING:
  - Removes the entry from the token map if the resulting value would be zero.
  - Does not check that the given key is in the map (a no-op if the key is missing)
  - Does not do bounds checking other than 0 (negative numbers are possible)
-}
tokensSubtractSingle :: String -> Integer -> Tokens -> Tokens
tokensSubtractSingle tokenType tokenDelta (Tokens currentTokens) =
    Tokens $
        Map.update
            ( \currentAmount -> case currentAmount - tokenDelta of
                0 -> Nothing
                x -> Just x
            )
            tokenType
            currentTokens

{- | "Subtracts" one set of tokens from another, element-wise.

Psuedo Exmaple:
    [("foo", 10), ("bar", 20)] `tokensSubtract` [("foo", 3), ("bar", 1)]
       == [("foo", 7), ("bar", 19)]

WARNING: See the warnings from "tokensSubtractSingle"
-}
tokensSubtract :: Tokens -> Tokens -> Tokens
tokensSubtract currentTokens (Tokens deltaTokensMap) =
    Map.foldrWithKey' tokensSubtractSingle currentTokens deltaTokensMap

{- | Subtracts a set of tokens from a given place in a given marking

WARNING:
  - Raises an exception if the place doesn't exisit in the marking
  - See warnings for "tokensSubtract"
-}
markingTokensSubtract :: Marking -> Place -> Tokens -> Marking
markingTokensSubtract (Marking markingMap) place deltaTokens =
    Marking $
        Map.adjust
            (\currentTokens -> currentTokens `tokensSubtract` deltaTokens)
            place
            markingMap

-- | Subtract the appropriate tokens from the appropriate place given a marking and an input arc
markingInputArcSubtract :: Marking -> InputArc -> Marking
markingInputArcSubtract marking (InputArc place _trans deltaTokens) =
    markingTokensSubtract marking place deltaTokens

-- | Subtract the appropriate tokens from the appropriate places given a list of input arcs
markingInputArcListSubtract :: Marking -> [InputArc] -> Marking
markingInputArcListSubtract marking arcs = foldl' markingInputArcSubtract marking arcs

transitionPresetSubtract :: PetriNet -> Transition -> PetriNet
transitionPresetSubtract net@(PetriNet{marking = oldMarking}) t =
    net
        { marking =
            markingInputArcListSubtract oldMarking (preSetArcs net t)
        }

singleTokenAddition :: String -> Integer -> Tokens -> Tokens
singleTokenAddition tokenType tokenDelta (Tokens currentToken) =
    Tokens
        ( -- insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
          Map.insertWith
            (+)
            tokenType
            tokenDelta
            currentToken
        )

{- | Adds one set of tokens to the corresponding spots in another set of tokens
it is going to be f a b => a is added over be
Ex: [("foo", 2), ("bar", 1)] `multipleTokensAddition` [("foo", 1), ("baz", 10)] == [("foo", 3), ("bar", 1), ("baz", 10)]
-}
multipleTokensAddition :: Tokens -> Tokens -> Tokens
multipleTokensAddition deltaTokens (Tokens currentTokens) =
    Map.foldrWithKey' singleTokenAddition deltaTokens currentTokens

{- | Adds a set of tokens in a given place in a given marking
When the key aka the place is not a member of the map, the original map is returned
-}
markingTokensAddition :: Marking -> Place -> Tokens -> Marking
markingTokensAddition (Marking givenMarking) place deltaTokens =
    Marking $
        Map.adjust
            (\currentTokens -> deltaTokens `multipleTokensAddition` currentTokens)
            place
            givenMarking

markingOutputArcAddition :: Marking -> OutputArc -> Marking
markingOutputArcAddition marking (OutputArc _trans place deltaTokens) = markingTokensAddition marking place deltaTokens

-- foldl' :: (b -> a -> b) -> b -> t a -> b
-- a ~ OutputArc
-- t ~ []
-- b ~ Marking
-- foldl' specialized :: (Marking -> OutputArc -> Marking) -> Marking -> [OutputArc] -> Marking
markingOutputArcsListAddition :: Marking -> [OutputArc] -> Marking
markingOutputArcsListAddition givenMarking arcs = foldl' markingOutputArcAddition givenMarking arcs

transitionPostSetAddition :: PetriNet -> Transition -> PetriNet
transitionPostSetAddition net@(PetriNet{marking = oldMarking}) t =
    net
        { marking =
            markingOutputArcsListAddition oldMarking (postSetArcs net t)
        }
