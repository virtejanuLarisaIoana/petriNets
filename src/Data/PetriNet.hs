module Data.PetriNet where

import Data.Foldable (foldl')
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Maybe
import Data.MetadataTracingQueue (MTQ, pushMTQ, toListMTQ, reRoot, empty, fromList)
import Data.MetadataTracingQueue as MTQ
import Data.List
import Data.Tree
import Data.Time.Clock (UTCTime)

type Place = String
type Transition = String

-- | A mapping from token type to quantity, used to describe 
-- untagged groups of tokens. Used to weights on input/output arcs
newtype Tokens = Tokens {tokenMap :: Map String Integer}
    deriving (Show, Eq)

-- | A mapping from token type to a FIFO "metadata tracking queue"
-- that tracks the transitions tokens have gone through.
newtype TokensMD metadata = TokensMD {tokensMDMap :: Map String (MTQ metadata)}
    deriving (Show, Eq)

stripMetadata :: TokensMD metadata -> Tokens
stripMetadata (TokensMD m ) = Tokens $ Map.map MTQ.lengthQ m

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

-- | A marking of a Petri Net, tracking the metadeta associated with each token
newtype Marking metadata = Marking {getMarking :: Map Place (TokensMD metadata)}
    deriving (Show, Eq)

data PetriNet metadata = PetriNet
    { places :: [Place]
    , transitions :: [Transition]
    , inputArcs :: [InputArc] -- minusW = preset arcs
    , outputArcs :: [OutputArc] -- plusW = postset arcs
    , marking :: Marking metadata -- list of tokens as we can have multiple types of tokens, different numbers
    }
    deriving (Show, Eq)

-- we are checking if a specific transtion is enabled
-- we have an input arc from P1 to T1 that carries x number of tokens of type y. => we check if P1 holds enough token of the needed types in the present marking
isEnabled :: PetriNet metadata -> Transition -> Maybe Bool
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
        go (arc : arcs) = tokensSatisfied (tokenI arc) (fromMaybe (Tokens Map.empty)
            ( stripMetadata <$> getMarking marking !? placeI arc)) && go arcs

-- Returns true like
--    x = [(3, foo), (4, bar)] x = [
--                                   ("foo", [("place1", 2), ("place2", 5)]), 
--                                  ("bar",[("place4", 3), ("place5", 1)])
--                                   ]
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
enabledTransitions :: PetriNet metadata -> [Transition]
enabledTransitions net@(PetriNet places tranz inputArcs outArcs marking) =
    [ t | t <- tranz, case isEnabled net t of
                        Just True -> True
                        Nothing -> False
    ]

-- | The list of arcs that connect the transition to its preset
preSetArcs :: PetriNet metadata -> Transition -> [InputArc]
preSetArcs net@(PetriNet places tranz inputArcs outArcs marking) t = [arc | arc <- inputArcs, transitionI arc == t, t `elem` tranz]

-- | The list of arcs that connect the transition to its postset
postSetArcs :: PetriNet metadata -> Transition -> [OutputArc]
postSetArcs net@(PetriNet places tranz inputArcs outArcs marking) t = [arc | arc <- outArcs, transitionO arc == t, t `elem` tranz]
{-
fireTransition :: PetriNet metadata -> Transition -> Maybe (PetriNet metadata)
fireTransition net@(PetriNet places tranz inputArcs outArcs marking) t =
    case isEnabled net t of
        Just True -> Just (transitionPostSetAddition (transitionPresetSubtract net t) t)
        _ -> Nothing
-}
{- | Shifts the single token type and amount from the right map 
into the lefty map.
This is a fold function for Map.foldrWithKey'

Psuedo Example:
    tokensSplitSingle 
       "foo" 
       4 
       ( {"foo": [(1, m0)]} --already on the added side 
       ,
         { "foo": [(10, m1), (5, m2)]
         , "bar": [(20, m3), (10, m4)]
         }
       )  == 
        ( { "foo": [(4, m1),(1, m0)])}
        , { "foo": [(6, m1) (5, m2)], "bar": (20, m2), (10, m4)}
        )

WARNING:
  - Removes the entry from the token map if the resulting value would be zero.
  - Does not check that the given key is in the map (a no-op if the key is missing)
  - Does not do bounds checking other than 0 (negative numbers are possible)
-}
tokensShiftSingle :: 
    String 
    -> Integer 
    -> (TokensMD metadata, TokensMD metadata) 
    -> (TokensMD metadata, TokensMD metadata)
tokensShiftSingle tokenType tokenDelta (TokensMD tokensToAddTo, TokensMD tokensToRemoveFrom) = 
                ( if Map.null tokensToAddTo
                    then TokensMD $ Map.singleton tokenType tokensToAdd
                    else TokensMD $ Map.insertWith MTQ.pushMTQ tokenType tokensToAdd tokensToAddTo
                , TokensMD $ Map.update removeTokens tokenType tokensToRemoveFrom
                )
            where
                tokensWeCareAbout  = tokensToRemoveFrom Map.! tokenType
                (tokensToAdd, remainingTokens) = MTQ.split tokenDelta tokensWeCareAbout

                addTokens =  MTQ.pushMTQ tokensToAdd  

                removeTokens m = case lengthQ remainingTokens of
                    0 -> Nothing
                    x -> Just remainingTokens



{- | "Shifts" one set of tokens from another, element-wise.

Psuedo Exmaple:
    ( 
    {}
    , {
      foo:("metadataTree1", 1), ("medtadataTree2", 20)
    , bar: ("metadataTree3", 10) 
      } 
    )
    `tokensShift` {("foo" ,3), ("bar", 1)}
    ==
    ( 
      {
       foo: ("metadataTree1", 1), ("medtadataTree2", 2)]
       ,bar: ("metadataTree3", 1)
      } 
      ,{
        foo: ("medtadataTree2", 18)
        , bar: ("metadataTree3", 9)
     )

 For every type of token that is requested by the arc, we shift them type to type at a time 
WARNING: See the warnings from "tokensShiftSingle"
-}
tokensShift :: (TokensMD metadata, TokensMD metadata) -> Tokens -> (TokensMD metadata, TokensMD metadata)
tokensShift tokensMD@(tokensToAddTo, tokensToRemoveFrom) (Tokens deltaTokensMap) =
    (
     TokensMD $ Map.fromList ( sortOn fst (Map.toList f)),
     TokensMD  $ Map.fromList (sortOn fst (Map.toList g))
    )
    where
        (TokensMD f,TokensMD g) = Map.foldrWithKey' tokensShiftSingle tokensMD deltaTokensMap

{- | Shifts a set of tokens out of a given place in a given marking and into the 
tokens accumulator on the first element of the tuple

WARNING:
  - Raises an exception if the place doesn't exisit in the marking
  - See warnings for "tokensSubtract"
-}
markingTokensShift :: (TokensMD metadata, Marking metadata) -> Place -> Tokens -> (TokensMD metadata, Marking metadata)
markingTokensShift (tokensToAddTo, Marking markingMap) place deltaTokens =
    let (shifted, remaining) = tokensShift (tokensToAddTo, markingMap Map.! place) deltaTokens
    in 
        ( shifted
        , Marking $ Map.adjust (const remaining) place markingMap
        )

-- | Shift the appropriate tokens from the appropriate place given a marking and an input arc
markingInputArcShift :: (TokensMD metadata, Marking metadata) -> InputArc -> (TokensMD metadata, Marking metadata)
markingInputArcShift tokensMarkingTuple (InputArc place _trans deltaTokens) =
    markingTokensShift tokensMarkingTuple place deltaTokens

-- | Subtract the appropriate tokens from the appropriate places given a list of input arcs
markingInputArcListShift :: Marking metadata -> [InputArc] -> (TokensMD metadata, Marking metadata)
markingInputArcListShift marking arcs = foldl' markingInputArcShift (TokensMD $ Map.empty, marking) arcs

-- | The tokens have moved "in to" the transition
transitionPresetShift :: PetriNet metadata -> Transition -> (TokensMD metadata, PetriNet metadata)
transitionPresetShift net@(PetriNet{marking = oldMarking}) t =
    let 
        (tokensAcc, newMarking) = markingInputArcListShift oldMarking (preSetArcs net t)
    in     
      (tokensAcc, net { marking = newMarking })

tokenPostShiftSingle ::  metadata -> String -> Integer -> (TokensMD metadata, TokensMD metadata) -> (TokensMD metadata, TokensMD metadata)
tokenPostShiftSingle meta tokenType tokenDelta (oldTokens, newTokens) = 
    let 
        -- oldMTQ :: MTQ metadata 
        -- Takes the metadata of the tokens that need to be deleted so it can reroot it for traceability

        allMTQs = Map.elems (tokensMDMap oldTokens)   -- [MTQ metadata]
        allPairs = concatMap MTQ.toListMTQ allMTQs    
        oldTrees = fmap fst allPairs

        newTokensMap = tokensMDMap newTokens
        newMetadata = MTQ.reRoot meta oldTrees
    in 
        if Map.null newTokensMap
            then  (oldTokens, TokensMD $ Map.singleton tokenType (MTQ.fromList [(newMetadata, tokenDelta)]))
        else 
            let
                newMTQ = MTQ.fromList [(newMetadata, tokenDelta)]
                updatedMap = Map.insertWith (flip MTQ.pushMTQ) tokenType newMTQ newTokensMap
            in
                (oldTokens, TokensMD updatedMap)

-- | Updates the Tokens for each tokenType and number from an arc 
tokensPostShift :: metadata -> (TokensMD metadata, TokensMD metadata) -> Tokens -> (TokensMD metadata, TokensMD metadata)
tokensPostShift meta tokensMD@(tokensToReroot, tokensToAddTo) (Tokens deltaTokensMap) =
    (
     TokensMD $ Map.fromList ( sortOn fst (Map.toList f)),
     TokensMD  $ Map.fromList (sortOn fst (Map.toList g))
    )
    where
        (TokensMD f,TokensMD g) = Map.foldrWithKey'
            (\tokenType tokenDelta acc -> tokenPostShiftSingle meta tokenType tokenDelta acc) tokensMD deltaTokensMap


attachMetadata :: Transition -> UTCTime -> (Transition, UTCTime)
attachMetadata t time = (t, time)

markingTokensPostShift :: metadata -> (TokensMD metadata, Marking metadata) -> Place -> Tokens -> (TokensMD metadata, Marking metadata)
markingTokensPostShift meta (tokensToReroot, Marking markingMap) place deltaTokens =
    let (old, new) = tokensPostShift meta (tokensToReroot, markingMap Map.! place) deltaTokens
    in 
        ( old
        , Marking $ Map.adjust (const new) place markingMap
        )

markingOutputArcShift :: metadata -> (TokensMD metadata, Marking metadata) -> OutputArc -> (TokensMD metadata, Marking metadata)
markingOutputArcShift meta tokensMarkingTuple (OutputArc _trans place deltaTokens) =
    markingTokensPostShift meta tokensMarkingTuple place deltaTokens

markingOutputArcListShift :: metadata -> (TokensMD metadata, Marking metadata) -> [OutputArc] -> (TokensMD metadata, Marking metadata)
markingOutputArcListShift meta (oldTokens, marking) arcs = foldl' (markingOutputArcShift meta) (oldTokens, marking) arcs

-- | The net is fired and gets a new marking
transitionPostSetShift :: metadata -> PetriNet metadata -> Transition -> PetriNet metadata
transitionPostSetShift meta net@(PetriNet{marking = oldMarking}) t =
    let 
        (oldTokens, intermidiaryNet) = transitionPresetShift net t
        intermidiaryMarking = marking intermidiaryNet
        newMarking = snd( markingOutputArcListShift meta (oldTokens, intermidiaryMarking) (postSetArcs net t) )
    in 
        net { marking = newMarking }
{-}
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

-- | Adds one set of tokens to the corresponding spots in another set of tokens
it is going to be f a b => a is added over be
Ex: [("foo", 2), ("bar", 1)] `multipleTokensAddition` [("foo", 1), ("baz", 10)] == [("foo", 3), ("bar", 1), ("baz", 10)]

multipleTokensAddition :: Tokens -> Tokens -> Tokens
multipleTokensAddition deltaTokens (Tokens currentTokens) =
    Map.foldrWithKey' singleTokenAddition deltaTokens currentTokens

-- | Adds a set of tokens in a given place in a given marking
When the key aka the place is not a member of the map, the original map is returned

markingTokensAddition :: Marking metadata -> Place -> Tokens -> Marking metadata
markingTokensAddition (Marking givenMarking) place deltaTokens =
    Marking $
        Map.adjust
            (\currentTokens -> deltaTokens `multipleTokensAddition` currentTokens)
            place
            givenMarking

markingOutputArcAddition :: Marking metadata -> OutputArc -> Marking metadata
markingOutputArcAddition marking (OutputArc _trans place deltaTokens) = markingTokensAddition marking place deltaTokens

-- foldl' :: (b -> a -> b) -> b -> t a -> b
-- a ~ OutputArc
-- t ~ []
-- b ~ Marking
-- foldl' specialized :: (Marking -> OutputArc -> Marking) -> Marking -> [OutputArc] -> Marking
markingOutputArcsListAddition :: Marking metadata -> [OutputArc] -> Marking metadata
markingOutputArcsListAddition givenMarking arcs = foldl' markingOutputArcAddition givenMarking arcs

transitionPostSetAddition :: PetriNet metadata -> Transition -> PetriNet metadata
transitionPostSetAddition net@(PetriNet{marking = oldMarking}) t =
    net
        { marking =
            markingOutputArcsListAddition oldMarking (postSetArcs net t)
        }
-}

