module Data.PetriNet where
import Data.Maybe
import Data.ByteString (group)
import Text.Read (Lexeme(String))

type Place = String 
type Transition = String

data Token = Token {numberT :: Int, typeT :: String} 
    deriving (Show, Eq)

newtype Tokens = Tokens [Token]
    deriving(Show, Eq)
-- type Tokens = Map String Int

data InputArc =  InputArc {placeI :: Place, 
                            transitionI :: Transition, 
                            tokenI :: Tokens} 
    deriving (Show, Eq)

data OutputArc = OutputArc {transitionO :: Transition, 
                            placeO :: Place,   
                            tokenO :: Tokens}
    deriving (Show, Eq)


data PetriNet = PetriNet {
    places :: [Place],
    transitions :: [Transition], 
    inputArcs :: [InputArc], --minusW = preset
    outputArcs :: [OutputArc], --plusW = postset 
    marking :: [(Place, Tokens)]--list of tokens as we can have multiple types of tokens, different numbers
} deriving (Show, Eq)



--we are checking if a specific transtion is enabled 
--we have an input arc from P1 to T1 that carries x number of tokens of type y. => we check if P1 holds enough token of the needed types in the present marking
isEnabled :: PetriNet -> Transition -> Maybe Bool
--check to see if the transition is part of our net in the if clause because pattern matching it would result in never reaching the 
isEnabled (PetriNet places tranz inputArcs outArcs marking) t = if t `elem` tranz then Just (go xs) else Nothing 
    where 
        --ex valid inputArc: p1, t, [3(=needed ) whatever, 2 triangle, 1 circle] => we have to check in the marking if p1 has at least 3 tokens of type watever, 1 token of type circle...
        --xs is the list of inputArcs corresponding to the transition
        xs :: [InputArc]
        xs =  [arc | arc <- inputArcs, transitionI arc == t ]

        -- InputArcs=[(p1, t1, [2x, 1y]), (p2, t1, [1x])]// marking = [(p1, [2x, 1z]), (p3, [3y, 1z])]
        go :: [InputArc] -> Bool                         
        go [] = True                                     
        go (arc : arcs) = tokensSatisfied (tokenI arc) (fromMaybe (Tokens []) (lookup (placeI arc) marking) ) && go arcs 


-- Returns true like
--    x = [(3, foo), (4, bar)]
--    y = [(16, foo), ( 4, bar), (10, baz)]
--  tokensSatisfied x y == true
tokensSatisfied :: Tokens -> Tokens -> Bool
tokensSatisfied (Tokens []) _ = True
tokensSatisfied (Tokens (current : needed)) (Tokens theOtherTokens) = 
    numberT current <= fromMaybe 0 (lookup (typeT current) (map (\t -> (typeT t, numberT t)) theOtherTokens))
    && tokensSatisfied (Tokens needed) (Tokens theOtherTokens)

--list of enabled transitions 
enabledTransitions :: PetriNet -> [Transition]
enabledTransitions net@(PetriNet places tranz inputArcs outArcs marking) = 
    [t | t<- tranz, 
        case isEnabled net t of
        Just True -> True
        Nothing -> False]

-- | The list of arcs that connect the transition to its preset
preSetArcs :: PetriNet -> Transition -> [InputArc] 
preSetArcs net@(PetriNet places tranz inputArcs outArcs marking) t = [arc | arc <- inputArcs, transitionI arc == t, t `elem` tranz ]

-- | The list of arcs that connect the transition to its postset
postSetArcs ::PetriNet -> Transition -> [OutputArc]
postSetArcs net@(PetriNet places tranz inputArcs outArcs marking) t = [arc | arc <- outArcs, transitionO arc == t, t `elem` tranz ]

{-}
fireTransition :: PetriNet -> Transition -> Maybe PetriNet
fireTransition net@(PetriNet places tranz inputArcs outArcs marking) t = 
    case isEnabled net t of
        Just True -> go 
        _ -> Nothing 
    where 
        go t = 

-}

{-
deleteTokensPreset :: PetriNet -> Transition -> PetriNet
deleteTokensPreset net@(PetriNet places tranz inputArcs outputArcs marking) t = 
    foldl' f marking (presetArcs net t)
    where
        f :: [(Place, Tokens)] -> InputArc -> [(Place, Tokens)]
        f marking' (InputArc placeI transitionI tokenI) = 
            foldl' f' [] marking' 
          where 
            f' :: [(Place, Tokens)] -> [(Place, Tokens)] -> [(Place, Tokens)]
            f' m1 m2 = if 

-}



{-



            let
                listOfRequiredTokend = tokenI currentArc
                newMarking :: [(Place, Tokens )]
                newMarking = 
            in
                go =

    
--fire transition=> delete the required tokens from the initial place 
-- => put enough tokens in the places that the output arcs end in
    -}