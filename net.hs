module Nets where
import Data.Maybe
import Data.ByteString (group)
import Text.Read (Lexeme(String))
type Place = String 
type Transition = String

data Token = Token {numberT :: Int, typeT :: String} 
    deriving (Show)

data InputArc =  InputArc {placeI :: Place, 
                            transitionI :: Transition, 
                            tokenI :: [Token]} 
    deriving (Show)

data OutputArc = OutputArc {transitionO :: Transition, 
                            placeO :: Place,   
                            tokenO :: [Token]}
    deriving (Show)


data PetriNet = PetriNet {
    places :: [Place],
    transitions :: [Transition], 
    inputArcs :: [InputArc], --minusW = preset
    outputArcs :: [OutputArc], --plusW = postset 
    marking :: [(Place, [Token])]--list of tokens as we can have multiple types of tokens, different numbers
} deriving (Show)


testNet :: PetriNet
testNet = PetriNet {
    places = ["P1", "P2", "P3"], 
    transitions = ["T1", "T2"],
    inputArcs = [
        InputArc {placeI = "P1", transitionI = "T1", tokenI = [ Token 2 "x" ]},
        InputArc {placeI = "P2", transitionI = "T1", tokenI = [ Token 1 "x" , Token 2 "y" ]}
    ],
    outputArcs = [
        OutputArc { transitionO = "T1", placeO = "P3", tokenO = [Token 3 "x"]}
    ],
    marking = [
        ("P1", [Token 1 "x"]),
        ("P2", [Token 1 "x", Token 2 "y", Token 1 "z"]),
        ("P3", [])
    ]

}

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
        go (arc : arcs) = tokensSatisfied (tokenI arc) (fromMaybe [] (lookup (placeI arc) marking) ) && go arcs 


-- Returns true like
--    x = [(3, foo), (4, bar)]
--    y = [(16, foo), ( 4, bar), (10, baz)]
--  tokensSatisfied x y == true
tokensSatisfied :: [Token] -> [Token] -> Bool
tokensSatisfied [] _ = True
tokensSatisfied (current : needed) theOtherTokens = 
    numberT current <= fromMaybe 0 (lookup (typeT current) (map (\t -> (typeT t, numberT t)) theOtherTokens))
    && tokensSatisfied needed theOtherTokens

--list of 
enabledTransitions :: PetriNet -> [Transition]
enabledTransitions net@(PetriNet places tranz inputArcs outArcs marking) = 
    [t | t<- tranz, 
        case isEnabled net t of
        Just True -> True
        Nothing -> False]
{-}
fireTransition :: PetriNet -> Transition -> Maybe PetriNet
fireTransition net@(PetriNet places tranz inputArcs outArcs marking) t = 
    case isEnabled net t of
        Just True -> go 
        _ -> Nothing 
    where 
        go t = 


    
-}
--fire transition=> delete 