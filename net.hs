module Nets where
import Data.Maybe
type Place = String 
type Transition = String
type Token = Int

data InputArc =  InputArc {placeI :: Place, transitionI :: Transition, tokenI :: Token} 
    deriving (Show)
data OutputArc = OutputArc {transitionO :: Transition, placeO :: Place, tokenO :: Token}
    deriving (Show)


data PetriNet = PetriNet {
    places :: [Place],
    transitions :: [Transition], 
    inputArcs :: [InputArc], --minusW
    outputArcs :: [OutputArc], --plusW
    marking :: [(Place, Token)]
} deriving (Show)

--we are checking if a specific transtion is enabled (the arcs that are )
isEnabled :: PetriNet -> Transition -> Maybe Bool
--check to see if the transition is part of our net in the if clause because pattern matching it would result in never reaching the 
isEnabled (PetriNet places tranz inputArcs outArcs markings) t = if t `elem` tranz then Just (go xs) else Nothing 
    where 
        --ex valid inputArc: p1, t, 3(=needed ) => we have to check in the marking if p1 has at least 3 tokens
        xs :: [InputArc]
        xs =  [arc | arc <- inputArcs, transitionI arc == t ]

        go :: [InputArc] -> Bool    -- lookup :: Eq a => a -> [(a, b)] -> Maybe b 
        go [] = True                -- from maybe 0 is putting the value 0 in case of returning nothing 
        go (arc : arcs) = tokenI arc <= Data.Maybe.fromMaybe 0 (lookup (placeI arc) markings) && go arcs

    
