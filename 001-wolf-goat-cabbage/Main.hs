module Main where
import Data.List

data Piece = W | G | C deriving (Eq)
data Side = L | R deriving (Show, Eq)
data State = State Side Side Side deriving (Show, Eq)
type Path = [State]

startState = State L L L
finalState = State R R R

cross :: Side -> Side
cross L = R
cross R = L

safe :: Piece -> State -> Bool
safe G _ = True
safe W (State _ g c) = g /= c
safe C (State w g _) = w /= g

move :: Piece -> State -> State
move W (State w g c) = State (cross w) g c
move G (State w g c) = State w (cross g) c
move C (State w g c) = State w g (cross c)

next :: State -> [State]
next s = map (flip move s) . filter (flip safe s) $ [W, G, C]

extend :: Path -> [Path]
extend p = map (\s -> p ++ [s]) . filter (`notElem` p) . next . last $ p

run :: [Path] -> Path
run paths = case find ((finalState ==) . last) paths of
  Nothing -> run $ concatMap extend paths
  Just path -> path

main :: IO ()
main = print $ run [[startState]]
