data Move = Rock | Paper | Scissors
    deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

outcome :: Move -> Move -> Outcome

outcome mine opp 
  | mine == opp = Tie
  | (mine == Rock) && (opp == Scissors) = Win
  | (mine == Scissors) && (opp == Rock) = Lose
  | ((fromEnum mine) - (fromEnum opp)) > 0 = Win
  | otherwise = Lose

moves = [Rock ..]

combs :: [(Move, Move)]
combs = [(a, b) | a <- moves, b <- moves]

toShow = zip combs [outcome (fst x) (snd x) | x <- combs]

parseMove :: String -> Maybe Move
parseMove str 
  | [(move, "")] <- reads str = Just move
  | otherwise = Nothing
