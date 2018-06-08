data Point = Point Int Int deriving Show

myPoint :: Point
myPoint = Point 3 4

hypotenuse :: Point -> Int
hypotenuse point = case point of 
                    Point x y -> (x^2 + y^2)

data MyPoly a b = MyLeft a | MyRight b | MyAll a b deriving (Show)

weirdList :: [MyPoly Bool Int]
weirdFirst = MyAll True 5
weirdSecond = MyLeft False
weirdList = [weirdFirst, weirdSecond]

addMaybe :: (Num a) => (Maybe a) -> (Maybe a) -> (Maybe a)
addMaybe (Just x) (Just y) = Just (x + y)
