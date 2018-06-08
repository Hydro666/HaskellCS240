data NoGenerics = Numbers Int Double
                | Words String [Char] [String]
                | Compund Int String deriving (Show, Read)

data Complex = MineFirst NoGenerics Int
             | BuiltInFirst String Int NoGenerics
             | ComplexIn NoGenerics Complex deriving (Show, Read)

twos :: Double -> NoGenerics
twos = Numbers 2

weirdWords :: NoGenerics
weirdWords = Words "This " "is " ["really ", "weird!"]

getNumber :: NoGenerics -> Double
getNumber (Numbers n d) = d

data Point = Point Double Double deriving (Show, Read)
