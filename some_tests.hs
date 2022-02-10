x :: Int
x = 12

s :: String
s = "hello world"

main :: IO ()
main = do
  putStrLn $ s ++ " no. " ++ show x
  putStrLn $ show $ add 3 4

data DateInfo = MyDate Int Int Int 
  deriving (Show, Eq)

myDate :: DateInfo
myDate = MyDate 11 22 2020

data Student = Student {
  firstName :: String,
  lastName :: String,
  age :: Int
} deriving (Show)

me :: Student
me = Student "sam" "lom" 46

add :: Int -> Int -> Int
add x y = x + y

