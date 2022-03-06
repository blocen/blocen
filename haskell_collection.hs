x :: Int
x = 12

s :: String
s = "hello world"

main :: IO ()
main = do
  putStrLn $ s ++ " no. " ++ show x
  putStrLn $ show $ add 3 4
  print (add 3 4)
  let t = "test world"
  print t
  putStrLn "enter int: "
  str <- getLine
  let i = read str :: Int
  print str
  print i 
  putStrLn $ "str is: " ++ show str
  putStrLn $ "i is: " ++ show (i * 2)
  let g = "2"
  case g of
    "1" -> putStrLn "one"
    "2" -> putStrLn "tow"
    _ -> putStrLn "none"

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
-- add x y = do
--   x + y
add x y = x + y

