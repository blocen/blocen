-- Week 3 Assignment
-- Write the following functions in a file called assignment.hs. All numeric function arguments
-- and results are of type int unless otherwise specified. Write type declarations for all your
-- functions. (20 marks)


-- 1. Write a function called add_and_double which adds two numbers together and then
-- doubles the result.

add_and_double :: Int -> Int -> Int
add_and_double x y = 2 * ( x + y )

-- 2. Write an infix operator called +* which does the same thing as add_and_double. Define
-- it in terms of add_and_double using the backtick (`) notation.

(+*) :: Int -> Int -> Int
-- (+*) = add_and_double
(+*) a b =  a `add_and_double` b

-- 3. Define your own type student that will be described by first name, last name, birth date,
-- specialization, study year, and average grade. Make it a record.

data DateInfo = MyDate Int Int Int 
  deriving (Show, Eq)

data Student = Student {
  firstName       :: String,
  lastName       :: String,
  birthDate      :: DateInfo,
  specialization :: String,
  studyYear      :: Int,
  avgGrade       :: Float
} deriving (Show)

birthDate1 :: DateInfo
birthDate1 = MyDate 11 22 2000

student1 :: Student
student1 = Student "Sam" "Sung" birthDate1 "IT" 2022 3.0

-- 4. Using pattern matching create a function that checks if the number we supplied to it is a
-- seven or not.

isSeven :: Int -> Bool
isSeven 7 = True
isSeven _ = False


