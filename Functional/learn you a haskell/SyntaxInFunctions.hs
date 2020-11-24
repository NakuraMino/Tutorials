lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- always remember to include a catch-all pattern 
-- at the end!
sayMe :: (Integral a) => a -> String 
sayMe 1 = "One!"
sayMe 2 = "Two!" 
sayMe 3 = "Three!"
sayMe x = "Not between 1 and 3"

-- we can also do recursion! a non-recursive approach 
-- is to use product [1..n]
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- pattern matching can be used on tuples too 
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- own head function
head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- @ can be used to reference the whole list 
firstLetter :: String -> String 
firstLetter "" = "Empty String"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] ++ " remainder is " ++ xs

-- first use of guards! similar to if statements 
bmiTell :: (RealFloat a) => a -> String 
bmiTell bmi 
    | bmi <= 18.5 = "You're underweight" 
    | bmi <= 25.0 = "you're normal" 
    | bmi <= 30.0 = "You're fat"
    | otherwise = "whale"

-- second BMI func w two params 
bmiWeightHeight :: (RealFloat a) => a -> a -> String 
bmiWeightHeight weight height 
    | bmi <= 18.5 = "underweight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "fat"
    | otherwise = "whale"
    where bmi = weight / height ^ 2

-- using the let function 
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h 
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

-- case expressions 
head'' :: [a] -> a
head'' xs = case xs of [] -> error "no head for empty list"
                       (x:_) -> x
