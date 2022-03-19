import Data.Char
---MAP---
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = (f x) : (mymap f xs)

mysqrt = mymap sqrt
mylength :: [String] -> [Int]
mylength = mymap length

toupper :: String -> String
toupper = mymap Data.Char.toUpper

listToUpper :: [String] -> [String]
listToUpper = mymap toupper
---Filter---
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs) = if(f x) then (x:(myfilter f xs)) else (myfilter f xs)

underX x = myfilter (x>)


---ZipWith---
myzipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipwith _ _ [] = []
myzipwith _ [] _ = []
myzipwith f (x:xs) (y:ys) = (f x y):(myzipwith f xs ys)

---Fold---
myFoldL _ []
myFoldL f (x:xs) = 