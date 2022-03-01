
---3 let and where---
rootsLet :: Floating t => t -> t -> t -> (t,t)
rootsLet a b c =
    let 
        d = sqrt (b*b-4*a*c)/(2*a)
    in
    (((-b-d)/(2*a)),((-b+d)/(2*a)))

rootsWhere :: (Floating t, Ord t)=> t -> t -> t -> (t,t)
rootsWhere a b c
    | d >= 0 = (((-b-d)/(2*a)),((-b+d)/(2*a)))
    | otherwise = (-1,-1)
    where
        d = sqrt (b*b-4*a*c)/(2*a)

---4 make the function---
second :: [x] -> x
second x = head (tail x)

secondLast :: [x] -> x
secondLast x = last (init x)

swap :: (x,y) -> (y,x)
swap (x,y) = (y,x)

pair :: x -> y -> (x,y)
pair x y = (x,y)

palindrome :: Eq x => [x] -> Bool
palindrome xs = xs == reverse xs

twice :: (x -> x) -> x -> x
twice f x = f (f x)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xss) = xs ++ flatten xss

alternate :: Num x => [x] -> [x]
alternate (x:xs) = [x] ++ [negate (head xs)] ++ alternate (tail xs)

setIdx :: [x] -> x -> Int -> [x]
setIdx [] e _ = [e]
setIdx (x: xs) e i = if (i == 0) then [e] ++ [x] ++ xs else [x] ++ setIdx xs e (i-1)

modIdx :: [a] -> (a -> a) -> Int -> [a]
modIdx [] _ _ = []
modIdx (x:xs) f i = if (i == 0) then [f x] ++ xs else [x] ++ modIdx xs f (i-1)

unique :: Eq x => [x] -> [x]
unique [] = []
unique (x:xs) = if elem x xs then unique xs else [x] ++ unique xs

---7 FizzBuzz---

