multThree :: (Num a) => a -> a -> a -> a
multThree x y z = z * y * z

multTwoWithNine = multThree 9

multWithEighteen = multTwoWithNine 2

compareWithHundered :: (Num a, Ord a) => a -> Ordering 
compareWithHundered = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--     where g x y = f y x

flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y

x = 3
list = [1,5,3,1,6] 
mappedList = map (+x) list

filteredList = filter (>x) list

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

sumOfOddSquaresSmallerThanTenThousand = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
    | even x = x:chain (x `div` 2)
    | odd x = x:chain (x*3 + 1)
    | otherwise = undefined 

numLongChains :: Int 
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

listOfFuns = map (*) [0..]

numLongChainsLambda :: Int 
numLongChainsLambda = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' xs = foldl (\acc x -> acc + x) 0 xs