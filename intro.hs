doubleMe x = x + x

--doubleUs x y = x*2 + y*2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallerNumber x = if x > 10
                        then x
                        else x*2
                    
lostNumbers = [4,8,15,16,23,42]

listIndex = "Steve Buscemi" !! 6

b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
c = b ++ [[1,1,1,1]]
d = [6,6,6]:b
bIndex = b !! 2

firstElement = head lostNumbers
removeFirst = tail lostNumbers
lastElement = last lostNumbers
removeLast = init lostNumbers
listLenghth = length lostNumbers

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- only works with pair tuples
firstInTuple = fst (8,11)
secondInTuple = snd (8,11)

zippedList = zip[1..] ['A'..'Z']

triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]