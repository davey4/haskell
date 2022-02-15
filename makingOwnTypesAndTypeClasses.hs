data Bool = False | True

data Shape = Circle Float Float Float | Rectangle Float Float Float Float 
  deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)

-- data Person = Person String String Int Float String String
--   deriving (Show)
-- firstName (Person firstname _ _ _ _ _) = firstname
-- lastName (Person _ lastname _ _ _ _) = lastname 
-- age (Person _ _ age _ _ _) = age
-- height (Person _ _ _ height _ _) = height
-- phoneNumber (Person _ _ _ _ number _) = number
-- favoriteFlavor (Person _ _ _ _ _ flavor) = flavor

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , favoriteFlavor :: String  
                     } deriving (Show) 

guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

data Maybe  a = Nothing | Just a