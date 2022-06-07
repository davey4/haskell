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

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

phoneBook :: [(String,String)]  
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ] 

-- type PhoneBook = [(String, String)]
type PhoneNumber = String 
type Name = String 
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook