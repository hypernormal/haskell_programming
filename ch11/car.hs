data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | Catapults | TakeYourChance deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 1000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 200)

isCar :: Vehicle -> Bool
isCar vehicle =
  case vehicle of
    Car _ _ -> True
    _ -> False

isPlane :: Vehicle -> Bool
isPlane vehicle =
  case vehicle of
    Plane _ _ -> True
    _ -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu vehicle =
  case vehicle of
    Car manu _ -> manu
