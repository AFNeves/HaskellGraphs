import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- ------------------------------------------------------------------------------------------------------

-- Função 1 | Returns all the cities in the graph.

-- O(n log n) | https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem

cities :: RoadMap -> [City]
cities roadmap = map (\(x:_) -> x) . Data.List.group . Data.List.sort $ [cities | (city1, city2, _) <- roadmap, cities <- [city1, city2]]

-- ------------------------------------------------------------------------------------------------------

-- Função 2 | Returns a boolean indicating whether two cities are linked directly.

-- O(n)

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((c1, c2, _):xs) city1 city2
    | (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1) = True
    | otherwise = areAdjacent xs city1 city2

-- ------------------------------------------------------------------------------------------------------

-- Função 3 | Returns a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise.

-- O(n)

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((c1, c2, d):xs) city1 city2
    | (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1) = Just d
    | otherwise = distance xs city1 city2
-- ------------------------------------------------------------------------------------------------------

-- Função 4 | Returns the cities adjacent to a particular city and the respective distances to them.

-- O(n)

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent ((c1, c2, d):xs) city
    | c1 == city = (c2, d) : adjacent xs city
    | c2 == city = (c1, d) : adjacent xs city
    | otherwise = adjacent xs city

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
