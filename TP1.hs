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

-- ------------------------------------------------------------------------------------------------------

-- Função 5 | Returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns a Nothing.

-- O(n * m) | n = length of the path, m = length of the roadmap

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (x1:x2:xs) = case distance roadmap x1 x2 of
    Just d -> fmap (d +) (pathDistance roadmap (x2:xs))
    Nothing -> Nothing

-- ------------------------------------------------------------------------------------------------------

-- Função 6 | Returns the names of the cities with the highest number of roads connecting to them.

-- O(n * m) | n = length of the roadmap, m = length of the cities

cityConnectionCounter :: RoadMap -> [(City, Int)]
cityConnectionCounter roadmap = map (\city -> (city, length (filter (\(c1,c2,_) -> c1 == city || c2 == city) roadmap))) (cities roadmap)

rome :: RoadMap -> [City]
rome roadmap = map fst (filter (\(_, counter) -> counter == maxCount) (cityConnCount))
    where
        cityConnCount = cityConnectionCounter roadmap
        maxCount = maximum (map snd cityConnCount)

-- ------------------------------------------------------------------------------------------------------

-- Função 7 | Returns a boolean indicating whether all the cities in the graph are connected in the roadmap.

dfs :: RoadMap -> City -> [City] -> [City]
dfs roadmap city visited
    | city `elem` visited = visited
    | otherwise = foldl (\acc c -> dfs roadmap c acc) (city:visited) [c | (c, _) <- adjacent roadmap city]

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = length (cities roadmap) == length (dfs roadmap "0" [])

-- ------------------------------------------------------------------------------------------------------

-- Função 8 | Returns the shortest path between two cities in the graph.

searchPrevMap :: City -> [(City, [City])] -> [City]
searchPrevMap city prevMap = case lookup city prevMap of
    Just prevCities -> prevCities
    Nothing -> []

searchDistMap :: City -> [(City, Distance)] -> Distance
searchDistMap city distMap = case lookup city distMap of
    Just currentDist -> currentDist
    Nothing -> maxBound

dijkstra :: RoadMap -> [(Distance, City)] -> [(City, Distance)] -> [(City, [City])] -> ([(City, Distance)], [(City, [City])])
dijkstra _ [] distMap prevMap = (distMap, prevMap)
dijkstra roadmap ((dist, city):queue) distMap prevMap = dijkstra roadmap newQueue newDistMap newPrevMap
    where
        currentDist = searchDistMap city distMap
        neighbors = adjacent roadmap city
        (newDistMap, newPrevMap, newQueue) = Data.List.foldl' update (distMap, prevMap, queue) neighbors

        update (distMap, prevMap, queue) (neighbor, weight)
            | newDist < prevNeighborDist = (newDistMap, newPrevMap, newQueue)
            | newDist == prevNeighborDist = (distMap, newPrevMapEqual, queue)
            | otherwise = (distMap, prevMap, queue)
                where
                    prevNeighborDist = searchDistMap neighbor distMap
                    newDist = currentDist + weight
                    newDistMap = (neighbor, newDist) : filter ((/= neighbor) . fst) distMap
                    newPrevMap = (neighbor, [city]) : filter ((/= neighbor) . fst) prevMap
                    newPrevMapEqual = (neighbor, city : searchPrevMap neighbor prevMap) : filter ((/= neighbor) . fst) prevMap
                    newQueue = Data.List.insertBy (\(d1, _) (d2, _) -> compare d1 d2) (newDist, neighbor) queue

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end = map reverse $ buildPaths end prevMap
    where
        initialQueue = [(0, start)]
        initialDistMap = [(start, 0)]
        initialPrevMap = [(start, [])]

        (distMap, prevMap) = dijkstra roadmap initialQueue initialDistMap initialPrevMap

        buildPaths city prevMap
            | city == start = [[start]]
            | otherwise = [city:path | prev <- searchPrevMap city prevMap, path <- buildPaths prev prevMap]

-- ------------------------------------------------------------------------------------------------------

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

gTest4 :: RoadMap
gTest4 = [("0","1",4),("1","2",2),("2","3",1),("1","3",3),("0","2",6)]