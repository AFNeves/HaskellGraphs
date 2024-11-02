import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- ------------------------------------------------------------------------------------------------------

-- Graph Representation Conversions

type AdjList = [(City,[(City,Distance)])]

type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)

type MemoTable = Data.Array.Array (Int,Int) (Maybe Distance)

toAdjList :: RoadMap -> AdjList
toAdjList roadmap = [(city, adjacent roadmap city) | city <- cities roadmap]

toAdjMatrix :: RoadMap -> AdjMatrix
toAdjMatrix roadmap = emptyMatrix Data.Array.// [((read city1 :: Int, read city2 :: Int), Just d) | (city1, city2, d) <- roadmap]
    where
        n = length (cities roadmap)
        emptyMatrix = Data.Array.array ((0,0), (n - 1,n - 1)) [((i, j), Nothing) | i <- [0..n-1], j <- [0..n-1]]

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

dijkstra :: AdjList -> [(Distance, City)] -> [(City, Distance)] -> [(City, [City])] -> ([(City, [City])])
dijkstra _ [] _ prevMap = prevMap
dijkstra adjList ((dist, city):queue) distMap prevMap = dijkstra adjList newQueue' newDistMap' newPrevMap'
    where
        neighbors = case lookup city adjList of
            Just list -> list
            Nothing -> []
        currentDist = searchDistMap city distMap
        (newQueue', newDistMap', newPrevMap') = updateNeighbors neighbors queue distMap prevMap

        updateNeighbors [] queue distMap prevMap = (queue, distMap, prevMap)
        updateNeighbors ((neighbor, weight):ns) queue distMap prevMap
            | newDist < prevNeighborDist = updateNeighbors ns newQueue newDistMap newPrevMap
            | newDist == prevNeighborDist = updateNeighbors ns queue distMap newPrevMapEqual
            | otherwise = updateNeighbors ns queue distMap prevMap
                where
                    newDist = currentDist + weight
                    prevNeighborDist = searchDistMap neighbor distMap
                    newQueue = Data.List.insertBy (\(d1, _) (d2, _) -> compare d1 d2) (newDist, neighbor) queue
                    newDistMap = (neighbor, newDist) : filter ((/= neighbor) . fst) distMap
                    newPrevMap = (neighbor, [city]) : filter ((/= neighbor) . fst) prevMap
                    newPrevMapEqual = (neighbor, city : searchPrevMap neighbor prevMap) : filter ((/= neighbor) . fst) prevMap

buildPaths :: City -> City -> [(City, [City])] -> [[City]]
buildPaths start city prevMap
    | city == start = [[start]]
    | otherwise = [city:path | prev <- searchPrevMap city prevMap, path <- buildPaths start prev prevMap]

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end = map reverse $ buildPaths start end foundPaths
    where
        initialQueue = [(0, start)]
        initialDistMap = [(start, 0)]
        initialPrevMap = [(start, [])]
        foundPaths = dijkstra (toAdjList roadmap) initialQueue initialDistMap initialPrevMap

-- ------------------------------------------------------------------------------------------------------

-- Função 9 | Returns a solution to the Traveling Salesman Problem (TSP) using Dynamic Programming.

--------------------------------------------------
-- AUXILIARY FUNCTIONS
--------------------------------------------------

notIn :: Int -> Int -> Bool
notIn i subset = (1 `Data.Bits.shiftL` i) Data.Bits..&. subset == 0

combinations :: Int -> Int -> [Int]
combinations 0 _ = [0]
combinations i n
    | i > n = []
    | otherwise = combinations i (n - 1) ++ [x Data.Bits..|. (1 `Data.Bits.shiftL` (n - 1)) | x <- combinations (i - 1) (n - 1)]

customMatrixLookup :: Data.Array.Array (Int, Int) (Maybe Distance) -> Int -> Int -> Distance
customMatrixLookup matrix i j = case matrix Data.Array.! (i, j) of
    Just d -> d
    Nothing -> maxBound

--------------------------------------------------
-- INITIALIZATION
--------------------------------------------------

createMemoTable :: Int -> MemoTable
createMemoTable n = Data.Array.array ((0,0), (n - 1, 2^n - 1)) [((i, j), Nothing) | i <- [0..n - 1], j <- [0..2^n - 1]]

initializeMemoTable :: AdjMatrix -> Int -> Int -> MemoTable
initializeMemoTable matrix start n = createMemoTable n Data.Array.//
    [((i, 1 `Data.Bits.shiftL` start Data.Bits..|. 1 `Data.Bits.shiftL` i), matrix Data.Array.! (start, i)) | i <- [0..n - 1], i /= start]

--------------------------------------------------
-- RECOVER PATH
--------------------------------------------------

arrayToPath :: Data.Array.Array Int Int -> Path
arrayToPath array = [show i | i <- Data.Array.elems array]

recoverPath :: AdjMatrix -> MemoTable -> Int -> Int -> Path
recoverPath matrix memo start n = arrayToPath (initialPathArray Data.Array.// recoveredPathIndices)
    where
        lastIndex = start
        initialState = (1 `Data.Bits.shiftL` n) - 1
        initialPathArray = Data.Array.array (0, n) [(i, -1) | i <- [0..n]] Data.Array.// [(0, start), (n, start)]

        recoveredPathIndices = recoverPathAux (n - 1) lastIndex initialState []

        recoverPathAux :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
        recoverPathAux i lastIndex state acc
            | i < 1 = acc
            | otherwise = recoverPathAux (i - 1) newIndex newState ((i, newIndex) : acc)
            where
                newIndex = findNextCity (-1) [0..n - 1] lastIndex state
                newState = state `Data.Bits.xor` (1 `Data.Bits.shiftL` newIndex)

                findNextCity :: Int -> [Int] -> Int -> Int -> Int
                findNextCity best [] _ _ = best
                findNextCity best (j:js) index state
                    | j == start || notIn j state = findNextCity best js lastIndex state
                    | otherwise =
                        let prevDist = customMatrixLookup memo best state + customMatrixLookup matrix best lastIndex
                            newDist = customMatrixLookup memo j state + customMatrixLookup matrix j lastIndex
                        in if best == -1 || newDist < prevDist
                            then findNextCity j js lastIndex state
                            else findNextCity best js lastIndex state

--------------------------------------------------
-- TSP SOLVE LOOP
--------------------------------------------------

solveTSP :: AdjMatrix -> MemoTable -> Int -> Int -> Int -> MemoTable
solveTSP matrix memo start n r
    | r > n = memo
    | otherwise = solveTSP matrix updatedMemo start n (r + 1)
        where
            updatedMemo = updateMemo memo r n

            updateMemo :: MemoTable -> Int -> Int -> MemoTable
            updateMemo memo r n = updateMemoAux memo (combinations r n)
                where
                    updateMemoAux :: MemoTable -> [Int] -> MemoTable
                    updateMemoAux memo [] = memo
                    updateMemoAux memo (subset:subsets)
                        | notIn start subset = updateMemoAux memo subsets
                        | otherwise = updateMemoAux (updateSubset memo subset) subsets

            updateSubset :: MemoTable -> Int -> MemoTable
            updateSubset memo subset = updateSubsetAux memo subset [0..n - 1]
                where
                    updateSubsetAux :: MemoTable -> Int -> [Int] -> MemoTable
                    updateSubsetAux memo _ [] = memo
                    updateSubsetAux memo subset (next:nexts)
                        | next == start || notIn next subset = updateSubsetAux memo subset nexts
                        | otherwise = updateSubsetAux (updateNext memo subset next) subset nexts

            updateNext :: MemoTable -> Int -> Int -> MemoTable
            updateNext memo subset next = memo Data.Array.// [((next, subset), Just minDist)]
                where
                    state = subset `Data.Bits.xor` (1 `Data.Bits.shiftL` next)
                    minDist = calcMinDist memo state next [0..n-1] maxBound

            calcMinDist :: MemoTable -> Int -> Int -> [Int] -> Distance -> Distance
            calcMinDist _ _ _ [] minDist = minDist
            calcMinDist memo state next (e:es) minDist
                | e == start || e == next || notIn e state = calcMinDist memo state next es minDist
                | otherwise = calcMinDist memo state next es (min minDist sumDist)
                    where
                        sumDist = customMatrixLookup memo e state + customMatrixLookup matrix e next

--------------------------------------------------
-- MAIN FUNCTION
--------------------------------------------------

travelSales :: RoadMap -> Path
travelSales roadmap = recoverPath matrix memo start n
    where
        start = 0 -- ALTER THIS LINE TO CHANGE THE STARTING CITY
        n = length (cities roadmap)
        matrix = toAdjMatrix roadmap
        memo = solveTSP matrix (initializeMemoTable matrix start n) start n 3

-- ------------------------------------------------------------------------------------------------------

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap
gTest4 = [("0","1",4),("1","2",2),("2","3",1),("1","3",3),("0","2",6)]