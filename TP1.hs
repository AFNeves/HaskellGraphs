import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical Assignment 1

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

---------------------------------------
-- ADJACENCY LIST REPRESENTATION     
---------------------------------------

type AdjList = [(City,[(City,Distance)])]

-- Receives a roadmap as an argument.
-- Converts the roadmap to an adjacency list.
-- Loops through the roadmap and calls the adjacent function to get the adjacent cities and their respective distances.

toAdjList :: RoadMap -> AdjList
toAdjList roadmap = [(city, adjacent roadmap city) | city <- cities roadmap]

---------------------------------------
-- ADJACENCY MATRIX REPRESENTATION     
---------------------------------------

type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)

-- Receives a roadmap as an argument.
-- Converts the roadmap to an adjacency matrix.
-- Creates an empty matrix with the same dimensions as the number of cities in the roadmap and fills it with the connections between the cities.

toAdjMatrix :: RoadMap -> AdjMatrix
toAdjMatrix roadmap = emptyMatrix Data.Array.// connections Data.Array.// reversedConnections
    where
        n = length (cities roadmap)
        emptyMatrix = Data.Array.array ((0,0), (n - 1,n - 1)) [((i, j), Nothing) | i <- [0..n-1], j <- [0..n-1]]
        connections = [((read city1 :: Int, read city2 :: Int), Just d) | (city1, city2, d) <- roadmap]
        reversedConnections = [((read city2 :: Int, read city1 :: Int), Just d) | (city1, city2, d) <- roadmap]

------------------------------------------------------
-- Function 1 | Returns all the cities in the graph.
------------------------------------------------------

-- Extracts all the cities from the roadmap, sorts them, groups them, and then maps the first element of each group to a list of cities.
-- Used the following StackOverflow thread as reference for this function.
-- https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem

cities :: RoadMap -> [City]
cities roadmap = map (\(x:_) -> x) . Data.List.group . Data.List.sort $ [cities | (city1, city2, _) <- roadmap, cities <- [city1, city2]]

-------------------------------------------------------------------------------------
-- Function 2 | Returns a boolean indicating whether two cities are directly linked.
-------------------------------------------------------------------------------------

-- Receives a roadmap and two cities as arguments.
-- Loops through the roadmap and checks if the two cities are adjacent returning True if they are, and False if they are not.

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((c1, c2, _):xs) city1 city2
    | (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1) = True
    | otherwise = areAdjacent xs city1 city2

-----------------------------------------------------------------------------
-- Function 3 | Returns the distance between two cities directly connected.
-----------------------------------------------------------------------------

-- Receives a roadmap and two cities as arguments.
-- Loops through the roadmap and checks if the two cities are adjacent returning the distance between them if they are, and Nothing if they are not.

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((c1, c2, d):xs) city1 city2
    | (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1) = Just d
    | otherwise = distance xs city1 city2

--------------------------------------------------------------------------------------------------------
-- Function 4 | Returns the cities adjacent to a particular city and the respective distances to them.
--------------------------------------------------------------------------------------------------------

-- Receives a roadmap and a city as arguments.
-- Loops through the roadmap and returns a list of tuples with the adjacent cities and their respective distances.

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent ((c1, c2, d):xs) city
    | c1 == city = (c2, d) : adjacent xs city
    | c2 == city = (c1, d) : adjacent xs city
    | otherwise = adjacent xs city

----------------------------------------------------------------
-- Function 5 | Returns the cost of a path between two cities.
----------------------------------------------------------------

-- Receives a roadmap and a path as arguments.
-- Loops through the path and returns the sum of the distances between the cities in the path if they are directly connected, and Nothing if they are not.

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (x1:x2:xs) = case distance roadmap x1 x2 of
    Just d -> fmap (d +) (pathDistance roadmap (x2:xs))
    Nothing -> Nothing

------------------------------------------------------------------------------------------------------
-- Function 6 | Returns the names of the cities with the highest number of roads connecting to them.
------------------------------------------------------------------------------------------------------

-- Receives a roadmap as the argument.
-- Creates a list of tuples with the cities and the number of roads connecting to them.

cityConnectionCounter :: RoadMap -> [(City, Int)]
cityConnectionCounter roadmap = map (\city -> (city, length (filter (\(c1,c2,_) -> c1 == city || c2 == city) roadmap))) (cities roadmap)

-- Receives a roadmap as an argument.
-- Calls the cityConnectionCounter function to get the list of tuples with the cities and the number of roads connecting to them.
-- Filters the list of tuples to get the cities with the highest number of roads connecting to them.

rome :: RoadMap -> [City]
rome roadmap = map fst (filter (\(_, counter) -> counter == maxCount) (cityConnCount))
    where
        cityConnCount = cityConnectionCounter roadmap
        maxCount = maximum (map snd cityConnCount)

---------------------------------------------------------------------------------------
-- Function 7 | Returns a boolean indicating whether the graph is strongly connected.
---------------------------------------------------------------------------------------

-- Receives a roadmap, a city, and a list of visited cities as arguments.
-- Performs a depth-first search on the graph starting from the city passed as an argument and returns a list of visited cities.

dfs :: RoadMap -> City -> [City] -> [City]
dfs roadmap city visited
    | city `elem` visited = visited
    | otherwise = foldl (\acc c -> dfs roadmap c acc) (city:visited) [c | (c, _) <- adjacent roadmap city]

-- Receives a roadmap as an argument.
-- Calls the dfs function to perform a depth-first search starting from the first city in the roadmap.
-- If the length of the list of visited cities is equal to the length of the list of cities in the roadmap, the graph is strongly connected.

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = length (cities roadmap) == length (dfs roadmap "0" [])

----------------------------------------------------------------------------
-- Function 8 | Returns the shortest path between two cities in the graph.
----------------------------------------------------------------------------

----------------------------------------------------------------
-- EXCEPTION HANDLERS | Handle search in maps preventing errors.
----------------------------------------------------------------

-- Receives a city and a prevMap as arguments.
-- Searches the prevMap for the city and returns the list of previous cities if the city is found, and an empty list if it is not.

searchPrevMap :: City -> [(City, [City])] -> [City]
searchPrevMap city prevMap = case lookup city prevMap of
    Just prevCities -> prevCities
    Nothing -> []

-- Receives a city and a distMap as arguments.
-- Searches the distMap for the city and returns the distance if the city is found, and infinity if it is not.

searchDistMap :: City -> [(City, Distance)] -> Distance
searchDistMap city distMap = case lookup city distMap of
    Just currentDist -> currentDist
    Nothing -> 1000000000000000

--------------------------------------------------------------------
-- DIJKSTRA'S ALGORITHM | Find the shortest path between two cities.
--------------------------------------------------------------------

-- Receives the start city, the end city, and a previous city table as arguments.
-- Recursively builds the paths between the start and end cities using the previous city table.

buildPaths :: City -> City -> [(City, [City])] -> [[City]]
buildPaths start city prevMap
    | city == start = [[start]]
    | otherwise = [city:path | prev <- searchPrevMap city prevMap, path <- buildPaths start prev prevMap]

-- Receives a graph represented as an adjacency list, a priority queue of cities to visit, a distance table, and a previous city table as arguments.
-- Implements Dijkstra's algorithm to find the shortest path between two cities in the graph.
-- Returns the previous city table with the shortest paths between the start and end cities.

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

-- Receives a roadmap, a start city, and an end city as arguments.
-- Calls the dijkstra function to find the shortest path between the start and end cities. Finally, calls the buildPaths function to rebuild the paths found.

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end = map reverse $ buildPaths start end foundPaths
    where
        initialQueue = [(0, start)]
        initialDistMap = [(start, 0)]
        initialPrevMap = [(start, [])]
        foundPaths = dijkstra (toAdjList roadmap) initialQueue initialDistMap initialPrevMap

-----------------------------------------------------------------------------------------------------
-- Função 9 | Returns a solution to the Traveling Salesman Problem (TSP) using Dynamic Programming.
-----------------------------------------------------------------------------------------------------

-- Used the following YouTube video as reference.
-- https://www.youtube.com/watch?v=cY4HiiFHO1o&t=705s

------------------------
-- AUXILIARY DATA TYPE
------------------------

-- Memoization Table Auxiliary Data Structure

type MemoTable = Data.Array.Array (Int,Int) (Maybe Distance)

-------------------------
-- AUXILIARY FUNCTIONS
-------------------------

-- Receives a integer I and a integer bit subset as arguments.
-- Checks if the bit at position I is not set in the binary representation of the subset.
-- Returns True if the bit is not set, and False if the bit is set.

notIn :: Int -> Int -> Bool
notIn i subset = (1 `Data.Bits.shiftL` i) Data.Bits..&. subset == 0

-- Receives a integer I and a integer N as arguments.
-- Generates all the possible combinations of binary numbers with N bits, with I bits set to 1.
-- Recursively generates the combinations by shifting the bits to the left and performing a bitwise OR operation.

combinations :: Int -> Int -> [Int]
combinations 0 _ = [0]
combinations i n
    | i > n = []
    | otherwise = combinations i (n - 1) ++ [x Data.Bits..|. (1 `Data.Bits.shiftL` (n - 1)) | x <- combinations (i - 1) (n - 1)]

-- Receives a matrix, an integer I, and an integer J as arguments.
-- Looks up the distance at position I, J in the matrix.
-- Safely handles the conversion of the Maybe Distance to a Distance.

customMatrixLookup :: Data.Array.Array (Int, Int) (Maybe Distance) -> Int -> Int -> Distance
customMatrixLookup matrix i j = case matrix Data.Array.! (i, j) of
    Just d -> d
    Nothing -> 1000000000000000

-------------------------------------
-- MEMOIZATION TABLE INITIALIZATION
-------------------------------------

-- Receives an integer N as an argument.
-- Creates a memoization table with dimensions N x 2^N filled with Nothing values.

createMemoTable :: Int -> MemoTable
createMemoTable n = Data.Array.array ((0,0), (n - 1, 2^n - 1)) [((i, j), Nothing) | i <- [0..n - 1], j <- [0..2^n - 1]]

-- Receives a graph represented as a adjacency matrix, an integer start, and an integer N as arguments.
-- Initializes the memoization table with the distances between the starting city and the other cities in the graph.

initializeMemoTable :: AdjMatrix -> Int -> Int -> MemoTable
initializeMemoTable matrix start n = createMemoTable n Data.Array.//
    [((i, 1 `Data.Bits.shiftL` start Data.Bits..|. 1 `Data.Bits.shiftL` i), matrix Data.Array.! (start, i)) | i <- [0..n - 1], i /= start]

-----------------------
-- RECOVER FOUND PATH
-----------------------

-- Receives an array as an argument.
-- Converts the array to a Path by mapping the elements of the array to a list of strings.

arrayToPath :: Data.Array.Array Int Int -> Path
arrayToPath array = [show i | i <- Data.Array.elems array]

-- Receives a graph represented as an adjacency matrix, a memoization table, the starting city, and the number of cities as arguments.
-- Sets up and calls the recoverPathAux function to recover the path found by the algorithm.
-- Calls the arrayToPath function to convert the found array to a Path.

recoverPath :: AdjMatrix -> MemoTable -> Int -> Int -> Path
recoverPath matrix memo start n = arrayToPath (initialPathArray Data.Array.// recoveredPathIndices)
    where
        initialIndex = start
        initialState = (1 `Data.Bits.shiftL` n) - 1
        initialPathArray = Data.Array.array (0, n) [(i, -1) | i <- [0..n]] Data.Array.// [(0, start), (n, start)]

        recoveredPathIndices = recoverPathAux (n - 1) initialIndex initialState []

        -- Receives an integer I to loop over, the last city visited, the current state, and an accumulator as arguments.
        -- Recursively recovers the path by calling the findNextCity function to find the next city to visit, updating the state, and the accumulator.

        recoverPathAux :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
        recoverPathAux i lastIndex state acc
            | i < 1 = acc
            | otherwise = recoverPathAux (i - 1) newIndex newState ((i, newIndex) : acc)
            where
                newIndex = findNextCity (-1) [0..n - 1] lastIndex state
                newState = state `Data.Bits.xor` (1 `Data.Bits.shiftL` newIndex)

                -- Receives the best city found so far, a list of cities, the last city visited, and the current state as arguments.
                -- Finds the next city to visit by comparing the distances between the cities in the list and the last city visited.

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

--------------------
-- TSP SOLVE LOGIC
--------------------

-- Receives a graph represented as an adjacency matrix, a memoization table, the starting city, the number of cities, and an integer R to loop over as arguments.
-- Loops over the cities in the graph and calls the updateMemo function to update the memoization table providing the subset list.
-- Calls the combinations function to generate all the possible subsets of numbers with N bits with R bits set to 1.

solveTSP :: AdjMatrix -> MemoTable -> Int -> Int -> Int -> MemoTable
solveTSP matrix memo start n r
    | r > n = memo
    | otherwise = solveTSP matrix updatedMemo start n (r + 1)
        where
            updatedMemo = updateMemo memo (combinations r n)

            -- Receives a memoization table and a list of subsets as arguments.
            -- Loops over the subsets and checks if the start city is in the subset.
            -- If the start city is in the subset, calls the updateSubset function.

            updateMemo :: MemoTable -> [Int] -> MemoTable
            updateMemo memo [] = memo
            updateMemo memo (subset:subsets)
                | notIn start subset = updateMemo memo subsets
                | otherwise = updateMemo (updateSubset memo subset [0..n - 1]) subsets

            -- Receives a memoization table, an integer subset, and a list of cities as arguments.
            -- Loops over the subsets and checks if the next city is the start city or if the next city is not in the subset.
            -- If the conditions are not met, calls the updateNext function.

            updateSubset :: MemoTable -> Int -> [Int] -> MemoTable
            updateSubset memo _ [] = memo
            updateSubset memo subset (next:nexts)
                | next == start || notIn next subset = updateSubset memo subset nexts
                | otherwise = updateSubset (updateNext memo subset next) subset nexts

            -- Receives a memoization table, an integer subset, and the next city as arguments.
            -- Calls the calcMinDist function to calculate the minimum distance between the cities.
            -- Updates the memoization table with the minimum distance found.

            updateNext :: MemoTable -> Int -> Int -> MemoTable
            updateNext memo subset next = memo Data.Array.// [((next, subset), Just minDist)]
                where
                    state = subset `Data.Bits.xor` (1 `Data.Bits.shiftL` next)
                    minDist = calcMinDist memo state next [0..n-1] maxBound

            -- Receives a memoization table, a state, the next city, a list of cities, and the minimum distance found so far as arguments.
            -- Loops over the cities and checks if the city is the start city, the next city, or if the city is not in the state.
            -- If the conditions are not met, calculates the minimum distance between the cities and continues the loop.

            calcMinDist :: MemoTable -> Int -> Int -> [Int] -> Distance -> Distance
            calcMinDist _ _ _ [] minDist = minDist
            calcMinDist memo state next (e:es) minDist
                | e == start || e == next || notIn e state = calcMinDist memo state next es minDist
                | otherwise = calcMinDist memo state next es (min minDist sumDist)
                    where
                        sumDist = customMatrixLookup memo e state + customMatrixLookup matrix e next

----------------------
-- TSP MAIN FUNCTION
----------------------

-- Receives a roadmap as an argument.
-- Calls the solveTSP function to solve the Traveling Salesman Problem using Dynamic Programming.
-- Calls the recoverPath function to recover the path found by the algorithm.

travelSales :: RoadMap -> Path
travelSales roadmap
    | not (isStronglyConnected roadmap) = []
    | otherwise = recoverPath matrix memo start n
        where
            start = 0 -- ALTER THIS LINE TO CHANGE THE STARTING CITY
            n = length (cities roadmap)
            matrix = toAdjMatrix roadmap
            memo = solveTSP matrix (initializeMemoTable matrix start n) start n 3

------------------------------------------------------------------------------------------------
-- Function 10 | Returns a solution to the Traveling Salesman Problem (TSP) using Brute Force.
------------------------------------------------------------------------------------------------

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined

---------------
-- TEST CASES
---------------

gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap
gTest3 = [("0","1",4),("2","3",2)]
