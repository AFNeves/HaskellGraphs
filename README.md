# HaskellGraphs

This **README** provides a comprehensive guide to the implementation of the two main functions in this project: `shortestPath` and `travelSales`. Each function is discussed in detail, including the algorithms used and auxiliary data structures.

## Shortest Path Function: `shortestPath`

The `shortestPath` function calculates the shortest path between two cities in the roadmap, represented as a `RoadMap`. The implementation uses **Dijkstra's** algorithm to efficiently find the shortest path from the starting city to the destination city.

### Data Structures Used

In the making of this function, several data structures were used to optimize the computation of the shortest path. These structures are essential for storing intermediate results, managing city distances, and efficiently updating paths during the search.

**1. *Adjacency List* (`AdjList`)**

The ***adjacency list*** is used to represent the graph as a collection of cities, each associated with a list of neighboring cities and the distances to them. This structure was chosen for its efficiency in accessing neighbors and their distances, crucial for **Dijkstra's** algorithm.

- **Implementation**: Represented as a list of tuples `(City, [(City, Distance)])`.

- **Functionality**: Provides efficient access to all cities directly reachable from a given city, facilitating traversal and exploration in algorithms such as Dijkstra and TSP.

- **Creation**: The `toAdjList` function constructs the adjacency list from the roadmap, calling the `adjacent` function to retrieve the neighbors of each city.

    ```haskell
    toAdjList :: RoadMap -> AdjList
    toAdjList roadmap = [(city, adjacent roadmap city) | city <- cities roadmap]
    ```

**2. *Priority Queue* (`PQueue`)**

The ***priority queue*** is a central structure in the Dijkstra algorithm, where it maintains the cities ordered by their current shortest known distances. This allows the algorithm to always expand the city with the shortest path next.

- **Implementation**: Represented as a list of tuples `(Distance,City)`.

- **Functionality**: Cities and their distances are inserted in sorted order, ensuring that retrieval of the shortest path is constant.

**3. *Distance Table* (`DistMap`)**

The ***distance table*** keeps track of the minimum known distance from the starting city to each city in the roadmap. This ensures that each city’s shortest path distance is stored and updated only when a shorter path is found.

- **Implementation**: Represented as a list of tuples `(City,Distance)`.

- **Functionality**: Allows for quick lookup of the shortest known distance to a city, enabling efficient updates during the algorithm.

**4. *Previous City Table* (`PrevMap`)**

The ***previous city table*** records the last city in the path before each city, which is essential for reconstructing the shortest path at the end of the algorithm.

- **Implementation**: Represented as a list of tuples `(City,[City])`.

- **Functionality**: Allows the reconstruction of paths by tracing back from the destination to the start.

### Exception Handling Functions

The `searchPrevMap` and `searchDistMap` functions are utility handlers designed to prevent runtime errors when looking up values in the tables, particularly when a city might be missing from the data. These functions ensure the algorithms can handle missing entries gracefully by returning default values.

```haskell
searchPrevMap :: City -> PrevMap -> [City]
searchPrevMap city prevMap = case lookup city prevMap of
    Just prevCities -> prevCities
    Nothing -> []
```

- **Purpose**: Finds the previous cities associated with a given City in prevMap.

- **Default Behavior**: Returns an empty list if city is not found, allowing the algorithm to proceed without errors due to missing data.

```haskell
searchDistMap :: City -> DistMap -> Distance
searchDistMap city distMap = case lookup city distMap of
    Just currentDist -> currentDist
    Nothing -> 1000000000000000
```

- **Purpose**: Retrieves the distance for a given City from distMap.

- **Default Behavior**: Returns a large number (treated as "infinity") if the city is absent, indicating that the city is unreachable in the current path context.

### Rebuiding Paths: `buildPaths`

The `buildPaths` function is responsible for reconstructing the shortest paths from the destination city back to the starting city. It recursively builds the paths by backtracking from the destination to the start, using the previous city table to determine the next city in the path.

```haskell
buildPaths :: City -> City -> PrevMap -> [Path]
buildPaths start city prevMap
    | city == start = [[start]]
    | otherwise = [city:path | prev <- searchPrevMap city prevMap, path <- buildPaths start prev prevMap]
```

- **Purpose**: Recursively builds the shortest paths from the destination city back to the starting city.

- **Base Case**: If the current city is the starting city, the path is complete, and the function returns a list containing the starting city.

- **Recursive Case**: For each previous city, the function appends the current city to the path and recursively builds the path from the start to the previous city.

### Dijkstra's Algorithm: `dijkstra` and `updateNeighbors`

The `dijkstra` function is the main function in the **Dijkstra's** algorithm implementation. It encapsulates the core logic of the algorithm, which is supported by the updateNeighbors function. The `updateNeighbors` function updates the priority queue, distance map, and previous map for the neighbors of the current city.

#### Dijkstra's Algorithm: `dijkstra`

The `dijkstra` function receives the adjacency list, priority queue, distance map, and previous city table as arguments. It iterates through the priority queue, expanding the city with the shortest known distance and calling `updateNeighbors` to update its neighbors. When the priority queue is empty, it means all cities have been visited, and the algorithm terminates, returning the previous city table.

```haskell
dijkstra :: AdjList -> PQueue -> DistMap -> PrevMap -> PrevMap
dijkstra _ [] _ prevMap = prevMap
dijkstra adjList ((dist, city):queue) distMap prevMap = dijkstra adjList newQueue' newDistMap' newPrevMap'
    where
        neighbors = case lookup city adjList of
            Just list -> list
            Nothing -> []
        currentDist = searchDistMap city distMap
        (newQueue', newDistMap', newPrevMap') = updateNeighbors neighbors queue distMap prevMap
```

- **Purpose**: Executes Dijkstra's algorithm to find the shortest paths from the starting city to all other cities in the graph.

- **Base Case**: If the priority queue is empty, the function returns the previous city table.

- **Recursive Case**: The function selects the city with the smallest distance from the priority queue, calls the `updateNeighbors` to update the distances to its neighbors, and recursively processes the remaining cities in the queue.

#### Updating Neighbors: `updateNeighbors`

The `updateNeighbors` function receives the neighbors of the current city, the priority queue, distance map, and previous city table. It iterates through the neighbors list and compares the new distance to the neighbor with the previous known distance. If the new distance is shorter, all three tables are updated with corresponding values. If the new distance is equal to the previous distance, the previous city is appended to the list of previous cities for the neighbor. If the new distance is longer, no updates are made, and the loop continues. When the neighbors list is empty, the function returns the updated priority queue, distance map, and previous city table.

```haskell
updateNeighbors :: [(City, Distance)] -> PQueue -> DistMap -> PrevMap -> (PQueue, DistMap, PrevMap)
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
```

- **Purpose**: Updates the priority queue, distance map, and previous map for the neighbors of the current city.

- **Base Case**: If there are no more neighbors to process, the function returns the updated priority queue, distance map, and previous map.

- **Recursive Case**: The function updates the distances to each neighbor, maintains the shortest known distances, and recursively processes the remaining neighbors:

    - If the new distance is shorter, the priority queue, distance map, and previous map are updated with the new values.

    - If the new distance is equal to the previous distance, the previous city is appended to the list of previous cities for the neighbor.

    - If the new distance is longer, no updates are made.

### Shortest Path Caller: `shortestPath`

The shortestPath function receives a roadmap, a start city, and an end city as arguments. It initializes the necessary data structures and calls the `dijkstra` function to find the shortest paths from the start city to all other cities. Finally, it calls the `buildPaths` function to reconstruct the shortest path from the start city to the end city.

```haskell
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end = map reverse $ buildPaths start end foundPaths
    where
        initialQueue = [(0, start)]
        initialDistMap = [(start, 0)]
        initialPrevMap = [(start, [])]
        foundPaths = dijkstra (toAdjList roadmap) initialQueue initialDistMap initialPrevMap
```

- **Purpose**: Organize and call the necessary functions to find the shortest path between two cities in the roadmap.

- **Functionality**: Initializes the priority queue, distance map, and previous city table, then calls `dijkstra` to find the shortest paths. Finally, it reconstructs the shortest path from the start city to the end city.

## Traveling Salesman Problem (TSP) Function: `travelSales`

The `travelSales` function solves the Traveling Salesman Problem (TSP) for a given roadmap. This function aims to find the minimum-cost tour that visits every city exactly once and returns to the starting city. It leverages dynamic programming with memoization to optimize the TSP solution.

**Implementation Details**

- **Data Structures**:
    
    - **Adjacency Matrix (`AdjMatrix`)**: This matrix, created by `toAdjMatrix`, stores distances between cities, making lookup operations efficient. Each entry `(i, j)` contains the distance from city `i` to city `j`, represented as `Maybe Distance` to allow for non-existent paths.
    
    - **Memo Table**: Used to store intermediate results for each city and visited subset of cities, ensuring subproblems are not recomputed. This table is filled by `solveTSP`, and `recoverPath` retrieves the path by backtracking through the memo table.

- **Algorithm**: The function first checks if the roadmap is strongly connected with `isStronglyConnected`, ensuring a tour is possible. If the roadmap meets this condition, it initializes the memo table and adjacency matrix, then invokes `solveTSP` to compute minimum costs recursively. After computing all costs, `recoverPath` reconstructs the optimal tour by tracing through the memo table.

***

This project was developed by Afonso Neves.