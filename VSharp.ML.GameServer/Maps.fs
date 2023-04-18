module VSharp.ML.GameServer.Maps

open System.Collections.Generic
open VSharp.ML.GameServer.Messages

let trainMaps, validationMaps =
    let trainMaps = Dictionary<_,_>()
    let validationMaps = Dictionary<_,_>()
    
    let add' (maps:Dictionary<_,_>) =
        let mutable firstFreeMapId = 0u
        fun maxSteps coverageToStart pathToDll coverageZone objectToCover ->
            maps.Add(firstFreeMapId, GameMap(firstFreeMapId, maxSteps, coverageToStart, pathToDll, coverageZone, objectToCover))
            firstFreeMapId <- firstFreeMapId + 1u
    
    let add = add' trainMaps 10000000u<step>
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
    add 25u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
        
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches1"    
    // too slow
    //add 25u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches1"
    // too slow
    //add 50u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches1"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    add 15u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    // too slow
    //add 25u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    // too slow
    //add 50u "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "NestedFors"
    add 25u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "NestedFors"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchKMP"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchKMP"
        
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchWords"
    // too slow
    //add 20u "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchWords"
    // too slow
    //add 40u "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchWords"
    // too slow
    //add 60u "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchWords"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    add 60u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "bsPartition"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "bsPartition"
       
    //add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "mergeSort"
    //add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "mergeSort"
    //25
    //add 40u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "mergeSort"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "matrixInverse"
    add 25u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "matrixInverse"
    
    //Moved to validation
    //add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "adjoint"
    //add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "adjoint"
    //add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "adjoint"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "determinant"
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "determinant"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "getCofactor"
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "getCofactor"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"
    add 40u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"
    //too slow
    //add 80u "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "solveWordWrap"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "solveWordWrap"
    add 80u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "solveWordWrap"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "waysToIncreaseLCSBy1"
    add 30u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "waysToIncreaseLCSBy1"
    add 70u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "waysToIncreaseLCSBy1"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze1BFS"
    add 30u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze1BFS"
    add 70u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze1BFS"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "findShortestPathLength"
    add 30u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "findShortestPathLength"
    add 80u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "findShortestPathLength"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "countIslands"
    add 30u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "countIslands"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixQueryModifyMatrix"
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixQueryModifyMatrix"
    
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "AhoCorasickMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "KMPSearchMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinSearchMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixMultiplicationMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MergeSortMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixInverseMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "SudokuMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "WordWrapMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "LCSMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze1Main"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze2Main"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "IslandsMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixQueryMain"
    
    //add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    //add 20u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    //add 40u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    //add 60u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
     
    let add = add' validationMaps
        
    //add 400u<step> 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "mergeSort"
    add 20000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "LoanExamBuild"    
    add 5000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "multiply_matrix"    
    add 5000u<step> 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "adjoint"
    add 5000u<step> 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "bridge"
    add 5000u<step> 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "PrimeFactorCount"

          
    trainMaps, validationMaps
