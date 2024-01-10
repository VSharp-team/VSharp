module VSharp.ML.GameServer.Maps

open System.Collections.Generic
open VSharp.ML.GameServer.Messages

let trainMaps, validationMaps =
    let trainMaps = Dictionary<_,_>()
    let validationMaps = Dictionary<_,_>()
    
    let add' (maps:Dictionary<_,_>) =
        let mutable firstFreeMapId = 0u
        fun maxSteps coverageToStart pathToDll objectToCover ->
            maps.Add(firstFreeMapId, GameMap(firstFreeMapId, maxSteps, coverageToStart, pathToDll, objectToCover))
            firstFreeMapId <- firstFreeMapId + 1u
    
    let add = add' trainMaps 10000000u<step>
   
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "BinarySearch"    
    add 50u<percent> "VSharp.ML.GameMaps.dll" "BinarySearch"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "Switches1"    
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "Switches2"
    //debug me//add 15u<percent> "VSharp.ML.GameMaps.dll" "Switches2"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "Switches3"
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "Switches4"
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "Switches5"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "NestedFors"    
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "SearchKMP"
    add 20u<percent> "VSharp.ML.GameMaps.dll" "SearchKMP"
   
    //add 0u<percent>  "VSharp.ML.GameMaps.dll" "SearchWords"
    //add 10u<percent> "VSharp.ML.GameMaps.dll" "SearchWords"
      
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "BellmanFord"    
    add 60u<percent> "VSharp.ML.GameMaps.dll" "BellmanFord"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "bsPartition"
    add 20u<percent> "VSharp.ML.GameMaps.dll" "bsPartition"
    add 70u<percent> "VSharp.ML.GameMaps.dll" "bsPartition"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "determinant"
    add 50u<percent> "VSharp.ML.GameMaps.dll" "determinant"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "getCofactor"
    add 50u<percent> "VSharp.ML.GameMaps.dll" "getCofactor"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "fillRemaining"
    add 20u<percent> "VSharp.ML.GameMaps.dll" "fillRemaining"
    add 40u<percent> "VSharp.ML.GameMaps.dll" "fillRemaining"   
    
    //add 0u<percent>  "VSharp.ML.GameMaps.dll" "solveWordWrap"
    //add 20u<percent> "VSharp.ML.GameMaps.dll" "solveWordWrap"
    //add 80u<percent> "VSharp.ML.GameMaps.dll" "solveWordWrap"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "waysToIncreaseLCSBy1"
    //add 30u<percent> "VSharp.ML.GameMaps.dll" "waysToIncreaseLCSBy1"
    //add 70u<percent> "VSharp.ML.GameMaps.dll" "waysToIncreaseLCSBy1"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "BinaryMaze1BFS"
    add 30u<percent> "VSharp.ML.GameMaps.dll" "BinaryMaze1BFS"
    add 70u<percent> "VSharp.ML.GameMaps.dll" "BinaryMaze1BFS"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "findShortestPathLength"
    add 30u<percent> "VSharp.ML.GameMaps.dll" "findShortestPathLength"
    add 80u<percent> "VSharp.ML.GameMaps.dll" "findShortestPathLength"
   
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "countIslands"
    add 30u<percent> "VSharp.ML.GameMaps.dll" "countIslands"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" "MatrixQueryModifyMatrix"
    add 50u<percent> "VSharp.ML.GameMaps.dll" "MatrixQueryModifyMatrix"
    
    add 0u<percent> "VSharp.ML.GameMaps.dll" "RedBlackTreeInsert"
        
    //add 0u<percent> "VSharp.ML.GameMaps.dll" "AhoCorasickMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" "KMPSearchMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" "BinSearchMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" "MatrixMultiplicationMain"
    //add 0u<percent> "VSharp.ML.GameMaps.dll" "MergeSortMain"    
    add 0u<percent> "VSharp.ML.GameMaps.dll" "SudokuMain"
    //add 0u<percent> "VSharp.ML.GameMaps.dll" "WordWrapMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" "LCSMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" "BinaryMaze1Main"
    add 0u<percent> "VSharp.ML.GameMaps.dll" "BinaryMaze2Main"
    add 0u<percent> "VSharp.ML.GameMaps.dll" "IslandsMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" "MatrixQueryMain"
  
    add 0u<percent> "Advanced.Algorithms.dll" "FindArticulationPoints"
    add 0u<percent> "Advanced.Algorithms.dll" "FindBridges"
    add 0u<percent> "Advanced.Algorithms.dll" "Compress"
        
    add 0u<percent> "Advanced.Algorithms.dll" "FindGCD"
    add 50u<percent> "Advanced.Algorithms.dll" "FindGCD"
    
    add 0u<percent> "Advanced.Algorithms.dll" "FindIntersections"
    
    //!!!DEBUG ME!!!//
    //add 0u<percent> "Advanced.Algorithms.dll" "FindSmallest"
    //!!!DEBUG ME!!!//
    //add 10u<percent> "Advanced.Algorithms.dll" "FindSmallest"    
    
    (*add 0u<percent> "Advanced.Algorithms.dll" "insertionSort"*)
    
    
    add 0u<percent> "Advanced.Algorithms.dll" "BaseConvert"
    
    add 0u<percent> "Advanced.Algorithms.dll" "FindAllPairShortestPathsJohnsons"
            
    //add 0u<percent> "Advanced.Algorithms.dll" "FindMinWeightMain"    
    
    add 0u<percent> "Advanced.Algorithms.dll" "FindCombinationRecurse"
    add 0u<percent> "Advanced.Algorithms.dll" "FindPermutationRecurse"
    add 0u<percent> "Advanced.Algorithms.dll" "FindSubsetRecurse"
    
    add 0u<percent> "Advanced.Algorithms.dll" "CalcBase10LogFloor"
    
    //add 0u<percent> "Advanced.Algorithms.dll" "OpenAddressDictionary.ContainsKey"
    //add 0u<percent> "Advanced.Algorithms.dll" "OpenAddressDictionary.Add"
    //add 0u<percent> "Advanced.Algorithms.dll" "OpenAddressDictionary.Remove"
    
    //add 0u<percent> "Advanced.Algorithms.dll" "SeparateChainingDictionary.ContainsKey"
    //add 0u<percent> "Advanced.Algorithms.dll" "SeparateChainingDictionary.Add"
    //add 0u<percent> "Advanced.Algorithms.dll" "SeparateChainingDictionary.Remove"
    
    //add 0u<percent> "Advanced.Algorithms.dll" "QuadTree.Delete"
    
    
    add 0u<percent> "Advanced.Algorithms.dll" "SuffixTree.Insert"
    add 0u<percent> "Advanced.Algorithms.dll" "SuffixTree.Delete"
    add 0u<percent> "Advanced.Algorithms.dll" "SuffixTree.Contains"
    add 0u<percent> "Advanced.Algorithms.dll" "SuffixTree.StartsWith"
    
    add 0u<percent> "Advanced.Algorithms.dll" "BpTree.HasItem"
    add 0u<percent> "Advanced.Algorithms.dll" "BpTree.Insert"
    add 0u<percent> "Advanced.Algorithms.dll" "BpTree.Delete"
    
    
    (*
    add 0u<percent> "Advanced.Algorithms.dll" "OpenAddressHashSet.Contains"
    add 0u<percent> "Advanced.Algorithms.dll" "OpenAddressHashSet.Add"
    add 0u<percent> "Advanced.Algorithms.dll" "OpenAddressHashSet.Remove"
    
    add 0u<percent> "Advanced.Algorithms.dll" "BloomFilter.AddKey"
    add 0u<percent> "Advanced.Algorithms.dll" "BloomFilter.KeyExists"
    
    add 0u<percent> "Advanced.Algorithms.dll" "DisJointSet.Union"
    add 0u<percent> "Advanced.Algorithms.dll" "DisJointSet.FindSet"
    *)
    
    //+add 0u<percent> "Cosmos.Core.dll" "CPU.EstimateCPUSpeedFromName"
    //add 0u<percent> "Cosmos.Core.dll" "CPU.GetMemoryMap"
    //add 0u<percent> "Cosmos.Core.dll" "CPU.GetLargestMemoryBlock"
    //add 0u<percent> "Cosmos.Core.dll" "CPU.GetCPUBrandString"
    
    //add 0u<percent> "Cosmos.Core.dll" "GCImplementation.AllocNewObject"
    //add 0u<percent> "Cosmos.Core.dll" "GCImplementation.Free"
    //+add 0u<percent> "Cosmos.Core.dll" "GCImplementation.GetAvailableRAM"
    //add 0u<percent> "Cosmos.Core.dll" "GCImplementation.Init"
    //add 0u<percent> "Cosmos.Core.dll" "GCImplementation.IncRootCountsInStruct"
    
    //add 0u<percent> "Cosmos.Core.dll" "Heap.Realloc"
    //add 0u<percent> "Cosmos.Core.dll" "Heap.Alloc"
    //add 0u<percent> "Cosmos.Core.dll" "Heap.Free"
    //add 0u<percent> "Cosmos.Core.dll" "Heap.Collect"
    //add 0u<percent> "Cosmos.Core.dll" "Heap.MarkAndSweepObject"
    //add 0u<percent> "Cosmos.Core.dll" "Heap.SweepTypedObject"
    (*
    add 0u<percent> "Cosmos.Core.dll" "Multiboot2.Init"
    
    add 0u<percent> "Cosmos.Core.dll" "VTablesImpl.IsInstance"
    add 0u<percent> "Cosmos.Core.dll" "VTablesImpl.SetTypeInfo"
    add 0u<percent> "Cosmos.Core.dll" "VTablesImpl.GetMethodAddressForType"
    *)
    add 0u<percent> "JetBrains.Lifetimes.dll" "CompactList.LastIndexOf"
    add 0u<percent> "JetBrains.Lifetimes.dll" "CompactList.Add"
    add 0u<percent> "JetBrains.Lifetimes.dll" "CompactList.ToArray"
    add 0u<percent> "JetBrains.Lifetimes.dll" "CompactList.RemoveAt"
        
    add 0u<percent> "JetBrains.Lifetimes.dll" "CopyOnWriteList.Add"
    add 0u<percent> "JetBrains.Lifetimes.dll" "CopyOnWriteList.Clear"
    add 0u<percent> "JetBrains.Lifetimes.dll" "CopyOnWriteList.Remove"
    add 0u<percent> "JetBrains.Lifetimes.dll" "CopyOnWriteList.RemoveAt"
    add 0u<percent> "JetBrains.Lifetimes.dll" "CopyOnWriteList.Insert"
        
    add 0u<percent> "JetBrains.Lifetimes.dll" "JetPriorityQueue.Add"
    add 0u<percent> "JetBrains.Lifetimes.dll" "JetPriorityQueue.Clear"
    add 0u<percent> "JetBrains.Lifetimes.dll" "JetPriorityQueue.TryExtract"
    add 0u<percent> "JetBrains.Lifetimes.dll" "JetPriorityQueue.TryPeek"
              
    //--add 0u<percent> "JetBrains.Lifetimes.dll" "ReactiveEx.FlowInto"
    //--add 0u<percent> "JetBrains.Lifetimes.dll" "ReactiveEx.AdviseAddRemove"
       
    //add 0u<percent> "Algorithms.dll" "BinaryTreeRecursiveWalker.ForEach"
    //add 0u<percent> "Algorithms.dll" "BinaryTreeRecursiveWalker.BinarySearch"
    
    //add 0u<percent> "Algorithms.dll" "CyclesDetector.IsCyclic"
    
    add 0u<percent> "Algorithms.dll" "Permutations.IsAnargram"
    add 0u<percent> "Algorithms.dll" "HeapSorter.HeapSortAscending"
    add 0u<percent> "Algorithms.dll" "OddEvenSorter.OddEvenSortAscending"
    
    //add 0u<percent> "Algorithms.dll" "BinarySearchTreeSorter.UnbalancedBSTSort"
    add 0u<percent> "Algorithms.dll" "BubbleSorter.BubbleSortAscending"
    //add 0u<percent> "Algorithms.dll" "BucketSorter.BucketSortAscending"
    //add 0u<percent> "Algorithms.dll" "CombSorter.CombSortAscending"
    add 0u<percent> "Algorithms.dll" "CountingSorter.CountingSort"
    add 0u<percent> "Algorithms.dll" "CycleSorter.CycleSortAscending"
    add 0u<percent> "Algorithms.dll" "GnomeSorter.GnomeSortAscending"
    add 0u<percent> "Algorithms.dll" "InsertionSorter.InsertionSort"
    //add 0u<percent> "Algorithms.dll" "PigeonHoleSorter.PigeonHoleSortAscending"
    add 0u<percent> "Algorithms.dll" "SelectionSorter.SelectionSortAscending"
    add 0u<percent> "Algorithms.dll" "ShellSorter.ShellSortAscending"
    
    add 0u<percent> "Algorithms.dll" "GreatestCommonDivisor.FindGCDEuclidean"
    add 0u<percent> "Algorithms.dll" "GreatestCommonDivisor.FindGCDStein"
    
    add 0u<percent> "Algorithms.dll" "SieveOfEratosthenes.GeneratePrimesUpTo"
        
    //add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" "DiscHasher.Calculate_PSX_BizIDHash"
    //add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" "DiscHasher.Calculate_PSX_RedumpHash"
    
    add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" "DiscMountJob.Run"
    add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" "DiscHasher.OldHash"

    //add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" "DiscSectorReader.ReadLBA_SubQ"
    //add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" "DiscSectorReader.ReadLBA_2448"
    //add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" "DiscStream.Read"

    add 0u<percent> "BizHawk.Emulation.Cores.dll" "TI83LinkPort.Update"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "AmstradGateArray.ClockCycle"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "AmstradGateArray.OnHSYNC"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "AmstradGateArray.GetVideoBuffer"
    
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "CRCT_6845.ClockCycle"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "CRTC6845.CycleClock"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "CRTC6845.ReadPort"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "CRTC6845.WritePort"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "ScanLine.AddDisplayValue"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "ScanLine.CommitScanline"
    
    add 0u<percent> "Virtu.dll" "DiskIIController.ReadIoRegionC0C0"
    add 0u<percent> "Virtu.dll" "DiskIIController.WriteIoRegionC0C0"

    add 0u<percent> "BizHawk.Emulation.Cores.dll" "MC68000.Disassemble"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "CP1610.Disassemble"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "CP1610.Execute"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "F3850.FetchInstruction"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "HuC6280.DisassembleExt"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "HuC6280.Execute"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "HuC6280.DisassembleCDL"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "I8048.Disassemble"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "I8048.ExecuteOne"
    
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "LR35902.BuildInstructionTable"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "LR35902.Disassemble"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "LR35902.ADDS_Func"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "LR35902.DA_Func"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "MC6800.ExecuteOne"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "MC6800.DA_Func"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "MOS6502X.State"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "x86.Execute"

    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "Emu83.LoadStateBinary"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "NECUPD765.ReadPort"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "NECUPD765.SetUnitSelect"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "AY38912.PortWrite"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "CPCBase.PollInput"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "CPCBase.LoadAllMedia"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "CPCBase.DecodeINPort"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "GateArrayBase.SetupScreenMapping"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "GateArrayBase.ClockCycle"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "CPC6128.ReadBus"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "CPC6128.WriteBus"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "CPC6128.InitROM"
    
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "CPC6128.ReadPort"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "CPC6128.WritePort"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "AmstradCPC.AmstradCPC"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "AmstradCPC.GetFirmware"

    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "AmstradCPC.FrameAdvance"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "CPCMachineMetaData.GetMetaString"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "AmstradCPC.OSD_ShowDiskStatus"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "AmstradCPC.OSD_ShowTapeStatus"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "AmstradCPC.CheckMessageSettings"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "SoundProviderMixer.GetSamplesSync"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "CartridgeDevice.Load"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "Mapper0000.Mapper0000"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "Mapper000F.Mapper000F"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "Mapper0005.Mapper0005"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "Mapper0013.Mapper0013"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "Mapper0020.Mapper0020"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "D64.Read" 
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "DiskBuilder.Build" 
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "G64.Read" 
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "G64.Write"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "Prg.Load"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "Tape.ExecuteCycle"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "Tape.Load"

    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "Chip6510.Read" 
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "Chip6510.Write" 
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "Chip6526.CreateCia1"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "Chip90611401.Write"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "Cia.ExecutePhase"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" "Cia.Write"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "Sid.Flush"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "Sid.filter_operator"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "Envelope.ExecutePhase2"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "Sid.Write"

    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "Sid.GetSamplesSync"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "Via.ExecutePhase"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" "Via.SyncState"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "Via.Write"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "Vic.ExecutePhase1"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" "Vic.Read"
    
    //add 0u<percent> "Virtu.dll" "Keyboard.SetKeys"
    
    //add 0u<percent> "Algorithms.dll" "TopologicalSorter.Sort"
    //add 0u<percent> "Algorithms.dll" "DijkstraShortestPaths.ShortestPathTo"
    //add 0u<percent> "Algorithms.dll" "DijkstraAllPairsShortestPaths.ShortestPath"
    //add 0u<percent> "Algorithms.dll" "DepthFirstSearcher.FindFirstMatch"
    //add 0u<percent> "Algorithms.dll" "ConnectedComponents.Compute"
    //add 0u<percent> "Algorithms.dll" "BreadthFirstShortestPaths.ShortestPathTo"
    //add 0u<percent> "Algorithms.dll" "BreadthFirstSearcher.FindFirstMatch"
    //add 0u<percent> "Algorithms.dll" "BellmanFordShortestPaths.ShortestPathTo"
    
    
    //add 0u  "VSharp.ML.GameMaps.dll" "KruskalMST"
    //add 20u "VSharp.ML.GameMaps.dll" "KruskalMST"
    //add 40u "VSharp.ML.GameMaps.dll" "KruskalMST"
    //add 60u "VSharp.ML.GameMaps.dll" "KruskalMST"

    let add = add' validationMaps
        
    //!!!add 1000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" "mergeSort"
    //add 20000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" "LoanExamBuild"    
    //add 10000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" "multiply_matrix"    
    //add 10000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" "adjoint"
    add 5000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" "LoanExamBuild"    
    add 5000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" "multiply_matrix"    
    add 5000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" "adjoint"
    add 1000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" "bridge"
    add 1000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" "PrimeFactorCount"    
    //add 10000u<step> 0u<percent> "Advanced.Algorithms.dll" "FindLongestPalindrome"
    add 5000u<step>  0u<percent> "Advanced.Algorithms.dll" "bucketSort"
    //add 15000u<step> 0u<percent> "Advanced.Algorithms.dll" "GetMaxBiPartiteMatchingMain"
    add 5000u<step> 0u<percent> "Advanced.Algorithms.dll" "GetMaxBiPartiteMatchingMain"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "ReactiveEx.AdviseUntil"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "ReactiveEx.AdviseOnce"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "Types.ToString"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "CompactList.Clear"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "CompactList.GetEnumerator"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "CopyOnWriteList.Contains"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "CopyOnWriteList.CopyTo"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "CopyOnWriteList.GetEnumerator"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "CopyOnWriteList.IndexOf"
    
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "BitHacks.Log2Floor"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "BitHacks.Log2Ceil"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "BitHacks.NumberOfBitSet"
    
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "StaticsForType.AddFirst"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "StaticsForType.AddLast"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "StaticsForType.ForEachValue"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "StaticsForType.PeekFirst"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "StaticsForType.PeekLast"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "StaticsForType.RemoveLastReferenceEqual"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "StaticsForType.ReplaceFirst"
    
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "Statics.For"
    
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "Memory.Barrier"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "Memory.CopyMemory"
    
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "ReactiveEx.AdviseUntil"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "ReactiveEx.AdviseOnce"
    
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "Types.ToString"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "Types.OptionalTypeInfo"
    
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "CollectionEx.ContentHashCode"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "CollectionEx.TryDequeue"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "CollectionEx.Enqueued"
    
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "SingleThreadScheduler.RunInCurrentStackFrame"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "SingleThreadScheduler.RunOnSeparateThread"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "SingleThreadScheduler.CreateOverExisting"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" "SingleThreadScheduler.Queue"
    
    add 5000u<step> 0u<percent> "BizHawk.Emulation.Cores.dll" "LR35902.ExecuteOne"
    
    add 5000u<step> 0u<percent> "Unity.dll" "ShaderStringBuilder.AppendLines"
    add 5000u<step> 0u<percent> "Unity.dll" "ShaderStringBuilder.Concat"
    add 5000u<step> 0u<percent> "Unity.dll" "ShaderStringBuilder.Dispose"    
    add 5000u<step> 0u<percent> "Unity.dll" "SlotValueHelper.AreCompatible"
    add 5000u<step> 0u<percent> "Unity.dll" "HlslTokenizer.GetOperatorToken"
    add 5000u<step> 0u<percent> "Unity.dll" "HlslTokenizer.ParseNumericLiteral"
    add 5000u<step> 0u<percent> "Unity.dll" "HlslTokenizer.Init"
    add 5000u<step> 0u<percent> "Unity.dll" "PartialDerivUtilWriter.MakeBinaryFunc"
    add 5000u<step> 0u<percent> "Unity.dll" "PartialDerivUtilWriter.MakeSingleFunc"
    add 5000u<step> 0u<percent> "Unity.dll" "PartialDerivUtilWriter.MakeImplicitCast"
    add 5000u<step> 0u<percent> "Unity.dll" "PartialDerivUtilWriter.GenerateDefinitionsAndFuncs"
    add 5000u<step> 0u<percent> "Unity.dll" "HlslParser.ParseStruct"
    add 5000u<step> 0u<percent> "Unity.dll" "HlslParser.ParseStatement"
    add 5000u<step> 0u<percent> "Unity.dll" "HlslGenerator.MarkApdNodesAndVariablesIsLegal"
    add 5000u<step> 0u<percent> "Unity.dll" "HlslGenerator.CalculateApdDependenciesRecurse"
    add 5000u<step> 0u<percent> "Unity.dll" "HlslGenerator.GenerateNodeExpression"
    add 5000u<step> 0u<percent> "Unity.dll" "Collections.Bitwise.FindUlong"
    add 5000u<step> 0u<percent> "Unity.dll" "Collections.Bitwise.FindUpto6bits"
    add 5000u<step> 0u<percent> "Unity.dll" "xxHash3.Hash64Internal"
    add 5000u<step> 0u<percent> "Unity.dll" "ObserverManager.NotifyObservers"
    add 5000u<step> 0u<percent> "Unity.dll" "PropertyBag.Accept"
    add 5000u<step> 0u<percent> "Unity.dll" "PropertyBag.AcceptWithSpecializedVisitor"
    add 5000u<step> 0u<percent> "Unity.dll" "PropertyBag.Register"
    add 5000u<step> 0u<percent> "Unity.dll" "UnsafeList.InsertRangeWithBeginEnd"
    add 5000u<step> 0u<percent> "Unity.dll" "UnsafeHashMap.TryRemove"
    add 5000u<step> 0u<percent> "Unity.dll" "UnsafeHashMap.ResizeExact"
        
    add 5000u<step> 0u<percent> "LiteDB.dll" "LiteEngine.DropCollection"
    add 5000u<step> 0u<percent> "LiteDB.dll" "LiteEngine.RenameCollection"
    add 5000u<step> 0u<percent> "LiteDB.dll" "LiteEngine.Delete"
    add 5000u<step> 0u<percent> "LiteDB.dll" "LiteEngine.EnsureIndex"
    add 5000u<step> 0u<percent> "LiteDB.dll" "LiteEngine.DropIndex"
    add 5000u<step> 0u<percent> "LiteDB.dll" "LiteEngine.Insert"
    add 5000u<step> 0u<percent> "LiteDB.dll" "LiteEngine.Rebuild"
    add 5000u<step> 0u<percent> "LiteDB.dll" "LiteEngine.Update"
    
    trainMaps, validationMaps
