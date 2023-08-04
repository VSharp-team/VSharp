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
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinarySearch"
        
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches1"    
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    //debug me//add 15u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches2"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches3"
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches4"
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "Switches5"
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "NestedFors"    
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchKMP"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchKMP"
   
    //add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchWords"
    //add 10u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "SearchWords"
      
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"    
    add 60u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BellmanFord"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "bsPartition"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "bsPartition"
    add 70u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "bsPartition"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "matrixInverse"
    add 25u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "matrixInverse"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "determinant"
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "determinant"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "getCofactor"
    add 50u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "getCofactor"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"
    add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"
    add 40u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "fillRemaining"   
    
    //add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "solveWordWrap"
    //add 20u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "solveWordWrap"
    //add 80u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "solveWordWrap"
    
    add 0u<percent>  "VSharp.ML.GameMaps.dll" CoverageZone.Method "waysToIncreaseLCSBy1"
    add 30u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "waysToIncreaseLCSBy1"
    //add 70u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "waysToIncreaseLCSBy1"
    
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
    
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "RedBlackTreeInsert"
        
    //add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "AhoCorasickMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "KMPSearchMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinSearchMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixMultiplicationMain"
    //add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MergeSortMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixInverseMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "SudokuMain"
    //add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "WordWrapMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "LCSMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze1Main"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "BinaryMaze2Main"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "IslandsMain"
    add 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "MatrixQueryMain"
  
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindArticulationPoints"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindBridges"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "Compress"
        
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindGCD"
    add 50u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindGCD"
    
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindIntersections"
    
    //!!!DEBUG ME!!!//
    //add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindSmallest"
    //!!!DEBUG ME!!!//
    //add 10u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindSmallest"    
    
    (*add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "insertionSort"*)
    
    
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "BaseConvert"
    
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindAllPairShortestPathsJohnsons"
            
    //add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindMinWeightMain"    
    
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindCombinationRecurse"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindPermutationRecurse"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindSubsetRecurse"
    
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "CalcBase10LogFloor"
    
    //add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "OpenAddressDictionary.ContainsKey"
    //add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "OpenAddressDictionary.Add"
    //add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "OpenAddressDictionary.Remove"
    
    //add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "SeparateChainingDictionary.ContainsKey"
    //add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "SeparateChainingDictionary.Add"
    //add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "SeparateChainingDictionary.Remove"
    
    //add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "QuadTree.Delete"
    
    
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "SuffixTree.Insert"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "SuffixTree.Delete"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "SuffixTree.Contains"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "SuffixTree.StartsWith"
    
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "BpTree.HasItem"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "BpTree.Insert"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "BpTree.Delete"
    
    
    (*
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "OpenAddressHashSet.Contains"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "OpenAddressHashSet.Add"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "OpenAddressHashSet.Remove"
    
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "BloomFilter.AddKey"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "BloomFilter.KeyExists"
    
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "DisJointSet.Union"
    add 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "DisJointSet.FindSet"
    *)
    
    //+add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "CPU.EstimateCPUSpeedFromName"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "CPU.GetMemoryMap"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "CPU.GetLargestMemoryBlock"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "CPU.GetCPUBrandString"
    
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "GCImplementation.AllocNewObject"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "GCImplementation.Free"
    //+add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "GCImplementation.GetAvailableRAM"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "GCImplementation.Init"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "GCImplementation.IncRootCountsInStruct"
    
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "Heap.Realloc"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "Heap.Alloc"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "Heap.Free"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "Heap.Collect"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "Heap.MarkAndSweepObject"
    //add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "Heap.SweepTypedObject"
    (*
    add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "Multiboot2.Init"
    
    add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "VTablesImpl.IsInstance"
    add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "VTablesImpl.SetTypeInfo"
    add 0u<percent> "Cosmos.Core.dll" CoverageZone.Method "VTablesImpl.GetMethodAddressForType"
    *)
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "CompactList.LastIndexOf"
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "CompactList.Add"
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "CompactList.ToArray"
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "CompactList.RemoveAt"
   
    
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "CopyOnWriteList.Add"
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "CopyOnWriteList.Clear"
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "CopyOnWriteList.Remove"
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "CopyOnWriteList.RemoveAt"
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "CopyOnWriteList.Insert"
    
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "JetPriorityQueue.Add"
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "JetPriorityQueue.Clear"
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "JetPriorityQueue.TryExtract"
    add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "JetPriorityQueue.TryPeek"
        
    //--add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "ReactiveEx.FlowInto"
    //--add 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "ReactiveEx.AdviseAddRemove"
       
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "BinaryTreeRecursiveWalker.ForEach"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "BinaryTreeRecursiveWalker.BinarySearch"
    
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "CyclesDetector.IsCyclic"
    
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "Permutations.IsAnargram"
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "HeapSorter.HeapSortAscending"
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "OddEvenSorter.OddEvenSortAscending"
    
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "BinarySearchTreeSorter.UnbalancedBSTSort"
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "BubbleSorter.BubbleSortAscending"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "BucketSorter.BucketSortAscending"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "CombSorter.CombSortAscending"
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "CountingSorter.CountingSort"
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "CycleSorter.CycleSortAscending"
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "GnomeSorter.GnomeSortAscending"
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "InsertionSorter.InsertionSort"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "PigeonHoleSorter.PigeonHoleSortAscending"
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "SelectionSorter.SelectionSortAscending"
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "ShellSorter.ShellSortAscending"
    
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "GreatestCommonDivisor.FindGCDEuclidean"
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "GreatestCommonDivisor.FindGCDStein"
    
    add 0u<percent> "Algorithms.dll" CoverageZone.Method "SieveOfEratosthenes.GeneratePrimesUpTo"
        
    //add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" CoverageZone.Method "DiscHasher.Calculate_PSX_BizIDHash"
    //add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" CoverageZone.Method "DiscHasher.Calculate_PSX_RedumpHash"
    
    add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" CoverageZone.Method "DiscMountJob.Run"
    add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" CoverageZone.Method "DiscHasher.OldHash"

    //add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" CoverageZone.Method "DiscSectorReader.ReadLBA_SubQ"
    //add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" CoverageZone.Method "DiscSectorReader.ReadLBA_2448"
    //add 0u<percent> "BizHawk.Emulation.DiscSystem.dll" CoverageZone.Method "DiscStream.Read"

    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "TI83LinkPort.Update"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "AmstradGateArray.ClockCycle"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "AmstradGateArray.OnHSYNC"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "AmstradGateArray.GetVideoBuffer"
    
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CRCT_6845.ClockCycle"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CRTC6845.CycleClock"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CRTC6845.ReadPort"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CRTC6845.WritePort"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "ScanLine.AddDisplayValue"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "ScanLine.CommitScanline"
    
    add 0u<percent> "Virtu.dll" CoverageZone.Method "DiskIIController.ReadIoRegionC0C0"
    add 0u<percent> "Virtu.dll" CoverageZone.Method "DiskIIController.WriteIoRegionC0C0"

    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "MC68000.Disassemble"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CP1610.Disassemble"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CP1610.Execute"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "F3850.FetchInstruction"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "HuC6280.DisassembleExt"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "HuC6280.Execute"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "HuC6280.DisassembleCDL"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "I8048.Disassemble"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "I8048.ExecuteOne"
    
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "LR35902.BuildInstructionTable"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "LR35902.ExecuteOne"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "LR35902.Disassemble"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "LR35902.ADDS_Func"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "LR35902.DA_Func"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "MC6800.ExecuteOne"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "MC6800.DA_Func"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "MOS6502X.State"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "x86.Execute"

    //add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Emu83.LoadStateBinary"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "NECUPD765.ReadPort"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "NECUPD765.SetUnitSelect"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "AY38912.PortWrite"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CPCBase.PollInput"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CPCBase.LoadAllMedia"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CPCBase.DecodeINPort"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "GateArrayBase.SetupScreenMapping"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "GateArrayBase.ClockCycle"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CPC6128.ReadBus"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CPC6128.WriteBus"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CPC6128.InitROM"
    
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CPC6128.ReadPort"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CPC6128.WritePort"
    //add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "AmstradCPC.AmstradCPC"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "AmstradCPC.GetFirmware"

    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "AmstradCPC.FrameAdvance"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CPCMachineMetaData.GetMetaString"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "AmstradCPC.OSD_ShowDiskStatus"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "AmstradCPC.OSD_ShowTapeStatus"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "AmstradCPC.CheckMessageSettings"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "SoundProviderMixer.GetSamplesSync"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "CartridgeDevice.Load"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Mapper0000.Mapper0000"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Mapper000F.Mapper000F"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Mapper0005.Mapper0005"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Mapper0013.Mapper0013"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Mapper0020.Mapper0020"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "D64.Read" 
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "DiskBuilder.Build" 
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "G64.Read" 
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "G64.Write"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Prg.Load"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Tape.ExecuteCycle"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Tape.Load"

    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Chip6510.Read" 
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Chip6510.Write" 
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Chip6526.CreateCia1"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Chip90611401.Write"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Cia.ExecutePhase"
    // add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Cia.Write"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Sid.Flush"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Sid.filter_operator"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Envelope.ExecutePhase2"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Sid.Write"

    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Sid.GetSamplesSync"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Via.ExecutePhase"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Via.SyncState"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Via.Write"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Vic.ExecutePhase1"
    add 0u<percent> "BizHawk.Emulation.Cores.dll" CoverageZone.Method "Vic.Read" 

    //add 0u<percent> "Virtu.dll" CoverageZone.Method "Keyboard.SetKeys"
    
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "TopologicalSorter.Sort"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "DijkstraShortestPaths.ShortestPathTo"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "DijkstraAllPairsShortestPaths.ShortestPath"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "DepthFirstSearcher.FindFirstMatch"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "ConnectedComponents.Compute"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "BreadthFirstShortestPaths.ShortestPathTo"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "BreadthFirstSearcher.FindFirstMatch"
    //add 0u<percent> "Algorithms.dll" CoverageZone.Method "BellmanFordShortestPaths.ShortestPathTo"
    
    
    
    //add 0u  "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    //add 20u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    //add 40u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    //add 60u "VSharp.ML.GameMaps.dll" CoverageZone.Method "KruskalMST"
    
    let add = add' validationMaps
        
    //!!!add 1000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "mergeSort"
    //add 20000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "LoanExamBuild"    
    //add 10000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "multiply_matrix"    
    //add 10000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "adjoint"
    add 5000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "LoanExamBuild"    
    add 5000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "multiply_matrix"    
    add 5000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "adjoint"
    add 1000u<step> 0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "bridge"
    add 1000u<step>  0u<percent> "VSharp.ML.GameMaps.dll" CoverageZone.Method "PrimeFactorCount"    
    //add 10000u<step> 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "FindLongestPalindrome"
    add 5000u<step>  0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "bucketSort"
    //add 15000u<step> 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "GetMaxBiPartiteMatchingMain"
    add 5000u<step> 0u<percent> "Advanced.Algorithms.dll" CoverageZone.Method "GetMaxBiPartiteMatchingMain"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "ReactiveEx.AdviseUntil"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "ReactiveEx.AdviseOnce"
    add 5000u<step> 0u<percent> "JetBrains.Lifetimes.dll" CoverageZone.Method "Types.ToString"

    trainMaps, validationMaps
