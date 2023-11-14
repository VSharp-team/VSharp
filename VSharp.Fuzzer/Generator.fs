namespace VSharp.Fuzzer

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.InteropServices
open VSharp
open VSharp.Core

type internal GenerationData = {
    seed: int
    method: Method
    this: obj
    thisType: Type
    args: obj array
    argsTypes: Type array
    mocks: Dictionary<ITypeMock, Mocking.Type>
    typeStorage: typeStorage
    referencedObjects: HashSet<obj>
    allocatedObjects: HashSet<obj>
    instantiatedMocks: Dictionary<obj, ITypeMock>
}

type internal Generator(options: Startup.FuzzerOptions, typeSolver: TypeSolver) =
    let instancesCache = Dictionary<Type, Type option>()
    let mutable allocatedObjects = HashSet<obj>()
    let mutable instantiatedMocks = Dictionary<obj, ITypeMock>()
    let mutable referencedObjects = HashSet<obj>()

    let traceGeneration (t: Type) msg = Logger.traceGeneration $"[{t.Name}] {msg}"

    let setAllFields (t: Type) (setter: Type -> obj) =
        traceGeneration t "Set all fields"
        let isStatic = t.IsAbstract && t.IsSealed
        assert (not isStatic)
        let fields = Reflection.fieldsOf isStatic t
        let instance = Reflection.createObject t
        for _, fieldInfo in fields do
            fieldInfo.SetValue(instance, setter fieldInfo.FieldType)
        instance

    let generateViaConstructor commonGenerator (t: Type) (rnd: Random) =
        traceGeneration t "Try generate via constructor"
        let constructors = t.GetConstructors()
        if (constructors.Length = 0)
        then
            traceGeneration t "Constructor not found"
            None
        else
            traceGeneration t "Constructor found"
            let constructor = constructors[rnd.Next(0, constructors.Length)]
            let constructorArgsTypes = constructor.GetParameters() |> Array.map (fun p -> p.ParameterType)
            let constructorArgs = constructorArgsTypes |> Array.map (commonGenerator rnd)
            constructor.Invoke(constructorArgs) |> Some

    let getInstance (t: Type) (rnd: Random) _ =
        traceGeneration t "Try get installable type"
        match instancesCache.TryGetValue t with
        | true, instance ->
            traceGeneration t $"Installable type got from cache: {match instance with | Some x -> x.Name | None -> instance.ToString()}"
            instance
        | false, _ ->
            let instances =
                t.Assembly.GetTypes()
                |> Array.filter (fun x ->
                    x.IsClass
                    && not x.ContainsGenericParameters
                    && not x.IsAbstract
                    && x.IsPublic
                    && t.IsAssignableFrom(x)
                )
            if instances.Length = 0 then
                traceGeneration t "Installable type not found"
                instancesCache.Add(t, None)
            else
                let installableType = instances[rnd.Next(0, instances.Length)]
                traceGeneration t $"Installable type found: {installableType}"
                instancesCache.Add(t, Some installableType)
            instancesCache[t]

    let generateUnboxedChar (rnd: Random) =
        // Supports only ASCII for compatibility with test XML serializer
        rnd.Next(33, 126) |> char

    let builtinNumericTypes = [
        typeof<int8>; typeof<int16>; typeof<int32>; typeof<int64>
        typeof<uint8>; typeof<uint16>; typeof<uint32>; typeof<uint64>
        typeof<float>; typeof<double>
        typeof<byte>
    ]

    let generateUnboxedBool (rnd: Random) =
        rnd.Next(0, 2) = 1

    // Generators
    let generateBuiltinNumeric _ (rnd: Random) (t: Type) =
        traceGeneration t "Generate builtin numeric"

        let size = Marshal.SizeOf t
        let (convert: byte array -> obj) = 
            match t with
            | _ when t = typeof<int8> -> (fun x -> sbyte x[0]) >> box
            | _ when t = typeof<int16> -> BitConverter.ToInt16 >> box
            | _ when t = typeof<int32> -> BitConverter.ToInt32 >> box
            | _ when t = typeof<int64> -> BitConverter.ToInt64 >> box
            | _ when t = typeof<uint8> -> (fun x -> x[0]) >> box
            | _ when t = typeof<uint16> -> BitConverter.ToUInt16 >> box
            | _ when t = typeof<uint32> -> BitConverter.ToUInt32 >> box
            | _ when t = typeof<uint64> -> BitConverter.ToUInt64 >> box
            | _ when t = typeof<float32> -> BitConverter.ToSingle >> box
            | _ when t = typeof<double> -> BitConverter.ToDouble >> box
            | _ -> failwith $"Unexpected type in generateBuiltinNumeric {t}"
            
        let buffer = Array.create<byte> size 0uy
        rnd.NextBytes(buffer)
        convert buffer

    let generateBool _ (rnd: Random) t =
        traceGeneration t "Generate bool"
        (generateUnboxedBool rnd) :> obj

    let generateDecimal _ (rnd: Random) t =
        traceGeneration t "Generate decimal"
        let scale = rnd.Next(29) |> byte
        let sign = generateUnboxedBool rnd
        Decimal (rnd.Next(), rnd.Next(), rnd.Next(), sign, scale) :> obj

    let generateChar _ (rnd: Random) t =
        traceGeneration t "Generate char"
        (generateUnboxedChar rnd) :> obj

    let generateString _ (rnd: Random) t =
        traceGeneration t "Generate string"
        let size = rnd.Next (0, options.stringMaxSize + 1)
        String(Array.init size (fun _ -> generateUnboxedChar rnd)) :> obj

    let generateEnum _ (rnd: Random) (t: Type) =
        traceGeneration t "Generate enum"
        let values = Enum.GetValues(t)
        let index = rnd.Next(0, values.Length)
        values.GetValue(index)

    let generateArray commonGenerator (rnd: Random) (t: Type) =
        traceGeneration t "Generate array"
        if t.IsSZArray then
            let arraySize = rnd.Next(0, options.arrayMaxSize + 1) |> int
            let elementType = t.GetElementType()
            let array = Array.CreateInstance(elementType, arraySize)
            for i in 0 .. arraySize - 1 do
                array.SetValue(commonGenerator rnd elementType, i)
            array :> obj
        else
            // TODO: multidimensional arrays
            // TODO: LowerBound
            __notImplemented__ ()

    let generateAbstractClass commonGenerator (rnd: Random) (t: Type) =
        traceGeneration t "Generate abstract class"
        match getInstance t rnd commonGenerator with
        | Some instance -> commonGenerator rnd instance
        | None ->
            let mock, typ = typeSolver.MockType t (commonGenerator rnd)
            let result = commonGenerator rnd typ
            instantiatedMocks.Add(result, mock)
            result

    let generateByRef commonGenerator (rnd: Random) (t: Type) =
        traceGeneration t "Generate ByRef"
        let referencedType = t.GetElementType()
        let object = commonGenerator rnd referencedType
        referencedObjects.Add(object) |> ignore
        object

    let generatePointer (commonGenerator: Random -> Type -> obj) (rnd: Random) (t: Type) =
        traceGeneration t "Generate pointer"
        let elementType = t.GetElementType()
        let object = commonGenerator rnd elementType
        let reference = ref object
        let pointer = System.Runtime.CompilerServices.Unsafe.AsPointer(reference)
        let result = Pointer.Box(pointer, t)
        allocatedObjects.Add result |> ignore
        result

    let generateDelegate commonGenerator (rnd: Random) (t: Type) =
        Prelude.__notImplemented__ ()

    let generateClass commonGenerator (rnd: Random) (t: Type) =
        traceGeneration t "Generate class"
        match generateViaConstructor commonGenerator t rnd with
        | Some obj -> obj
        | None -> setAllFields t (commonGenerator rnd)

    // Classification
    let (|ValueType|PointerType|ReferenceType|ByRefType|) (t: Type) =
        if t.IsValueType then ValueType
        elif t.IsPointer then PointerType
        elif t.IsByRef then ByRefType
        else ReferenceType

    // Value types
    let (|Enum|BuiltinNumeric|Decimal|Boolean|Char|Void|OtherStruct|) (t: Type) =
        if t.IsEnum then Enum
        elif List.contains t builtinNumericTypes then BuiltinNumeric
        elif t = typeof<Decimal> then Decimal
        elif t = typeof<bool> then Boolean
        elif t = typeof<char> then Char
        elif t = typeof<System.Void> then Void 
        else OtherStruct

    // Reference types
    let (|Array|Delegate|String|AbstractClass|OtherClass|) (t: Type) =
        if t.IsArray then Array
        elif t = typeof<string> then String
        elif t.IsSubclassOf typeof<System.Delegate> then Delegate
        elif t.IsAbstract || t.IsInterface then AbstractClass
        else OtherClass

    let rec commonGenerate (rnd: Random) (t: Type) =
        Logger.traceGeneration $"Generate: {t.Name}"
        let concreteGenerate =
            match t with
            | ValueType ->
                match t with
                | Enum -> generateEnum
                | BuiltinNumeric -> generateBuiltinNumeric
                | Decimal -> generateDecimal
                | Boolean -> generateBool
                | Char -> generateChar
                | Void -> fun _ _ _ -> ()
                // A structure does not differ from a class in terms of generation
                | OtherStruct -> generateClass
            | ReferenceType ->
                match t with
                | Array -> generateArray
                | Delegate -> generateDelegate
                | String -> generateString
                | AbstractClass -> generateAbstractClass
                | OtherClass -> generateClass
            | ByRefType -> __notImplemented__ ()
            | PointerType -> __notImplemented__ ()

        concreteGenerate commonGenerate rnd t

    member this.GenerateObject rnd (t: Type) =
        Logger.traceGeneration $"Target type: {t.Name}"
        match t with
        | ByRefType -> generateByRef commonGenerate rnd t
        | PointerType -> generatePointer commonGenerate rnd t
        | _ -> commonGenerate rnd t

    member private this.RefreshInstantiatedMocks () =
        let result = instantiatedMocks
        instantiatedMocks <- Dictionary<obj, ITypeMock>()
        result

    member private this.RefreshReferencedObjects () =
        let result = referencedObjects
        referencedObjects <- HashSet<obj>()
        result

    member private this.RefreshAllocatedObjects () =
        let result = allocatedObjects
        allocatedObjects <- HashSet<obj>()
        result

    member private this.GenerateCase (method: MethodBase) rndSeed =
        let rnd = Random(rndSeed)

        let methodThisType = method.DeclaringType

        let methodThis =
            if Reflection.hasThis method
            then this.GenerateObject rnd methodThisType
            else null

        let argsTypes =
            method.GetParameters()
            |> Array.map (fun info -> info.ParameterType)

        let args =
            argsTypes
            |> Array.map (this.GenerateObject rnd)

        {| this = methodThis; thisType = methodThisType; args = args; argsTypes = argsTypes |}

    member this.Generate (method: MethodBase) typeStorage rndSeed =
        let case = this.GenerateCase method rndSeed
        {
            seed = rndSeed
            method = Application.getMethod method
            this = case.this
            thisType = case.thisType
            args = case.args
            argsTypes = case.argsTypes
            typeStorage = typeStorage
            allocatedObjects = this.RefreshAllocatedObjects ()
            referencedObjects = this.RefreshReferencedObjects ()
            instantiatedMocks = this.RefreshInstantiatedMocks ()
            mocks = typeSolver.GetMocks ()
        }
