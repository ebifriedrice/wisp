namespace Wisp.Compiler

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Globalization
open System.Numerics
open System.Reflection
open System.Reflection.Emit
open Wisp.Core

exception InteropException of string

module DelegateConversions =
    let tryConvert (value: obj) (targetType: Type) =
        let targetType = Nullable.GetUnderlyingType targetType |> Option.ofObj |> Option.defaultValue targetType
        if isNull value then
            if targetType.IsValueType then None else Some null
        elif targetType = typeof<obj> || targetType.IsInstanceOfType value then
            Some value
        elif targetType.IsEnum then
            try Some(Enum.ToObject(targetType, value)) with _ -> None
        elif targetType.IsValueType && value :? IConvertible then
            try Some(Convert.ChangeType(value, targetType, Globalization.CultureInfo.InvariantCulture)) with _ -> None
        else
            None

    let Convert(value: obj, targetType: Type) : obj =
        match tryConvert value targetType with
        | Some converted -> converted
        | None ->
            raise (
                InvalidCastException(
                    sprintf "Cannot convert delegate result %O to %O." value targetType
                )
            )

module DelegateOps =
    let private assemblyName = AssemblyName("Wisp.Delegates." + Guid.NewGuid().ToString("N"))
    let private assembly = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndCollect)
    let private moduleBuilder = assembly.DefineDynamicModule(assemblyName.Name)
    let private adapters = ConcurrentDictionary<Type, Type>()
    let private ifnInvoke = typeof<IFn>.GetMethod("Invoke")

    let private buildAdapter (delegateType: Type) =
        let invokeMethod = delegateType.GetMethod("Invoke")
        if isNull invokeMethod then
            raise (ArgumentException(sprintf "%O is not a delegate type." delegateType))
        let parameters = invokeMethod.GetParameters()
        if invokeMethod.ReturnType.IsByRef then
            raise (NotSupportedException "Delegates with byref returns are not supported.")
        let adapterBuilder =
            moduleBuilder.DefineType(
                "Wisp.Generated.Delegates.Adapter_" + Guid.NewGuid().ToString("N"),
                TypeAttributes.Public ||| TypeAttributes.Sealed
            )
        let functionField = adapterBuilder.DefineField("_fn", typeof<IFn>, FieldAttributes.Private)
        let constructorBuilder =
            adapterBuilder.DefineConstructor(
                MethodAttributes.Public,
                CallingConventions.Standard,
                [| typeof<IFn> |]
            )
        let constructorIl = constructorBuilder.GetILGenerator()
        constructorIl.Emit(OpCodes.Ldarg_0)
        constructorIl.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
        constructorIl.Emit(OpCodes.Ldarg_0)
        constructorIl.Emit(OpCodes.Ldarg_1)
        constructorIl.Emit(OpCodes.Stfld, functionField)
        constructorIl.Emit(OpCodes.Ret)

        let delegateConversionsType =
            Assembly.GetExecutingAssembly().GetType("Wisp.Compiler.DelegateConversions")
        let convertResultMethod: MethodInfo =
            delegateConversionsType.GetMethod("Convert", [| typeof<obj>; typeof<Type> |])
        let getTypeFromHandle =
            typeof<Type>.GetMethod("GetTypeFromHandle", [| typeof<RuntimeTypeHandle> |])
        let intPtrConstructor = typeof<IntPtr>.GetConstructor([| typeof<int64> |])
        let intPtrToPointer = typeof<IntPtr>.GetMethod("ToPointer", Type.EmptyTypes)
        let readOnlyListGetItem =
            typeof<System.Collections.Generic.IReadOnlyList<obj>>.GetProperty("Item").GetGetMethod()

        let methodBuilder =
            adapterBuilder.DefineMethod(
                "Invoke",
                MethodAttributes.Public ||| MethodAttributes.HideBySig,
                invokeMethod.ReturnType,
                parameters |> Array.map (fun parameter -> parameter.ParameterType)
            )
        for index in 0 .. parameters.Length - 1 do
            methodBuilder.DefineParameter(index + 1, parameters.[index].Attributes, parameters.[index].Name)
            |> ignore

        let emitPointerAsIntPtr (il: ILGenerator) =
            il.Emit(OpCodes.Conv_I8)
            il.Emit(OpCodes.Newobj, intPtrConstructor)
            il.Emit(OpCodes.Box, typeof<IntPtr>)

        let emitArgument (il: ILGenerator) (index: int) =
            let parameter = parameters.[index]
            let parameterType = parameter.ParameterType
            if parameterType.IsByRef then
                let elementType = parameterType.GetElementType()
                if parameter.IsOut && not parameter.IsIn then
                    il.Emit OpCodes.Ldnull
                else
                    il.Emit(OpCodes.Ldarg, index + 1)
                    il.Emit(OpCodes.Ldobj, elementType)
                    if elementType.IsPointer then
                        emitPointerAsIntPtr il
                    elif elementType.IsValueType || elementType.IsGenericParameter then
                        il.Emit(OpCodes.Box, elementType)
            elif parameterType.IsPointer then
                il.Emit(OpCodes.Ldarg, index + 1)
                emitPointerAsIntPtr il
            else
                il.Emit(OpCodes.Ldarg, index + 1)
                if parameterType.IsValueType || parameterType.IsGenericParameter then
                    il.Emit(OpCodes.Box, parameterType)

        let emitToTargetType (il: ILGenerator) (targetType: Type) =
            if targetType.IsPointer then
                il.Emit(OpCodes.Ldtoken, typeof<IntPtr>)
                il.Emit(OpCodes.Call, getTypeFromHandle)
                il.EmitCall(OpCodes.Call, convertResultMethod, null)
                il.Emit(OpCodes.Unbox_Any, typeof<IntPtr>)
                il.Emit(OpCodes.Call, intPtrToPointer)
            elif targetType.IsGenericParameter then
                il.Emit(OpCodes.Unbox_Any, targetType)
            else
                il.Emit(OpCodes.Ldtoken, targetType)
                il.Emit(OpCodes.Call, getTypeFromHandle)
                il.EmitCall(OpCodes.Call, convertResultMethod, null)
                if targetType.IsValueType then
                    il.Emit(OpCodes.Unbox_Any, targetType)
                else
                    il.Emit(OpCodes.Castclass, targetType)

        let il = methodBuilder.GetILGenerator()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, functionField)
        il.Emit(OpCodes.Ldc_I4, parameters.Length)
        il.Emit(OpCodes.Newarr, typeof<obj>)
        for index in 0 .. parameters.Length - 1 do
            il.Emit(OpCodes.Dup)
            il.Emit(OpCodes.Ldc_I4, index)
            emitArgument il index
            il.Emit(OpCodes.Stelem_Ref)
        il.Emit(OpCodes.Callvirt, ifnInvoke)

        let writeBacks = ResizeArray<int>()
        for index in 0 .. parameters.Length - 1 do
            let parameter = parameters.[index]
            if parameter.ParameterType.IsByRef && not (parameter.IsIn && not parameter.IsOut) then
                writeBacks.Add index

        if writeBacks.Count > 0 then
            let resultLocal = il.DeclareLocal(typeof<obj>)
            let listLocal = il.DeclareLocal(typeof<System.Collections.Generic.IReadOnlyList<obj>>)
            il.Emit(OpCodes.Stloc, resultLocal)
            il.Emit(OpCodes.Ldloc, resultLocal)
            il.Emit(OpCodes.Castclass, typeof<System.Collections.Generic.IReadOnlyList<obj>>)
            il.Emit(OpCodes.Stloc, listLocal)
            for writeBackIndex in 0 .. writeBacks.Count - 1 do
                let parameterIndex = writeBacks.[writeBackIndex]
                let elementType = parameters.[parameterIndex].ParameterType.GetElementType()
                il.Emit(OpCodes.Ldarg, parameterIndex + 1)
                il.Emit(OpCodes.Ldloc, listLocal)
                il.Emit(OpCodes.Ldc_I4, writeBackIndex + 1)
                il.Emit(OpCodes.Callvirt, readOnlyListGetItem)
                emitToTargetType il elementType
                il.Emit(OpCodes.Stobj, elementType)
            if invokeMethod.ReturnType = typeof<Void> then
                il.Emit OpCodes.Ret
            else
                il.Emit(OpCodes.Ldloc, listLocal)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Callvirt, readOnlyListGetItem)
                emitToTargetType il invokeMethod.ReturnType
                il.Emit OpCodes.Ret
        else
            if invokeMethod.ReturnType = typeof<Void> then
                il.Emit OpCodes.Pop
            else
                emitToTargetType il invokeMethod.ReturnType
            il.Emit OpCodes.Ret
        adapterBuilder.CreateTypeInfo().AsType()

    let Create(delegateType: Type, functionValue: obj) =
        if not (typeof<Delegate>.IsAssignableFrom delegateType) then
            raise (ArgumentException(sprintf "%O is not a delegate type." delegateType))
        let adapterType = adapters.GetOrAdd(delegateType, buildAdapter)
        let constructor = adapterType.GetConstructor([| typeof<IFn> |])
        let adapter = constructor.Invoke([| functionValue |])
        let invokeMethod = adapterType.GetMethod("Invoke")
        Delegate.CreateDelegate(delegateType, adapter, invokeMethod)

    let Invoke(delegateValue: Delegate, arguments: obj[]) =
        let invokeMethod = delegateValue.GetType().GetMethod("Invoke")
        let parameters = invokeMethod.GetParameters()
        if parameters.Length <> arguments.Length then
            raise (ArgumentException(sprintf "Delegate expects %d argument(s), got %d." parameters.Length arguments.Length))
        let converted =
            arguments
            |> Array.mapi (fun index argument ->
                let targetType = parameters.[index].ParameterType
                DelegateConversions.tryConvert argument targetType |> Option.defaultValue argument)
        delegateValue.DynamicInvoke converted

module Clr =
    let private flagsInstance = BindingFlags.Public ||| BindingFlags.Instance
    let private flagsStatic = BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.FlattenHierarchy

    let resolveType (name: string) = TypeHints.tryResolveName name

    let symbolTypeName (symbol: Symbol) =
        match symbol.Namespace with
        | Some namespaceName -> namespaceName + "." + symbol.Name
        | None -> symbol.Name

    let private isNumericType (targetType: Type) =
        targetType = typeof<int>
        || targetType = typeof<int64>
        || targetType = typeof<int16>
        || targetType = typeof<sbyte>
        || targetType = typeof<byte>
        || targetType = typeof<uint16>
        || targetType = typeof<uint>
        || targetType = typeof<uint64>
        || targetType = typeof<single>
        || targetType = typeof<double>
        || targetType = typeof<decimal>

    let rec private tryConvert (value: obj) (targetType: Type) =
        let targetType = Nullable.GetUnderlyingType targetType |> Option.ofObj |> Option.defaultValue targetType
        if isNull value then
            if targetType.IsValueType then None else Some null
        elif targetType = typeof<obj> || targetType.IsInstanceOfType value then
            Some value
        elif targetType.IsArray && value :? System.Collections.IEnumerable && not (value :? string) then
            let elementType = targetType.GetElementType()
            let items = (value :?> System.Collections.IEnumerable) |> Seq.cast<obj> |> Seq.toArray
            let converted = Array.CreateInstance(elementType, items.Length)
            let mutable valid = true
            for index in 0 .. items.Length - 1 do
                if valid then
                    match tryConvert items.[index] elementType with
                    | Some item -> converted.SetValue(item, index)
                    | None -> valid <- false
            if valid then Some(converted :> obj) else None
        elif typeof<Delegate>.IsAssignableFrom targetType then
            match value with
            | :? Delegate as delegateValue when targetType.IsInstanceOfType delegateValue -> Some delegateValue
            | :? IFn ->
                try
                    Some(DelegateOps.Create(targetType, value) :> obj)
                with _ ->
                    None
            | _ -> None
        elif targetType.IsGenericType then
            let definition = targetType.GetGenericTypeDefinition()
            let genericArguments = targetType.GetGenericArguments()
            let isDictionaryType =
                genericArguments.Length = 2
                && (definition = typedefof<Dictionary<_, _>>
                    || definition = typedefof<IDictionary<_, _>>
                    || definition = typedefof<IReadOnlyDictionary<_, _>>)
            let elementType = if genericArguments.Length = 1 then genericArguments.[0] else typeof<obj>
            let isSequenceType =
                genericArguments.Length = 1
                && (definition = typedefof<List<_>>
                    || definition = typedefof<IList<_>>
                    || definition = typedefof<IReadOnlyList<_>>
                    || definition = typedefof<ICollection<_>>
                    || definition = typedefof<IEnumerable<_>>)
            let isSetType =
                genericArguments.Length = 1
                && (definition = typedefof<HashSet<_>>
                    || definition = typedefof<ISet<_>>)
            if isDictionaryType then
                match value with
                | :? IPersistentMap as map ->
                    let keyType = genericArguments.[0]
                    let valueType = genericArguments.[1]
                    let dictionaryType = typedefof<Dictionary<_, _>>.MakeGenericType genericArguments
                    let dictionary = Activator.CreateInstance dictionaryType
                    let addMethod = dictionaryType.GetMethod("Add", genericArguments)
                    let mutable valid = true
                    for entry in map.Entries do
                        if valid then
                            match tryConvert entry.Key keyType, tryConvert entry.Value valueType with
                            | Some key, Some entryValue ->
                                try
                                    addMethod.Invoke(dictionary, [| key; entryValue |]) |> ignore
                                with _ ->
                                    valid <- false
                            | _ -> valid <- false
                    if valid && targetType.IsAssignableFrom dictionaryType then Some dictionary else None
                | _ -> None
            elif (isSequenceType || isSetType) && value :? System.Collections.IEnumerable && not (value :? string) then
                let items =
                    match value with
                    | :? IReadOnlyList<obj>
                    | :? ISeq
                    | :? ISeqable -> Seqs.toList value
                    | :? System.Collections.IEnumerable as enumerable ->
                        enumerable |> Seq.cast<obj> |> Seq.toList
                    | _ -> []
                let collectionType =
                    (if isSetType then typedefof<HashSet<_>> else typedefof<List<_>>).MakeGenericType [| elementType |]
                let collection = Activator.CreateInstance collectionType
                let addMethod = collectionType.GetMethod("Add")
                let mutable valid = true
                for item in items do
                    if valid then
                        match tryConvert item elementType with
                        | Some converted -> addMethod.Invoke(collection, [| converted |]) |> ignore
                        | None -> valid <- false
                if valid && targetType.IsAssignableFrom collectionType then Some collection else None
            else
                None
        elif targetType.IsEnum then
            try
                Some(Enum.ToObject(targetType, value))
            with _ ->
                None
        elif isNumericType targetType && Numeric.isNumber value then
            try Some(Convert.ChangeType(value, targetType, CultureInfo.InvariantCulture))
            with _ -> None
        elif targetType = typeof<BigInteger> && Numeric.isNumber value then
            try
                match value with
                | :? BigInteger as number -> Some(box number)
                | :? double as number -> Some(box (BigInteger number))
                | :? decimal as number -> Some(box (BigInteger number))
                | _ -> Some(box (BigInteger(Numeric.toInt64 value)))
            with _ ->
                None
        else
            None

    let tryConvertValue (value: obj) (targetType: Type) = tryConvert value targetType

    let private tryBind (parameters: ParameterInfo[]) (arguments: obj[]) =
        if parameters.Length <> arguments.Length then
            None
        else
            let converted = Array.zeroCreate<obj> arguments.Length
            let mutable score = 0
            let mutable valid = true
            let mutable index = 0
            while valid && index < arguments.Length do
                let parameterType = parameters.[index].ParameterType
                match tryConvert arguments.[index] parameterType with
                | Some value ->
                    converted.[index] <- value
                    if isNull value || not (parameterType.IsInstanceOfType value) then
                        score <- score + 1
                    index <- index + 1
                | None ->
                    valid <- false
            if valid then Some(score, converted) else None

    let private describeType (clrType: Type) =
        if isNull clrType.FullName then clrType.Name else clrType.FullName

    let private describeArguments (arguments: obj[]) =
        arguments |> Array.map Print.printString |> String.concat " "

    let private invokeMethod (methodInfo: MethodInfo) (target: obj) (arguments: obj[]) =
        try
            let result = methodInfo.Invoke(target, arguments)
            if methodInfo.ReturnType = typeof<Void> then null else result
        with
        | :? TargetInvocationException as error when not (isNull error.InnerException) ->
            raise error.InnerException
        | :? TargetInvocationException as error ->
            raise (InteropException error.Message)

    let private findBestMethod (clrType: Type) (flags: BindingFlags) (memberName: string) (arguments: obj[]) =
        clrType.GetMethods flags
        |> Array.filter (fun methodInfo ->
            methodInfo.Name = memberName
            && not methodInfo.IsGenericMethodDefinition
            && methodInfo.GetParameters().Length = arguments.Length)
        |> Array.choose (fun methodInfo ->
            match tryBind (methodInfo.GetParameters()) arguments with
            | Some(score, converted) -> Some(score, methodInfo, converted)
            | None -> None)
        |> Array.sortBy (fun (score, _, _) -> score)
        |> Array.tryHead

    let private getPropertyOrField (clrType: Type) (flags: BindingFlags) (memberName: string) (target: obj) =
        try
            let propertyInfo = clrType.GetProperty(memberName, flags)
            if not (isNull propertyInfo) && propertyInfo.GetIndexParameters().Length = 0 then
                Some(propertyInfo.GetValue target)
            else
                let fieldInfo = clrType.GetField(memberName, flags)
                if isNull fieldInfo then None else Some(fieldInfo.GetValue target)
        with :? AmbiguousMatchException ->
            None

    let invokeStatic (clrType: Type) (memberName: string) (arguments: obj[]) =
        match findBestMethod clrType flagsStatic memberName arguments with
        | Some(_, methodInfo, converted) -> invokeMethod methodInfo null converted
        | None ->
            match getPropertyOrField clrType flagsStatic memberName null with
            | Some value when arguments.Length = 0 -> value
            | _ ->
                raise (
                    InteropException(
                        sprintf "No static member %s.%s matches (%s)." (describeType clrType) memberName (describeArguments arguments)
                    )
                )

    let invokeInstance (target: obj) (memberName: string) (arguments: obj[]) =
        if isNull target then
            raise (InteropException(sprintf "Cannot call instance member %s on nil." memberName))
        let clrType = target.GetType()
        match findBestMethod clrType flagsInstance memberName arguments with
        | Some(_, methodInfo, converted) -> invokeMethod methodInfo target converted
        | None ->
            match getPropertyOrField clrType flagsInstance memberName target with
            | Some value when arguments.Length = 0 -> value
            | _ ->
                raise (
                    InteropException(
                        sprintf "No instance member %s.%s matches (%s)." (describeType clrType) memberName (describeArguments arguments)
                    )
                )

    let getInstanceProperty (target: obj) (memberName: string) =
        if isNull target then
            raise (InteropException(sprintf "Cannot read instance member %s on nil." memberName))
        let clrType = target.GetType()
        match getPropertyOrField clrType flagsInstance memberName target with
        | Some value -> value
        | None -> raise (InteropException(sprintf "No instance property or field %s.%s exists." (describeType clrType) memberName))

    let construct (clrType: Type) (arguments: obj[]) =
        if clrType.IsAbstract || clrType.IsInterface then
            raise (InteropException(sprintf "%s cannot be instantiated." (describeType clrType)))
        let constructors =
            clrType.GetConstructors()
            |> Array.choose (fun constructorInfo ->
                match tryBind (constructorInfo.GetParameters()) arguments with
                | Some(score, converted) -> Some(score, constructorInfo, converted)
                | None -> None)
            |> Array.sortBy (fun (score, _, _) -> score)
        match constructors |> Array.tryHead with
        | Some(_, constructorInfo, converted) ->
            try
                constructorInfo.Invoke(converted)
            with :? TargetInvocationException as error when not (isNull error.InnerException) ->
                raise error.InnerException
        | None when arguments.Length = 0 && clrType.IsValueType -> Activator.CreateInstance clrType
        | None ->
            raise (
                InteropException(
                    sprintf "No constructor for %s matches (%s)." (describeType clrType) (describeArguments arguments)
                )
            )
