namespace Wisp.Compiler

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open Wisp.Core

type RecordTypeInfo =
    { DisplayName: string
      Fields: RecordField list
      FieldNames: string list
      ClrType: Type
      Constructor: obj[] -> obj
      ConstructFromMap: IPersistentMap -> obj }

type InterfaceTypeInfo =
    { Name: string
      ClrType: Type }

/// Emits real CLR interfaces from definterface declarations.
[<Sealed>]
type InterfaceRegistry() =
    let assemblyName = AssemblyName("Wisp.Interfaces." + Guid.NewGuid().ToString("N"))
    let assembly = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndCollect)
    let moduleBuilder = assembly.DefineDynamicModule(assemblyName.Name)
    let mutable counter = 0
    let types = ConcurrentDictionary<string, InterfaceTypeInfo>(StringComparer.Ordinal)

    let sanitize (name: string) =
        let chars =
            name
            |> Seq.map (fun ch -> if Char.IsLetterOrDigit ch || ch = '_' then ch else '_')
            |> Seq.toArray
        String(chars)

    member _.Assembly = assembly :> Assembly

    member _.TryFind(name: string) =
        match types.TryGetValue name with
        | true, info -> Some info
        | false, _ -> None

    member _.Define(definition: InterfaceDefinition) =
        if String.IsNullOrWhiteSpace definition.Name then
            invalidArg "definition" "An interface name cannot be empty."
        let index = counter
        counter <- counter + 1
        let typeBuilder =
            moduleBuilder.DefineType(
                sprintf "Wisp.Generated.Interfaces.I%d_%s" index (sanitize definition.Name),
                TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract
            )
        for methodInfo in definition.Methods do
            let methodBuilder =
                typeBuilder.DefineMethod(
                    methodInfo.Name,
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.Abstract
                    ||| MethodAttributes.HideBySig
                    ||| MethodAttributes.NewSlot,
                    methodInfo.ReturnType,
                    methodInfo.Parameters |> List.map (fun parameter -> parameter.ParameterType) |> Array.ofList
                )
            for index in 0 .. methodInfo.Parameters.Length - 1 do
                let parameter = methodInfo.Parameters.[index]
                methodBuilder.DefineParameter(index + 1, parameter.Attributes, sprintf "arg%d" index)
                |> ignore
        let created = typeBuilder.CreateTypeInfo().AsType()
        let info = { Name = definition.Name; ClrType = created }
        types.[definition.Name] <- info
        TypeHints.registerType definition.Name created
        info.ClrType

/// Emits custom CLR delegate types declared with defdelegate.
[<Sealed>]
type DelegateRegistry() =
    let assemblyName = AssemblyName("Wisp.Delegates.Declared." + Guid.NewGuid().ToString("N"))
    let assembly = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndCollect)
    let moduleBuilder = assembly.DefineDynamicModule(assemblyName.Name)
    let mutable counter = 0
    let types = ConcurrentDictionary<string, Type>(StringComparer.Ordinal)

    let sanitize (name: string) =
        name |> Seq.map (fun ch -> if Char.IsLetterOrDigit ch || ch = '_' then ch else '_') |> Seq.toArray |> String

    member _.Assembly = assembly :> Assembly

    member _.TryFind(name: string) =
        match types.TryGetValue name with
        | true, clrType -> Some clrType
        | false, _ -> None

    member _.Define(definition: DelegateDefinition) =
        if String.IsNullOrWhiteSpace definition.Name then
            invalidArg "definition" "A delegate name cannot be empty."
        let index = counter
        counter <- counter + 1
        let typeBuilder =
            moduleBuilder.DefineType(
                sprintf "Wisp.Generated.Delegates.D%d_%s" index (sanitize definition.Name),
                TypeAttributes.Public ||| TypeAttributes.Sealed,
                typeof<MulticastDelegate>
            )
        let constructorBuilder =
            typeBuilder.DefineConstructor(
                MethodAttributes.Public
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.SpecialName
                ||| MethodAttributes.RTSpecialName,
                CallingConventions.Standard,
                [| typeof<obj>; typeof<IntPtr> |]
            )
        constructorBuilder.SetImplementationFlags(MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed)
        let invokeBuilder =
            typeBuilder.DefineMethod(
                "Invoke",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.NewSlot
                ||| MethodAttributes.HideBySig,
                definition.ReturnType,
                definition.Parameters
                |> List.map (fun parameter -> parameter.ParameterType)
                |> Array.ofList
            )
        for index in 0 .. definition.Parameters.Length - 1 do
            let parameter = definition.Parameters.[index]
            invokeBuilder.DefineParameter(index + 1, parameter.Attributes, sprintf "arg%d" index)
            |> ignore
        invokeBuilder.SetImplementationFlags(MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed)
        let created = typeBuilder.CreateTypeInfo().AsType()
        types.[definition.Name] <- created
        TypeHints.registerType definition.Name created
        created

/// Generates real CLR classes deriving from WispRecordBase. Type-hinted fields
/// become typed CLR properties and typed constructor parameters; the base class
/// retains the boxed value map used by printing and value equality. Generic
/// definitions emit a CLR generic type definition whose field hints may refer
/// to its own type parameters.
[<Sealed>]
type RecordRegistry() =
    let assemblyName = AssemblyName("Wisp.Records." + Guid.NewGuid().ToString("N"))
    let assembly = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndCollect)
    let moduleBuilder = assembly.DefineDynamicModule(assemblyName.Name)
    let types = ConcurrentDictionary<string, RecordTypeInfo>(StringComparer.Ordinal)
    let mutable counter = 0

    let sanitize (name: string) =
        let chars =
            name
            |> Seq.map (fun ch -> if Char.IsLetterOrDigit ch || ch = '_' then ch else '_')
            |> Seq.toArray
        String(chars)

    let constraintType
        (resolver: string -> Type option)
        (environment: Map<string, Type>)
        (expression: TypeExpr)
        =
        match TypeHints.resolveWithTypeParametersAndResolver resolver environment (TypeReference expression) with
        | Ok resolved -> resolved
        | Error errors ->
            let message =
                errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message) |> String.concat "; "
            raise (EvaluationException(sprintf "Cannot resolve type constraint: %s" message, None))

    let applyGenericParameters
        (resolver: string -> Type option)
        (typeBuilder: TypeBuilder)
        (definition: TypeDefinition)
        (environment: Map<string, Type>)
        =
        let names = definition.TypeParameters |> List.map (fun parameter -> parameter.Name) |> Array.ofList
        let builders = typeBuilder.DefineGenericParameters names
        let mutable updatedEnvironment = environment
        for index in 0 .. builders.Length - 1 do
            let parameter = definition.TypeParameters.[index]
            let builder = builders.[index]
            updatedEnvironment <- updatedEnvironment.Add(parameter.Name, builder :> Type)

        for index in 0 .. builders.Length - 1 do
            let parameter = definition.TypeParameters.[index]
            let builder = builders.[index]
            let mutable flags = GenericParameterAttributes.None
            if parameter.Variance = Covariant then flags <- flags ||| GenericParameterAttributes.Covariant
            if parameter.Variance = Contravariant then flags <- flags ||| GenericParameterAttributes.Contravariant
            let interfaceTypes = ResizeArray<Type>()
            for constraintValue in parameter.Constraints do
                match constraintValue with
                | ReferenceTypeConstraint -> flags <- flags ||| GenericParameterAttributes.ReferenceTypeConstraint
                | ValueTypeConstraint
                | UnmanagedConstraint -> flags <- flags ||| GenericParameterAttributes.NotNullableValueTypeConstraint
                | DefaultConstructorConstraint -> flags <- flags ||| GenericParameterAttributes.DefaultConstructorConstraint
                | BaseTypeConstraint expression ->
                    builder.SetBaseTypeConstraint(constraintType resolver updatedEnvironment expression)
                | InterfaceConstraint expression ->
                    interfaceTypes.Add(constraintType resolver updatedEnvironment expression)
                | TypeParameterConstraint name ->
                    match updatedEnvironment.TryFind name with
                    | Some bound -> builder.SetBaseTypeConstraint bound
                    | None ->
                        raise (EvaluationException(sprintf "Unknown constraint type parameter %s." name, None))
                | NullableReferenceTypeConstraint
                | NotNullConstraint
                | AllowsRefStructConstraint
                | DefaultConstraint -> ()
            builder.SetGenericParameterAttributes flags
            if interfaceTypes.Count > 0 then
                builder.SetInterfaceConstraints(interfaceTypes.ToArray())

        updatedEnvironment

    member _.Assembly = assembly :> Assembly

    member _.TryFind(name: string) =
        match types.TryGetValue name with
        | true, info -> Some info
        | false, _ -> None

    member _.Define
        (
            name: string,
            fields: RecordField list,
            definition: TypeDefinition option,
            isStruct: bool,
            resolver: string -> Type option
        )
        =
        if String.IsNullOrWhiteSpace name then invalidArg (nameof name) "A type name cannot be empty."
        let fieldNames = fields |> List.map (fun field -> field.Name)
        if fieldNames |> List.distinct |> List.length <> fieldNames.Length then
            invalidArg (nameof fields) "Record field names must be unique."

        let describeHintErrors (errors: TypeHintError list) =
            errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message) |> String.concat "; "

        let resolveDefinitionType environment (expression: TypeExpr) =
            match TypeHints.resolveWithTypeParametersAndResolver resolver environment (TypeReference expression) with
            | Ok resolved -> resolved
            | Error errors ->
                raise (
                    EvaluationException(
                        sprintf "Cannot resolve type %s: %s" (sprintf "%A" expression) (describeHintErrors errors),
                        None
                    )
                )

        let baseType =
            if isStruct then
                typeof<ValueType>
            else
                match definition |> Option.bind (fun definition -> definition.BaseType) with
                | None -> typeof<WispRecordBase>
                | Some expression ->
                    let resolved = resolveDefinitionType Map.empty expression
                    if resolved = typeof<obj> then typeof<WispRecordBase>
                    elif typeof<WispRecordBase>.IsAssignableFrom resolved then resolved
                    else
                        raise (
                            EvaluationException(
                                sprintf "%s must derive from WispRecordBase to be a record base." resolved.FullName,
                                None
                            )
                        )

        let index = counter
        counter <- counter + 1
        let clrName = sprintf "Wisp.Generated.Records.R%d_%s" index (sanitize name)
        let typeAttributes =
            if isStruct then TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.SequentialLayout
            else TypeAttributes.Public
        let typeBuilder = moduleBuilder.DefineType(clrName, typeAttributes, baseType)

        let environment =
            match definition with
            | Some definition when definition.Arity > 0 ->
                applyGenericParameters resolver typeBuilder definition Map.empty
            | _ -> Map.empty

        let typeParameterNames =
            definition
            |> Option.map (fun definition ->
                definition.TypeParameters |> List.map (fun parameter -> parameter.Name) |> Set.ofList)
            |> Option.defaultValue Set.empty

        let rec rewriteTypeParameters (expression: TypeExpr) =
            match expression with
            | NamedType(name, None, []) when typeParameterNames.Contains name -> TypeParameter name
            | NamedType(name, arity, args) -> NamedType(name, arity, args |> List.map rewriteTypeParameters)
            | TypeParameter name -> TypeParameter name
            | ArrayType(element, rank) -> ArrayType(rewriteTypeParameters element, rank)
            | PointerType element -> PointerType(rewriteTypeParameters element)
            | ByRefType element -> ByRefType(rewriteTypeParameters element)

        let interfaceTypes =
            match definition with
            | None -> []
            | Some definition ->
                let implemented = baseType.GetInterfaces()
                definition.Interfaces
                |> List.map (resolveDefinitionType environment)
                |> List.distinct
                |> List.filter (fun interfaceType ->
                    if not interfaceType.IsInterface then
                        raise (EvaluationException(sprintf "%O is not an interface." interfaceType, None))
                    let unsupported =
                        interfaceType.GetMethods()
                        |> Array.exists (fun methodInfo ->
                            (methodInfo.IsStatic && methodInfo.IsAbstract)
                            || methodInfo.IsGenericMethodDefinition
                            || methodInfo.ReturnType.IsByRef
                            || methodInfo.ReturnType.IsPointer
                            || methodInfo.GetParameters()
                               |> Array.exists (fun parameter -> parameter.ParameterType.IsPointer))
                    if unsupported then
                        raise (
                            EvaluationException(
                                sprintf "Interface %O has members records cannot implement." interfaceType,
                                None
                            )
                        )
                    not (implemented |> Array.contains interfaceType))

        for interfaceType in interfaceTypes do
            typeBuilder.AddInterfaceImplementation interfaceType

        let resolveField (field: RecordField) =
            match field.Hint with
            | None -> field
            | Some hintForm ->
                match TypeHints.parse hintForm with
                | Ok(TypeReference expression) ->
                    let hint = TypeReference(rewriteTypeParameters expression)
                    match TypeHints.resolveWithTypeParametersAndResolver resolver environment hint with
                    | Ok resolved -> { field with FieldType = resolved; Hint = None }
                    | Error errors ->
                        let notes =
                            errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message)
                        raise (
                            EvaluationException(
                                sprintf "Cannot resolve type hint for field %s: %s" field.Name (String.concat "; " notes),
                                None
                            )
                        )
                | Ok(TypeDefinitionHint _) ->
                    raise (EvaluationException(sprintf "Field %s cannot use a type definition as a field type." field.Name, None))
                | Error errors ->
                    let notes =
                        errors |> List.map (fun error -> sprintf "%s: %s" error.Path error.Message)
                    raise (
                        EvaluationException(
                            sprintf "Invalid type hint for field %s: %s" field.Name (String.concat "; " notes),
                            None
                        )
                    )
        let fields = fields |> List.map resolveField
        let fieldArray = fields |> Array.ofList
        // Wisp-level method dispatch keys on the deftype symbol, so the record
        // TypeName must match it even when the CLR hint uses a qualified name.
        let displayName = name
        let isGeneric = definition |> Option.exists (fun definition -> definition.Arity > 0)

        let recordRuntimeContextType =
            typeof<WispRecordBase>.Assembly.GetType("Wisp.Core.RecordRuntimeContext")
        if isNull recordRuntimeContextType then
            raise (EvaluationException("RecordRuntimeContext is unavailable.", None))
        let recordRuntimeContextGetCurrent = recordRuntimeContextType.GetMethod("getCurrent")
        let recordSupportType = typeof<WispRecordBase>.Assembly.GetType("Wisp.Core.RecordSupport")
        if isNull recordSupportType then
            raise (EvaluationException("RecordSupport is unavailable.", None))
        let fieldNamesHelper =
            recordSupportType.GetMethod("FieldNames", [| typeof<string[]> |])
        let fieldsHelper =
            recordSupportType.GetMethod("Fields", [| typeof<string>; typeof<string[]>; typeof<obj[]> |])
        let equalsHelper =
            recordSupportType.GetMethod(
                "Equals",
                [| typeof<string>; typeof<string[]>; typeof<obj[]>; typeof<obj> |]
            )
        let hashHelper =
            recordSupportType.GetMethod("Hash", [| typeof<string>; typeof<string[]>; typeof<obj[]> |])
        let toStringHelper =
            recordSupportType.GetMethod("ToString", [| typeof<string>; typeof<string[]>; typeof<obj[]> |])

        let structFields =
            if isStruct then
                fieldArray
                |> Array.mapi (fun index field ->
                    typeBuilder.DefineField(sprintf "field_%d" index, field.FieldType, FieldAttributes.Private))
            else
                Array.empty<FieldBuilder>

        let structRuntimeField =
            if isStruct then
                Some(typeBuilder.DefineField("_runtime", typeof<obj>, FieldAttributes.Private))
            else
                None

        if isStruct then
            let constructorBuilder =
                typeBuilder.DefineConstructor(
                    MethodAttributes.Public,
                    CallingConventions.Standard,
                    fieldArray |> Array.map (fun field -> field.FieldType)
                )
            let constructorIl = constructorBuilder.GetILGenerator()
            for fieldIndex in 0 .. fieldArray.Length - 1 do
                constructorIl.Emit(OpCodes.Ldarg_0)
                constructorIl.Emit(OpCodes.Ldarg, fieldIndex + 1)
                constructorIl.Emit(OpCodes.Stfld, structFields.[fieldIndex])
            constructorIl.Emit(OpCodes.Ldarg_0)
            constructorIl.Emit(OpCodes.Call, recordRuntimeContextGetCurrent)
            constructorIl.Emit(OpCodes.Stfld, structRuntimeField.Value)
            constructorIl.Emit(OpCodes.Ret)
        else
            let baseConstructor =
                baseType.GetConstructor([| typeof<string>; typeof<string[]>; typeof<obj[]> |])
            if isNull baseConstructor then
                raise (
                    EvaluationException(
                        sprintf "Base type %O must expose a (string, string[], obj[]) constructor." baseType,
                        None
                    )
                )
            let constructorBuilder =
                typeBuilder.DefineConstructor(
                    MethodAttributes.Public,
                    CallingConventions.Standard,
                    fieldArray |> Array.map (fun field -> field.FieldType)
                )
            let constructorIl = constructorBuilder.GetILGenerator()
            constructorIl.Emit(OpCodes.Ldarg_0)
            constructorIl.Emit(OpCodes.Ldstr, displayName)
            constructorIl.Emit(OpCodes.Ldc_I4, fieldArray.Length)
            constructorIl.Emit(OpCodes.Newarr, typeof<string>)
            for fieldIndex in 0 .. fieldArray.Length - 1 do
                constructorIl.Emit(OpCodes.Dup)
                constructorIl.Emit(OpCodes.Ldc_I4, fieldIndex)
                constructorIl.Emit(OpCodes.Ldstr, fieldArray.[fieldIndex].Name)
                constructorIl.Emit(OpCodes.Stelem_Ref)
            constructorIl.Emit(OpCodes.Ldc_I4, fieldArray.Length)
            constructorIl.Emit(OpCodes.Newarr, typeof<obj>)
            for fieldIndex in 0 .. fieldArray.Length - 1 do
                constructorIl.Emit(OpCodes.Dup)
                constructorIl.Emit(OpCodes.Ldc_I4, fieldIndex)
                constructorIl.Emit(OpCodes.Ldarg, fieldIndex + 1)
                if fieldArray.[fieldIndex].FieldType.IsValueType || fieldArray.[fieldIndex].FieldType.IsGenericParameter then
                    constructorIl.Emit(OpCodes.Box, fieldArray.[fieldIndex].FieldType)
                constructorIl.Emit(OpCodes.Stelem_Ref)
            constructorIl.Emit(OpCodes.Call, baseConstructor)
            constructorIl.Emit(OpCodes.Ret)

        let emitStringArray (il: ILGenerator) =
            il.Emit(OpCodes.Ldc_I4, fieldArray.Length)
            il.Emit(OpCodes.Newarr, typeof<string>)
            for fieldIndex in 0 .. fieldArray.Length - 1 do
                il.Emit(OpCodes.Dup)
                il.Emit(OpCodes.Ldc_I4, fieldIndex)
                il.Emit(OpCodes.Ldstr, fieldArray.[fieldIndex].Name)
                il.Emit(OpCodes.Stelem_Ref)

        let emitValueArray (il: ILGenerator) =
            il.Emit(OpCodes.Ldc_I4, fieldArray.Length)
            il.Emit(OpCodes.Newarr, typeof<obj>)
            for fieldIndex in 0 .. fieldArray.Length - 1 do
                il.Emit(OpCodes.Dup)
                il.Emit(OpCodes.Ldc_I4, fieldIndex)
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldfld, structFields.[fieldIndex])
                if fieldArray.[fieldIndex].FieldType.IsValueType || fieldArray.[fieldIndex].FieldType.IsGenericParameter then
                    il.Emit(OpCodes.Box, fieldArray.[fieldIndex].FieldType)
                il.Emit(OpCodes.Stelem_Ref)

        if isStruct then
            typeBuilder.AddInterfaceImplementation(typeof<IWispRecord>)

            let defineInterfaceGetter
                (propertyName: string)
                (methodName: string)
                (returnType: Type)
                (interfaceMethod: MethodInfo)
                (emit: ILGenerator -> unit)
                =
                let methodBuilder =
                    typeBuilder.DefineMethod(
                        methodName,
                        MethodAttributes.Public
                        ||| MethodAttributes.Virtual
                        ||| MethodAttributes.Final
                        ||| MethodAttributes.SpecialName
                        ||| MethodAttributes.HideBySig
                        ||| MethodAttributes.NewSlot,
                        returnType,
                        Type.EmptyTypes
                    )
                let il = methodBuilder.GetILGenerator()
                emit il
                il.Emit OpCodes.Ret
                let propertyBuilder = typeBuilder.DefineProperty(propertyName, PropertyAttributes.None, returnType, [||])
                propertyBuilder.SetGetMethod methodBuilder
                typeBuilder.DefineMethodOverride(methodBuilder, interfaceMethod)
                methodBuilder

            defineInterfaceGetter
                "TypeName"
                "get_TypeName"
                typeof<string>
                (typeof<IWispRecord>.GetProperty("TypeName").GetGetMethod())
                (fun il -> il.Emit(OpCodes.Ldstr, displayName))
            |> ignore

            defineInterfaceGetter
                "FieldNames"
                "get_FieldNames"
                (typeof<IWispRecord>.GetProperty("FieldNames").PropertyType)
                (typeof<IWispRecord>.GetProperty("FieldNames").GetGetMethod())
                (fun il ->
                    emitStringArray il
                    il.Emit(OpCodes.Call, fieldNamesHelper))
            |> ignore

            defineInterfaceGetter
                "Fields"
                "get_Fields"
                typeof<IPersistentMap>
                (typeof<IWispRecord>.GetProperty("Fields").GetGetMethod())
                (fun il ->
                    il.Emit(OpCodes.Ldstr, displayName)
                    emitStringArray il
                    emitValueArray il
                    il.Emit(OpCodes.Call, fieldsHelper))
            |> ignore

            let defineOverride (name: string) (returnType: Type) (parameterTypes: Type[]) (baseMethod: MethodInfo) (emit: ILGenerator -> unit) =
                let methodBuilder =
                    typeBuilder.DefineMethod(
                        name,
                        MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig,
                        returnType,
                        parameterTypes
                    )
                let il = methodBuilder.GetILGenerator()
                emit il
                il.Emit OpCodes.Ret
                typeBuilder.DefineMethodOverride(methodBuilder, baseMethod)

            defineOverride
                "Equals"
                typeof<bool>
                [| typeof<obj> |]
                (typeof<obj>.GetMethod("Equals", [| typeof<obj> |]))
                (fun il ->
                    il.Emit(OpCodes.Ldstr, displayName)
                    emitStringArray il
                    emitValueArray il
                    il.Emit(OpCodes.Ldarg_1)
                    il.Emit(OpCodes.Call, equalsHelper))

            defineOverride
                "GetHashCode"
                typeof<int>
                Type.EmptyTypes
                (typeof<obj>.GetMethod("GetHashCode", Type.EmptyTypes))
                (fun il ->
                    il.Emit(OpCodes.Ldstr, displayName)
                    emitStringArray il
                    emitValueArray il
                    il.Emit(OpCodes.Call, hashHelper))

            defineOverride
                "ToString"
                typeof<string>
                Type.EmptyTypes
                (typeof<obj>.GetMethod("ToString", Type.EmptyTypes))
                (fun il ->
                    il.Emit(OpCodes.Ldstr, displayName)
                    emitStringArray il
                    emitValueArray il
                    il.Emit(OpCodes.Call, toStringHelper))

        let baseGet = typeof<WispRecordBase>.GetMethod("Get")
        for fieldIndex in 0 .. fieldArray.Length - 1 do
            let field = fieldArray.[fieldIndex]
            let propertyBuilder = typeBuilder.DefineProperty(field.Name, PropertyAttributes.None, field.FieldType, [||])
            let getterBuilder =
                typeBuilder.DefineMethod(
                    sprintf "get_%s_%d" (sanitize field.Name) fieldIndex,
                    MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig,
                    field.FieldType,
                    Type.EmptyTypes
                )
            let getterIl = getterBuilder.GetILGenerator()
            getterIl.Emit(OpCodes.Ldarg_0)
            if isStruct then
                getterIl.Emit(OpCodes.Ldfld, structFields.[fieldIndex])
            else
                getterIl.Emit(OpCodes.Ldc_I4, fieldIndex)
                getterIl.Emit(OpCodes.Call, baseGet)
                if field.FieldType.IsValueType || field.FieldType.IsGenericParameter then
                    getterIl.Emit(OpCodes.Unbox_Any, field.FieldType)
                else
                    getterIl.Emit(OpCodes.Castclass, field.FieldType)
            getterIl.Emit(OpCodes.Ret)
            propertyBuilder.SetGetMethod getterBuilder

        if not interfaceTypes.IsEmpty then
            let runtimeOpsType = Assembly.GetExecutingAssembly().GetType("Wisp.Compiler.RuntimeOps")
            if isNull runtimeOpsType then
                raise (EvaluationException("RuntimeOps is unavailable for interface dispatch.", None))
            let invokeRecordMethod =
                runtimeOpsType.GetMethod("InvokeRecordMethod", BindingFlags.Public ||| BindingFlags.Static)
            if isNull invokeRecordMethod then
                raise (EvaluationException("RuntimeOps.InvokeRecordMethod is missing.", None))
            let convertResult =
                runtimeOpsType.GetMethod("ConvertResult", BindingFlags.Public ||| BindingFlags.Static)
            if isNull convertResult then
                raise (EvaluationException("RuntimeOps.ConvertResult is missing.", None))
            let runtimeGetter = typeof<WispRecordBase>.GetProperty("Runtime").GetGetMethod()
            let runtimeType = invokeRecordMethod.GetParameters().[0].ParameterType
            let getTypeFromHandle =
                typeof<Type>.GetMethod("GetTypeFromHandle", [| typeof<RuntimeTypeHandle> |])

            let emitted = Dictionary<string, MethodBuilder * ResizeArray<MethodInfo>>()

            let signatureKey (methodInfo: MethodInfo) =
                let parameters =
                    methodInfo.GetParameters()
                    |> Array.map (fun parameter ->
                        let parameterType = parameter.ParameterType
                        if isNull parameterType.FullName then parameterType.Name else parameterType.FullName)
                    |> String.concat ","
                methodInfo.Name + "(" + parameters + ")"

            let baseImplements (methodInfo: MethodInfo) =
                let parameterTypes = methodInfo.GetParameters() |> Array.map (fun parameter -> parameter.ParameterType)
                try
                    match baseType.GetMethod(methodInfo.Name, parameterTypes) with
                    | null -> false
                    | found -> not found.IsAbstract
                with _ ->
                    false

            let readOnlyListGetItem =
                typeof<System.Collections.Generic.IReadOnlyList<obj>>.GetProperty("Item").GetGetMethod()

            let emitConvertFromObject (il: ILGenerator) (targetType: Type) =
                il.Emit(OpCodes.Ldtoken, targetType)
                il.Emit(OpCodes.Call, getTypeFromHandle)
                il.Emit(OpCodes.Call, convertResult)
                if targetType.IsValueType || targetType.IsGenericParameter then
                    il.Emit(OpCodes.Unbox_Any, targetType)
                else
                    il.Emit(OpCodes.Castclass, targetType)

            let emitStub (methodInfo: MethodInfo) =
                let parameters = methodInfo.GetParameters()
                let parameterTypes = parameters |> Array.map (fun parameter -> parameter.ParameterType)
                let stub =
                    typeBuilder.DefineMethod(
                        methodInfo.Name,
                        MethodAttributes.Public
                        ||| MethodAttributes.Virtual
                        ||| MethodAttributes.Final
                        ||| MethodAttributes.HideBySig
                        ||| MethodAttributes.NewSlot,
                        methodInfo.ReturnType,
                        parameterTypes
                    )
                for index in 0 .. parameters.Length - 1 do
                    stub.DefineParameter(index + 1, parameters.[index].Attributes, parameters.[index].Name)
                    |> ignore
                let il = stub.GetILGenerator()
                il.Emit(OpCodes.Ldarg_0)
                match structRuntimeField with
                | Some runtimeField -> il.Emit(OpCodes.Ldfld, runtimeField)
                | None -> il.Emit(OpCodes.Call, runtimeGetter)
                il.Emit(OpCodes.Castclass, runtimeType)
                il.Emit(OpCodes.Ldarg_0)
                if isStruct then
                    il.Emit(OpCodes.Ldobj, typeBuilder)
                    il.Emit(OpCodes.Box, typeBuilder)
                il.Emit(OpCodes.Ldstr, methodInfo.Name)
                let arguments = il.DeclareLocal(typeof<obj[]>)
                il.Emit(OpCodes.Ldc_I4, parameters.Length)
                il.Emit(OpCodes.Newarr, typeof<obj>)
                il.Emit(OpCodes.Stloc, arguments)
                for index in 0 .. parameters.Length - 1 do
                    let parameter = parameters.[index]
                    let parameterType = parameter.ParameterType
                    il.Emit(OpCodes.Ldloc, arguments)
                    il.Emit(OpCodes.Ldc_I4, index)
                    if parameterType.IsByRef then
                        let elementType = parameterType.GetElementType()
                        if parameter.IsOut && not parameter.IsIn then
                            il.Emit OpCodes.Ldnull
                        else
                            il.Emit(OpCodes.Ldarg, index + 1)
                            il.Emit(OpCodes.Ldobj, elementType)
                            if elementType.IsValueType || elementType.IsGenericParameter then
                                il.Emit(OpCodes.Box, elementType)
                    else
                        il.Emit(OpCodes.Ldarg, index + 1)
                        if parameterType.IsValueType || parameterType.IsGenericParameter then
                            il.Emit(OpCodes.Box, parameterType)
                    il.Emit(OpCodes.Stelem_Ref)
                il.Emit(OpCodes.Ldloc, arguments)
                il.Emit(OpCodes.Call, invokeRecordMethod)

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
                        emitConvertFromObject il elementType
                        il.Emit(OpCodes.Stobj, elementType)
                    if methodInfo.ReturnType = typeof<Void> then
                        il.Emit OpCodes.Ret
                    else
                        il.Emit(OpCodes.Ldloc, listLocal)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Callvirt, readOnlyListGetItem)
                        emitConvertFromObject il methodInfo.ReturnType
                        il.Emit OpCodes.Ret
                else
                    if methodInfo.ReturnType = typeof<Void> then
                        il.Emit OpCodes.Pop
                    elif methodInfo.ReturnType.IsGenericParameter then
                        il.Emit(OpCodes.Unbox_Any, methodInfo.ReturnType)
                    else
                        emitConvertFromObject il methodInfo.ReturnType
                    il.Emit OpCodes.Ret
                stub

            for interfaceType in interfaceTypes do
                for methodInfo in interfaceType.GetMethods() do
                    if methodInfo.IsAbstract && not methodInfo.IsStatic && not (baseImplements methodInfo) then
                        let key = signatureKey methodInfo
                        match emitted.TryGetValue key with
                        | true, (_, overrides) -> overrides.Add methodInfo
                        | false, _ ->
                            let stub = emitStub methodInfo
                            let overrides = ResizeArray<MethodInfo>()
                            overrides.Add methodInfo
                            emitted.[key] <- (stub, overrides)

            for entry in emitted do
                let stub, overrides = entry.Value
                for interfaceMethod in overrides do
                    typeBuilder.DefineMethodOverride(stub, interfaceMethod)

        let created = typeBuilder.CreateTypeInfo().AsType()

        let constructorFunction (values: obj[]) =
            if isGeneric then
                raise (
                    EvaluationException(
                        sprintf "Generic record %s needs type arguments; use (new [:Name ...] ...)." displayName,
                        None
                    )
                )
            let constructor = created.GetConstructor(fieldArray |> Array.map (fun field -> field.FieldType))
            if values.Length <> fieldArray.Length then
                raise (ArityException(string fieldArray.Length, values.Length))
            let converted = Array.zeroCreate<obj> fieldArray.Length
            for fieldIndex in 0 .. fieldArray.Length - 1 do
                match Clr.tryConvertValue values.[fieldIndex] fieldArray.[fieldIndex].FieldType with
                | Some value -> converted.[fieldIndex] <- value
                | None ->
                    raise (
                        EvaluationException(
                            sprintf "Cannot convert field %s to %O." fieldArray.[fieldIndex].Name fieldArray.[fieldIndex].FieldType,
                            None
                        )
                    )
            constructor.Invoke(converted)

        let constructFromMap (map: IPersistentMap) =
            let values =
                fieldArray
                |> Array.map (fun field ->
                    match map.TryGet(Keyword.Intern field.Name) with
                    | Some value -> value
                    | None -> null)
            constructorFunction values

        let info =
            { DisplayName = displayName
              Fields = fields
              FieldNames = fieldNames
              ClrType = created
              Constructor = constructorFunction
              ConstructFromMap = constructFromMap }
        types.[name] <- info
        match definition with
        | Some definition -> types.[definition.Name] <- info
        | None -> ()
        info
