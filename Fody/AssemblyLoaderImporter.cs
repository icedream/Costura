using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using TypeAttributes = Mono.Cecil.TypeAttributes;

partial class ModuleWeaver
{
    readonly ConstructorInfo instructionConstructorInfo = typeof(Instruction).GetConstructor(BindingFlags.NonPublic | BindingFlags.Instance, null, new[] { typeof(OpCode), typeof(object) }, null);

    bool hasUnmanaged;

    MethodDefinition attachMethod;
    MethodDefinition loaderCctor;
    FieldDefinition assemblyNamesField;
    FieldDefinition symbolNamesField;
    FieldDefinition preloadListField;
    FieldDefinition preload32ListField;
    FieldDefinition preload64ListField;
    FieldDefinition checksumsField;

    void ImportAssemblyLoader(bool createTemporaryAssemblies)
    {
        var moduleDefinition = GetTemplateModuleDefinition();

        TypeDefinition sourceType;
        if (createTemporaryAssemblies)
        {
            sourceType = moduleDefinition.Types.First(x => x.Name == "ILTemplateWithTempAssembly");
            DumpSource("ILTemplateWithTempAssembly");
        }
        else if (hasUnmanaged)
        {
            sourceType = moduleDefinition.Types.First(x => x.Name == "ILTemplateWithUnmanagedHandler");
            DumpSource("ILTemplateWithUnmanagedHandler");
        }
        else
        {
            sourceType = moduleDefinition.Types.First(x => x.Name == "ILTemplate");
            DumpSource("ILTemplate");
        }

        var commonType = moduleDefinition.Types.First(x => x.Name == "Common");
        DumpSource("Common");

        CopyFields(sourceType, commonType);
        CopyMethods(sourceType, commonType);
        sourceType = commonType;

        var targetType = new TypeDefinition("Costura", "AssemblyLoader", sourceType.Attributes,
            Resolve(sourceType.BaseType, sourceType, sourceType)); // TODO uh...
        targetType.CustomAttributes.Add(new CustomAttribute(compilerGeneratedAttributeCtor));
        ModuleDefinition.Types.Add(targetType);
        CopyFields(sourceType, targetType);
        CopyMethod(sourceType.Methods.First(x => x.Name == "ResolveAssembly"), targetType);

        loaderCctor = CopyMethod(sourceType.Methods.First(x => x.IsConstructor && x.IsStatic), targetType);
        attachMethod = CopyMethod(sourceType.Methods.First(x => x.Name == "Attach"), targetType);

        foreach (var field in targetType.Fields)
        {
            if (field.Name == "assemblyNames")
                assemblyNamesField = field;
            if (field.Name == "symbolNames")
                symbolNamesField = field;
            if (field.Name == "preloadList")
                preloadListField = field;
            if (field.Name == "preload32List")
                preload32ListField = field;
            if (field.Name == "preload64List")
                preload64ListField = field;
            if (field.Name == "checksums")
                checksumsField = field;
        }
    }

    void DumpSource(string file)
    {
        string localFile = Path.Combine(Path.GetDirectoryName(AssemblyFilePath), file + ".cs");

        if (File.Exists(localFile))
            return;

        using (var stream = GetType().Assembly.GetManifestResourceStream(String.Format("Costura.Fody.template.{0}.cs", file)))
        {
            using (var outStream = new FileStream(localFile, FileMode.Create))
                stream.CopyTo(outStream);
        }
    }

    void CopyFields(TypeDefinition sourceType, TypeDefinition targetType)
    {
        foreach (var field in sourceType.Fields)
        {
            var newField = new FieldDefinition(field.Name, field.Attributes,
                Resolve(field.FieldType, sourceType, targetType));
            if (field.HasConstant)
                newField.Constant = field.Constant;
            targetType.Fields.Add(newField);
        }
    }

    ModuleDefinition GetTemplateModuleDefinition()
    {
        var readerParameters = new ReaderParameters
                                   {
                                       AssemblyResolver = AssemblyResolver,
                                       ReadSymbols = true,
                                       SymbolStream = GetType().Assembly.GetManifestResourceStream("Costura.Fody.Template.pdb"),
                                   };

        using (var resourceStream = GetType().Assembly.GetManifestResourceStream("Costura.Fody.Template.dll"))
        {
            return ModuleDefinition.ReadModule(resourceStream, readerParameters);
        }
    }

    TypeReference Resolve(TypeReference baseType, TypeDefinition sourceType, TypeDefinition targetType)
    {
        var typeDefinition = baseType.Resolve();
        var typeReference = ModuleDefinition.Import(typeDefinition);
        if (baseType is ArrayType)
        {
            return new ArrayType(typeReference);
        }
        if (baseType.IsGenericInstance)
        {
            typeReference = typeReference.MakeGenericInstanceType(baseType.GetGenericInstanceArguments().ToArray());
        }
        return typeReference;
    }

    TypeDefinition CopyNestedType(TypeDefinition templateType, TypeDefinition targetType, bool makePrivate = false)
    {
        if (templateType == null)
            throw new ArgumentNullException("templateType");
        if (targetType == null)
            throw new ArgumentNullException("targetType");
        var attributes = templateType.Attributes;
        if (makePrivate)
        {
            attributes &= ~TypeAttributes.Public;
        }

        if (templateType.Attributes.HasFlag(TypeAttributes.Public))
        {
            attributes |= TypeAttributes.NestedPublic;
        }
        else
        {
            attributes |= TypeAttributes.NestedPrivate;
        }

        var newType = new TypeDefinition(templateType.Namespace, templateType.Name, attributes)
        {
            DeclaringType = targetType,
            BaseType = Resolve(templateType.BaseType, templateType, targetType),
            Attributes = attributes | TypeAttributes.NestedPrivate
        };
        CopyFields(templateType, newType);
        CopyMethods(templateType, newType);
        
        targetType.NestedTypes.Add(newType);
        return newType;
    }

    private void CopyMethods(TypeDefinition templateType, TypeDefinition targetType, bool makePrivate = false)
    {
        foreach (var method in templateType.Methods)
        {
            CopyMethod(method, targetType, makePrivate);
        }
    }

    MethodDefinition CopyMethod(MethodDefinition templateMethod, TypeDefinition targetType, bool makePrivate = false)
    {
        var attributes = templateMethod.Attributes;
        if (makePrivate)
        {
            attributes &= ~Mono.Cecil.MethodAttributes.Public;
            attributes |= Mono.Cecil.MethodAttributes.Private;
        }
        var returnType = Resolve(templateMethod.ReturnType, templateMethod.DeclaringType, targetType);
        var newMethod = new MethodDefinition(templateMethod.Name, attributes, returnType)
                            {
                                IsPInvokeImpl = templateMethod.IsPInvokeImpl,
                                IsPreserveSig = templateMethod.IsPreserveSig,
                                DeclaringType = targetType
                            };
        if (templateMethod.IsPInvokeImpl)
        {
            var moduleRef = ModuleDefinition.ModuleReferences.FirstOrDefault(mr => mr.Name == templateMethod.PInvokeInfo.Module.Name);
            if (moduleRef == null)
            {
                moduleRef = new ModuleReference(templateMethod.PInvokeInfo.Module.Name);
                ModuleDefinition.ModuleReferences.Add(moduleRef);
            }
            newMethod.PInvokeInfo = new PInvokeInfo(templateMethod.PInvokeInfo.Attributes, templateMethod.PInvokeInfo.EntryPoint, moduleRef);
        }

        if (templateMethod.Body != null)
        {
            newMethod.Body.InitLocals = templateMethod.Body.InitLocals;
            CopyInstructions(templateMethod, newMethod);
            CopyExceptionHandlers(templateMethod, newMethod);
            foreach (var variableDefinition in templateMethod.Body.Variables)
            {
                var newVariableDefinition = new VariableDefinition(Import(variableDefinition.VariableType, templateMethod.DeclaringType, targetType) as TypeReference);
                newVariableDefinition.Name = variableDefinition.Name;
                newMethod.Body.Variables.Add(newVariableDefinition);
            }
        }
        foreach (var parameterDefinition in templateMethod.Parameters)
        {
            var newParameterDefinition = new ParameterDefinition(Resolve(parameterDefinition.ParameterType, templateMethod.DeclaringType, targetType));
            newParameterDefinition.Name = parameterDefinition.Name;
            newMethod.Parameters.Add(newParameterDefinition);
        }

        targetType.Methods.Add(newMethod);
        return newMethod;
    }

    void CopyExceptionHandlers(MethodDefinition templateMethod, MethodDefinition newMethod)
    {
        if (!templateMethod.Body.HasExceptionHandlers)
        {
            return;
        }
        foreach (var exceptionHandler in templateMethod.Body.ExceptionHandlers)
        {
            var handler = new ExceptionHandler(exceptionHandler.HandlerType);
            var templateInstructions = templateMethod.Body.Instructions;
            var targetInstructions = newMethod.Body.Instructions;
            if (exceptionHandler.TryStart != null)
            {
                handler.TryStart = targetInstructions[templateInstructions.IndexOf(exceptionHandler.TryStart)];
            }
            if (exceptionHandler.TryEnd != null)
            {
                handler.TryEnd = targetInstructions[templateInstructions.IndexOf(exceptionHandler.TryEnd)];
            }
            if (exceptionHandler.HandlerStart != null)
            {
                handler.HandlerStart = targetInstructions[templateInstructions.IndexOf(exceptionHandler.HandlerStart)];
            }
            if (exceptionHandler.HandlerEnd != null)
            {
                handler.HandlerEnd = targetInstructions[templateInstructions.IndexOf(exceptionHandler.HandlerEnd)];
            }
            if (exceptionHandler.FilterStart != null)
            {
                handler.FilterStart = targetInstructions[templateInstructions.IndexOf(exceptionHandler.FilterStart)];
            }
            if (exceptionHandler.CatchType != null)
            {
                handler.CatchType = Resolve(exceptionHandler.CatchType, templateMethod.DeclaringType, newMethod.DeclaringType);
            }
            newMethod.Body.ExceptionHandlers.Add(handler);
        }
    }

    void CopyInstructions(MethodDefinition templateMethod, MethodDefinition newMethod)
    {
        foreach (var instruction in templateMethod.Body.Instructions)
        {
            newMethod.Body.Instructions.Add(CloneInstruction(instruction, templateMethod.DeclaringType, newMethod.DeclaringType));
        }
    }

    Instruction CloneInstruction(Instruction instruction, TypeDefinition sourceType, TypeDefinition targetType)
    {
        Instruction newInstruction;
        if (instruction.OpCode == OpCodes.Ldstr && ((string)instruction.Operand) == "To be replaced at compile time")
        {
            newInstruction = Instruction.Create(OpCodes.Ldstr, resourcesHash);
        }
        else
        {
            newInstruction = (Instruction)instructionConstructorInfo.Invoke(new[] { instruction.OpCode, instruction.Operand });
            newInstruction.Operand = Import(instruction.Operand, sourceType, targetType);
        }
        newInstruction.SequencePoint = TranslateSequencePoint(instruction.SequencePoint);
        return newInstruction;
    }

    SequencePoint TranslateSequencePoint(SequencePoint sequencePoint)
    {
        if (sequencePoint == null)
            return null;

        var document = new Document(Path.Combine(Path.GetDirectoryName(AssemblyFilePath), Path.GetFileName(sequencePoint.Document.Url)))
        {
            Language = sequencePoint.Document.Language,
            LanguageVendor = sequencePoint.Document.LanguageVendor,
            Type = sequencePoint.Document.Type,
        };

        return new SequencePoint(document)
        {
            StartLine = sequencePoint.StartLine,
            StartColumn = sequencePoint.StartColumn,
            EndLine = sequencePoint.EndLine,
            EndColumn = sequencePoint.EndColumn,
        };
    }

    object Import(object operand, TypeDefinition sourceType, TypeDefinition targetType)
    {
        if (operand == null)
            return null;

        var typeReference = operand as TypeReference;
        if (typeReference != null)
        {
            if (typeReference.IsDeclaredInside(sourceType))
            {
                var t = targetType.NestedTypes.FirstOrDefault(x => x.Name == typeReference.Name)
                    ?? CopyNestedType(typeReference.DeclaringType.Resolve().NestedTypes.First(x => x.Name == typeReference.Name), targetType);
                return t;
            }
            return Resolve(typeReference, sourceType, targetType);
        }

        var memberReference = operand as MemberReference;
        if (memberReference == null)
        {
            return operand;
        }

        TypeDefinition targetDeclaringType = null;
        TypeDefinition sourceDeclaringType = null;
        
        if (memberReference.DeclaringType.FullName == sourceType.FullName)
        {
            // source type
            sourceDeclaringType = sourceType;
            targetDeclaringType = targetType;
        }
        else if (memberReference.DeclaringType.IsDeclaredInside(sourceType))
        {
            // nested type
            // TODO: Recurse nested types
            sourceDeclaringType = sourceType.NestedTypes.First(n => n.Name == memberReference.DeclaringType.Name);
            targetDeclaringType = targetType.NestedTypes.FirstOrDefault(x => x.Name == memberReference.DeclaringType.Name)
                ?? CopyNestedType(memberReference.DeclaringType.Resolve(), targetType);
        }

        var methodReference = operand as MethodReference;
        if (methodReference != null)
        {
            if (sourceDeclaringType != null)
            {
                var mr = targetDeclaringType.Methods.FirstOrDefault(x => x.Name == methodReference.Name && x.Parameters.Count == methodReference.Parameters.Count)
                    ?? CopyMethod(sourceDeclaringType.Methods.First(m => m.Name == methodReference.Name && m.Parameters.Count == methodReference.Parameters.Count),
                        targetDeclaringType,
                        sourceDeclaringType != sourceType);
                return mr;
            }

            if (methodReference.DeclaringType.IsGenericInstance)
            {
                return ModuleDefinition.Import(methodReference.Resolve())
                    .MakeHostInstanceGeneric(methodReference.DeclaringType.GetGenericInstanceArguments().ToArray());
            }

            return ModuleDefinition.Import(methodReference.Resolve());
        }

        var fieldReference = operand as FieldReference;
        if (fieldReference != null)
        {
            if (sourceDeclaringType != null)
            {
                return targetDeclaringType.Fields.FirstOrDefault(f => f.Name == fieldReference.Name);
            }

            return ModuleDefinition.Import(fieldReference.Resolve());
        }
        return operand;
    }
}