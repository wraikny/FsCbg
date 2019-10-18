#load "FsCbg.fs"

module FsCbg =
  open FsCbg

  let inline cppFullname (x: ^a) =
    let ns = (^a: (member namespace_: string) x)
    let name = (^a: (member name: string) x)
    ns |> function
    | null | "" -> name
    | _ -> sprintf "%s::%s" ns name
  
  type CppType<'lang when 'lang : comparison> with
    member inline x.CppName() =
      x |> function
      | Void -> "void"
      | Int -> "itn32_t"
      | Float -> "float"
      | Bool -> "bool"
      | String -> "const char16_t*"
      | Class x ->
        sprintf "std::shared_ptr<%s>" <| cppFullname x
      | Struct x -> cppFullname x
      | Enum x -> cppFullname x
  
    member inline x.CName(?isReturn: bool) =
      let isReturn = defaultArg isReturn false

      x |> function
      | Void | Int | Float
      | Bool | String -> x.CppName()
      | Struct x when isReturn ->
        cppFullname x
      | Enum _ -> "int32_t"
      | Struct _
      | Class _ -> "void*"
  
  type CppGenerator<'lang when 'lang : comparison> = {
    define: Define<'lang>
    outputPath: string
    header: string
    funcNameCreateAndAddSharedPtr: string
    funcNameAddAndGetSharedPtr: string
  } with
    static member inline Init(define, path) = {
      define = define
      outputPath = path
      header = ""
      funcNameCreateAndAddSharedPtr = "CreateAndAddSharedPtr"
      funcNameAddAndGetSharedPtr = "AddAndGetSharedPtr"
    }
  
  let convertCtoCpp (gen: _ CppGenerator) (type_: _ CppType) (name: string) =
    type_ |> function
    | Int | Float | Bool | String -> name
    | Class x ->
      let fullname = cppFullname x
      sprintf "%s<%s>((%s*)%s)"
        gen.funcNameCreateAndAddSharedPtr
        fullname fullname name
    | Struct x ->
      sprintf "(*((%s*)%s))" (cppFullname x) name
    | Enum x ->
      sprintf "(%s::%s)%s" x.namespace_ x.name name
    | Void -> invalidOp "Cannot convert Void"
  
  let convertRet (gen: _ CppGenerator) (type_: _ CppType) (name: string) =
    type_ |> function
    | Int | Float | Bool | String -> name
    | Class x ->
      sprintf "(void*)%s<%s>(%s)"
        gen.funcNameAddAndGetSharedPtr
        (cppFullname x) name
    | Struct _ -> sprintf "(%s)" name
    | Enum _ -> sprintf "(int32_t)%s" name
    | Void -> invalidOp "Cannot convert Void"
  

  let generateFunc (x: _ Class, func: _ Function) =
    let fname = Define.getCFuncName x func

    let args =
      [ for arg in func.args ->
        sprintf "%s %s"
          ( arg.type_.CName() )
          arg.name
      ]
    
    let args =
      if not (func.isStatic || func.isConstructor) then
        "void* cbg_self"::args
      else args

    let name =
      if func.isConstructor then Class x else func.returnValue.type_
      |> fun x -> x.CName(true)

    Code.Zero
    ++ (
      sprintf "CBGEXPORT %s CBGSTDCALL %s(%s) {"
        name fname (args |> String.concat ", ")
    )

    |> Code.str 4