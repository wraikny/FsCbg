namespace FsCbg

[<AutoOpenAttribute>]
module Utils =
  let inline zero< ^a when ^a: (static member Zero: ^a) > =
    LanguagePrimitives.GenericZero< ^a >
  let inline one< ^a when ^a: (static member One: ^a) > =
    LanguagePrimitives.GenericOne< ^a >


type Description<'lang when 'lang : comparison> = {
  descs: Map<'lang, string>
} with
  static member inline Zero = {
    descs = Map.empty
  }

type EnumFlag =
  | IntFlag of int
  | ByteFlag of byte
  | NoFlag

type EnumValue<'lang when 'lang : comparison> = {
  name: string
  desc: 'lang Description
  value: EnumFlag
}

type CppType<'lang when 'lang : comparison> =
  | Void
  | Int
  | Float
  | Bool
  | String
  | Enum of 'lang Enum
  | Struct of 'lang Struct
  | Class of 'lang Class

and Argument<'lang when 'lang : comparison> = {
  name: string
  type_: 'lang CppType
  desc: 'lang Description
} with
  static member inline Zero = {
    name = ""
    type_ = Void
    desc = zero
  }

and ReturnValue<'lang when 'lang : comparison> = {
  type_: 'lang CppType
  desc: 'lang Description
  doCache: bool
} with
  member inline x.Type = x.type_
  member inline x.Desc = x.desc
  member inline x.DoCache =
    x.type_ |> function
    | Class _ -> x.doCache
    | _ -> false
  
  static member inline Zero = {
    type_ = Void
    desc = zero
    doCache = false
  }

and Function<'lang when 'lang : comparison> = {
  name: string
  brief: 'lang Description
  drsc: 'lang Description
  args: 'lang Argument list
  returnValue: 'lang ReturnValue
  isStatic: bool
  isConstructor: bool
} with
  static member inline Zero = {
    name = ""
    brief = zero
    drsc = zero
    args = List.empty
    returnValue = zero
    isStatic = false
    isConstructor = false
  }

and Property<'lang when 'lang : comparison> = {
  name: string
  type_: 'lang CppType
  hasGetter: bool
  hasSetter: bool
  brief: 'lang Description
  cacheSetValue: bool
} with
  static member inline Zero = {
    name = ""
    type_ = Void
    hasGetter = false
    hasSetter = false
    brief = zero
    cacheSetValue = false
  }

and Enum<'lang when 'lang : comparison> = {
    name: string
    values: EnumValue<'lang> list
    brief: 'lang Description
    desc: 'lang Description
    namespace_: string
} with
  static member inline Zero = {
    name = ""
    values = List.empty
    brief = zero
    desc = zero
    namespace_ = ""
  }

and Field<'lang when 'lang : comparison> = {
  name: string
  type_: 'lang CppType
}

and Struct<'lang when 'lang : comparison> = {
  name: string
  fields: 'lang Field list
  namespace_: string
} with
  static member inline Zero = {
    name = ""
    fields = List.empty
    namespace_ = ""
  }

and Class<'lang when 'lang : comparison> = {
  name: string
  funcs: 'lang Function list
  properties: 'lang Property list
  constructors: 'lang Function list
  doCache: bool
  namespace_: string
}

module Description =
  let inline add lang text desc =
    { desc with
        descs = Map.add lang text desc.descs
    }

module Argument =
  let inline init type' name: _ Argument =
    { zero with type_ = type'; name = name }

module Property =
  let inline getter (x: _ Property): _ Function =
    { zero with
        name = sprintf "Get%s" x.name
        returnValue = { zero with type_ = x.type_ }
    }

  let inline setter (x: _ Property): _ Function =
    { zero with
        name = sprintf "Get%s" x.name
        args = [
          { name = sprintf "Set%s" x.name; type_ = x.type_; desc = zero }
        ]
    }

type Define<'lang when 'lang : comparison> = {
  enums: 'lang Enum list
  classes: 'lang Class list
  structs: 'lang Struct list
}

[<RequireQualifiedAccess>]
module Define =
  let inline getCFuncName (x: _ Class) (f: _ Function) =
    sprintf "cbg_%s_%s" x.name f.name

  let inline getCReleaseFuncName (x: _ Class) =
    sprintf "cbg_%s_Release" x.name


type Code = {
  indent: int
  lines: (int * string) list
} with
  static member inline Zero = { indent = zero; lines = [] }
  
  static member inline (++) (code: Code, s: string) =
    { code with lines = (code.indent, s)::code.lines }
  
  static member inline (++) (code: Code, x: Code) =
    let lines =
      List.append
        (List.map (fun (i, s) -> (i + code.indent, s)) x.lines)
        code.lines
    { code with lines = lines }
  
  static member inline (+>) (code: Code, s: string) =
    { code with lines = (code.indent + one, s)::code.lines}

  static member inline (+>) (code: Code, x: Code) =
    let lines =
      List.append
        (List.map (fun (i, s) -> (i + code.indent + one, s)) x.lines)
        code.lines
    { code with lines = lines }


[<RequireQualifiedAccess>]
module Code =
  let inline add (s: string) (code: Code) : Code = code ++ s

  let inline indent (s: string) (code: Code): Code = code +> s

  let inline incr code = { code with indent = code.indent + one }
  let inline decr code = { code with indent = code.indent - one }

  let rec str (indentSpace: int) (code: Code): string =
    let lines = code.lines |> Seq.rev
    let space: string =
      [| for _ in 1..indentSpace -> ' ' |]
      |> System.String

    seq {
      for (indent, s) in lines do
        let space = seq { for _ in 1..indent -> space } |> String.concat ""
        let ss: string [] = s.Split [|'\n'|]
        for s in ss -> sprintf "%s%s" space s
    }
    |> String.concat "\n"