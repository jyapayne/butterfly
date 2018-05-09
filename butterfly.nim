import macros
import tables
import strutils
import typetraits

proc `$`*[T](ar: openarray[T]): string =
    ## Converts an array into a string
    result = "["
    if ar.len() > 0:
        result &= $ar[0]
    for i in 1..ar.len()-1:
        result &= ", " & $ar[i]
    result &= "]"

proc typeToStr*[T](some:typedesc[T]): string = name(T)

template tupleObjToStr(obj): string {.dirty.} =
  var res = typeToStr(type(obj))
  template helper(n) {.gensym.} =
    res.add("(")
    var firstElement = true
    for name, value in n.fieldPairs():
      when compiles(value):
        if not firstElement:
          res.add(", ")
        res.add(name)
        res.add(": ")
        when (value is object or value is tuple):
          when (value is tuple):
            res.add("tuple " & typeToStr(type(value)))
          else:
            res.add(typeToStr(type(value)))
          helper(value)
        elif (value is string):
          res.add("\"" & $value & "\"")
        else:
          res.add($value)
        firstElement = false
    res.add(")")
  helper(obj)
  res

proc `$`*(s: ref object): string =
  result = "ref " & tupleObjToStr(s[]).replace(":ObjectType", "")

proc objToStr*[T: object](obj: var T): string =
  tupleObjToStr(obj)

proc objToStr*[T: tuple](obj: T): string =
  result = "tuple " & tupleObjToStr(obj)

macro toString*(obj: typed): untyped =
  ## this macro is to work around not being
  ## able to override system.`$`
  ##
  ## Basically, I want to use my proc to print
  ## objects and tuples, but the regular $ for
  ## everything else
  let kind = obj.getType().typeKind
  case kind:
    of ntyTuple, ntyObject:
      template toStrAst(obj): string =
        einheit.objToStr(obj)
      result = getAst(toStrAst(obj))
    of ntyString:
      template toStrAst(obj): string =
        "\"" & $(obj) & "\""
      result = getAst(toStrAst(obj))
    else:
      template toStrAst(obj): string =
        $(obj)
      result = getAst(toStrAst(obj))


type

  LitKind = enum
    lkBoolLit,
    lkCharLit,
    lkIntLit, lkInt8Lit,
    lkInt16Lit, lkInt32Lit, lkInt64Lit,
    lkUIntLit, lkUInt8Lit,
    lkUInt16Lit, lkUInt32Lit, lkUInt64Lit,
    lkFloatLit, lkFloat32Lit, lkFloat64Lit,
    lkStrLit

  Literal = ref object
    case kind: LitKind
    of lkBoolLit:
      boolVal: bool
    of lkCharLit:
      charVal: char
    of lkIntLit:
      intVal: int
    of lkInt8Lit:
      int8Val: int8
    of lkInt16Lit:
      int16Val: int16
    of lkInt32Lit:
      int32Val: int32
    of lkInt64Lit:
      int64Val: int64
    of lkUIntLit:
      uintVal: uint
    of lkUInt8Lit:
      uint8Val: uint8
    of lkUInt16Lit:
      uint16Val: uint16
    of lkUInt32Lit:
      uint32Val: uint32
    of lkUInt64Lit:
      uint64Val: uint64
    of lkFloatLit:
      floatVal: float
    of lkFloat32Lit:
      float32Val: float32
    of lkFloat64Lit:
      float64Val: float64
    of lkStrLit:
      strVal: string

  FieldType = enum
    ftObject,
    ftLiteral

  Field = ref object
    case kind: FieldType
    of ftObject:
      objVal: Object
    of ftLiteral:
      litVal: Literal

  Object = ref object
    name: string
    fields: TableRef[string, Field]

  ObjectType = ref object
    name: string
    fields: TableRef[string, string]

  TypeType = enum
    ttObject,
    ttLiteral

  Type = ref object
    case kind: TypeType
    of ttObject:
      objType: ObjectType
    of ttLiteral:
      litType: string

  Context = ref object
    locals: TableRef[string, Context]
    stmts: seq[string]
    types: TableRef[string, Type]
    parent: Context

proc value(lit: Literal): string =
  case lit.kind:
  of lkBoolLit:
    result = $lit.boolVal
  of lkCharLit:
    result = "'" & $lit.charVal & "'"
  of lkIntLit:
    result = $lit.intVal
  of lkInt8Lit:
    result = $lit.int8Val
  of lkInt16Lit:
    result = $lit.int16Val
  of lkInt32Lit:
    result = $lit.int32Val
  of lkInt64Lit:
    result = $lit.int64Val
  of lkUIntLit:
    result = $lit.uintVal
  of lkUInt8Lit:
    result = $lit.uint8Val
  of lkUInt16Lit:
    result = $lit.uint16Val
  of lkUInt32Lit:
    result = $lit.uint32Val
  of lkUInt64Lit:
    result = $lit.uint64Val
  of lkFloatLit:
    result = $lit.floatVal
  of lkFloat32Lit:
    result = $lit.float32Val
  of lkFloat64Lit:
    result = $lit.float64Val
  of lkStrLit:
    result = "\"" & $lit.strVal & "\""


proc type(lit: Literal): string =
  case lit.kind:
  of lkBoolLit:
    result = "bool"
  of lkCharLit:
    result = "char"
  of lkIntLit:
    result = "int"
  of lkInt8Lit:
    result = "int8"
  of lkInt16Lit:
    result = "int16"
  of lkInt32Lit:
    result = "int32"
  of lkInt64Lit:
    result = "int64"
  of lkUIntLit:
    result = "uint"
  of lkUInt8Lit:
    result = "uint8"
  of lkUInt16Lit:
    result = "uint16"
  of lkUInt32Lit:
    result = "uint32"
  of lkUInt64Lit:
    result = "uint64"
  of lkFloatLit:
    result = "float"
  of lkFloat32Lit:
    result = "float32"
  of lkFloat64Lit:
    result = "float64"
  of lkStrLit:
    result = "string"

proc type*(obj: Object): string
proc value*(obj: Object): string

proc type*(field: Field): string =
  case field.kind:
  of ftObject:
    result = butterfly.type(field.objVal)
  of ftLiteral:
    result = butterfly.type(field.litVal)

proc value*(field: Field): string =
  case field.kind:
  of ftObject:
    result = butterfly.value(field.objVal)
  of ftLiteral:
    result = butterfly.value(field.litVal)

proc type*(obj: Object): string =
  result = obj.name & "("

  var fieldData: seq[string] = @[]

  for fname in obj.fields.keys():
    let value = obj.fields[fname]
    let ftype = butterfly.type(value)
    fieldData.add(fname & ": " & ftype)

  result &= fieldData.join(", ")
  result &= ")"

proc value*(obj: Object): string =
  result = obj.name & "("

  var fieldData: seq[string] = @[]

  for fname in obj.fields.keys():
    let value = obj.fields[fname]
    let fval = butterfly.value(value)
    fieldData.add(fname & ": " & fval)

  result &= fieldData.join(", ")
  result &= ")"

proc `value=`*[T](lit: var Literal, value: T) =
  when T is char:
    lit.charVal = value
  elif T is bool:
    lit.boolVal = value
  elif T is int:
    lit.intVal = value
  elif T is int8:
    lit.int8Val = value
  elif T is int16:
    lit.int16Val = value
  elif T is int32:
    lit.int32Val = value
  elif T is int64:
    lit.int64Val = value
  elif T is uint:
    lit.intVal = value
  elif T is uint8:
    lit.int8Val = value
  elif T is uint16:
    lit.int16Val = value
  elif T is uint32:
    lit.int32Val = value
  elif T is uint64:
    lit.int64Val = value
  elif T is float:
    lit.floatVal = value
  elif T is float32:
    lit.float32Val = value
  elif T is float64:
    lit.float64Val = value
  elif T is string:
    lit.strVal = value

var procTable = newTable[string, Context]()

proc debugProc() =
  echo "Debug"

proc updateProcTable(procName: string, params: string) =
  let
    key = procName & params

  if not procTable.hasKey(key):
    procTable[key] = new(Context)

template doNodes(nodeKind: NimNodeKind, node: NimNode, code: untyped) {.dirty.}=
  var stackN: seq[NimNode] = @[node]

  while stackN.len() > 0:
    let newNode = stackN.pop()
    for i in 0 ..< newNode.len():
      let n = newNode[i]
      if n.kind == nodeKind:
        code
      else:
        stackN.add(n)


iterator getNode(nodeKind: NimNodeKind, node: NimNode): NimNode =
  var stack: seq[NimNode] = @[node]

  while stack.len() > 0:
    let newNode = stack.pop()
    for i in 0 ..< newNode.len():
      let child = newNode[i]
      if child.kind == nodeKind:
        yield child
      else:
        stack.add(child)

template addStmtTree(parentx: NimNode, nodex: NimNode, indexx: int): untyped {.dirty.} =
  proc helper(parent: NimNode, node: NimNode, index: int): NimNode =
    var newList = newNimNode(nnkStmtList)

    for child in node.children:

      if child.kind == nnkIfStmt:
        for subch in child.children:
          if subch.kind == nnkElifBranch:
            discard helper(subch, subch[1], 1)
          else:
            discard helper(subch, subch[0], 0)

      if child.kind == nnkBlockStmt:
          discard helper(child, child[1], 1)


      let line = child.lineInfoObj()
      echo "Inserting debug on line: " & $line.line
      newList.add(newCall(bindSym("debugProc")))
      newList.add(child)
      for n in getNode(nnkCall, child):
        if n.kind != nnkEmpty:
          let
            impl = n[0].getImpl()
            procName = $impl[0].toStrLit()
            params = $impl[3].toStrLit()
            key = procName & params

          if not visitedProcs.hasKey(key):
            # this can be added once a compiler bug is fixed
            #[newList.add(
              newCall(
                bindSym("updateProcTable"),
                newLit(procName),
                newLit(params)
              )
            )]#
            visitedProcs[key] = true
            # add the next stmt list onto the stack
            # of the newly discovered proc
            if impl[6].kind == nnkStmtList:
              discard helper(impl, impl[6], 6)
              echo impl.toStrLit

    newList.add(newCall(bindSym("debugProc")))
    parent[index] = newList
    return parent
  helper(parentx, nodex, indexx)


macro debug(code: typed): untyped =
  var visitedProcs = newTable[string, bool]()

  code[0] = addStmtTree(code[0], code[0][6], 6)

  result = code
  echo code.treeRepr
  echo code.toStrLit()

# This proc now has it's stmts replaced!
proc otherProc(stuff: string): string =
  let a = "beans"
  echo stuff & a
  result = stuff & a

debug:
  proc main() =
    let x = "balls".otherProc()
    let y = otherProc("balls2")
    block:
      echo "Beans"
      echo "Beans"
    if x == "ballsbeans":
      echo "Yaya"
      echo "who"
    else:
      echo "balls"
      echo "multi"


when isMainModule:
  main()
