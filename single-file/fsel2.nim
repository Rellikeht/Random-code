import macros
import options

func nones[T](arr: var openArray[Option[T]]) =
  for i in low(arr)..high(arr):
    arr[i] = none[T]()

const asize = 10
var

  tab2 {.compileTime.}: array[asize, Option[proc(a, b: int): int]]
  tab1 {.compileTime.}: array[asize, Option[proc(a: int): int]]
  tab0 {.compileTime.}: array[asize, Option[proc(): int]]

static:
  nones(tab0)
  nones(tab1)
  nones(tab2)

macro makeLambda(num: int, fn: untyped): untyped =
  expectKind(fn, nnkProcDef)

  let
    alen = if len(fn[3]) > 1: len(fn[3][1]) else: 0
    ftype = nnkProcTy.newTree(fn[3], newEmptyNode())
    lambda = nnkLambda.newTree(
      newEmptyNode(),
      newEmptyNode(),
      newEmptyNode(),
      fn[3],
      newEmptyNode(),
      newEmptyNode(),
      fn[6]
      )

  quote("@") do:
    static:
      when @alen == 4: tab2[@num] = some[@ftype](@lambda)
      elif @alen == 3: tab1[@num] = some[@ftype](@lambda)
      else: tab0[@num] = some[@ftype](@lambda)

proc f(a, b: int): int {.makeLambda(1).} = a+b
proc f(a, b: int): int {.makeLambda(2).} = a*b
proc f(a: int): int {.makeLambda(3).} = 420*a
proc f(): int {.makeLambda(4).} = 3600

const privtab0 = tab0
var rtab0 = privtab0

static:
  for f in tab0:
    if isSome f:
      echo get(f)()

echo get(rtab0[4])()
