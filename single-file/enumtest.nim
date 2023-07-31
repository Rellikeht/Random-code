import options
import std/enumutils
import std/setutils

type
  u16 = uint16

  InternalRegister* = enum R0 R1 R2 R3 R4 R5 R6 R7
  ExternalRegister* = enum
    IP = 0
    SP = 1
    LR = 2
    UI = 4
    FL = 5
    CF = 7

proc inEnum[T: SomeInteger](e: typedesc[enum], n: T): bool =
  const
    lo = T(low(e))
    hi = T(high(e))
  let
    values {.global.} = block:
      var possible: set[lo..hi]
      for i in items(e): possible[T(i)] = true
      possible
  return n >= lo and n <= hi and n in values

proc intRegister(val: u8|u16): Option[InternalRegister] =
  if inEnum(InternalRegister, val): return some(InternalRegister(val))
  else: return none(InternalRegister)

proc extRegister(val: u8|u16): Option[ExternalRegister] =
  if inEnum(ExternalRegister, val): return some(ExternalRegister(val))
  else: return none(ExternalRegister)

for i in u16(0)..u16(10):
    #echo($ i & " " & $ get regopt)

    let reg1 = intRegister(i)
    if not isSome(reg1):
      echo "Bad internal register " & $ i
    let reg2 = extRegister(i)
    if not isSome(reg2):
      echo "Bad external register " & $ i
