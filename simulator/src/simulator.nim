import math
import options
import std/enumutils
import std/setutils

import parseutils
import strutils
import os
#import parseopt

# Because why not
template TODO(_: varargs[untyped]) {.used.} = discard


# DEFINITIONS


# TODO D maybe distinct numeric types
type
  u8 = uint8
  u16 = uint16

  MemVal* = u8
  MemAddr* = u16
  RegisterValue* = u16
  Imm16* = u16

  InternalRegister* = enum R0 R1 R2 R3 R4 R5 R6 R7
  ExternalRegister* = enum
    IP = 0
    SP = 1
    LR = 2
    UI = 4
    FL = 5
    CF = 7

  # It is weird that range is a type not a value
  # This is probably to use it as inices for arrays, but still
  intRegRange = range[int(low(InternalRegister))..int(high(InternalRegister))]
  extRegRange = range[int(low(ExternalRegister))..int(high(ExternalRegister))]

  Instruction = u16
  OpcodeIndex = u16

  Registers = InternalRegister | ExternalRegister
  Memory = array[MemAddr, MemVal]
  Instructions = seq[Instruction]

  MachineState* = enum
    ready
    halted
    exceptional

  MachineInfo* = object
    case status: MachineState
      of ready, halted: discard
      of exceptional:
        info*: string

  # Feels like OOP
  Machine* = object
    memory: Memory
    iregisters: array[intRegRange, RegisterValue]
    eregisters: array[extRegRange, RegisterValue]

  Opcode* = enum
    NOP = 0x00
    HLT = 0x01
    MOV = 0x05
    RDM = 0x06
    WRM = 0x07
    RDX = 0x0C
    WRX = 0x0D
    PSH = 0x0E
    POP = 0x0F
    MUL = 0x10
    CMP = 0x11
    TST = 0x13
    JMP = 0x14
    CAL = 0x15
    RET = 0x16
    ADD = 0x18
    SUB = 0x19
    NOT = 0x1A
    AND = 0x1B
    ORR = 0x1C
    XOR = 0x1D
    SLL = 0x1E
    SLR = 0x1F

  Flag* = enum
    greater = 0b00000001
    equal = 0b00000010
    lesser = 0b00000100

  BFormat* = enum
    binary
    decimal
    hexadecimal

  EnumSetType = u16

# They say to use least powerful feature that fits purpose
# So they became funcs
func bitlen[T: SomeInteger](t: typedesc[T]): int = 8*sizeof(t)
func ones[T: SomeInteger](n: T): T = T(2)^n-T(1)

# Explicit hack for checking if value is in enum, because
# more advanced method (with global pragma) generated
# compiler error
func enumset[E: enum](t: typedesc[E]): set[EnumSetType] =
  var elements: set[EnumSetType] = {}
  for i in items(t): elements[EnumSetType(i)] = true
  return elements


# CONSTANTS


const
  iRegisterValues = enumset(InternalRegister)
  eRegisterValues = enumset(ExternalRegister)

  dumpPause = repeat("=", 60)
  defaultBFormat = hexadecimal

  opcodeBits = OpcodeIndex(5)
  opcodeOnes = ones(opcodeBits)
  opcodeLen = OpcodeIndex(bitlen(OpcodeIndex))

  iFormatShift = opcodeLen-opcodeBits
  iFormatOpcodeMask = opcodeOnes shl iFormatShift
  rFormatOpcodeMask = opcodeOnes

  argBits = OpcodeIndex(3)
  argOnes = ones(argBits)

  arg1Shift = iFormatShift - argBits
  arg1Mask = argOnes shl arg1Shift
  arg2Shift = OpcodeIndex(5)
  arg2Mask = argOnes shl arg2Shift

  conditionCodeMask = block:
    var mask: u16
    for e in Flag: mask += u16(e)
    mask

  mulInstrFlag = RegisterValue(1)
  imm4Mask = ones(RegisterValue(4))
  msb = u16(1) shl u16(8*sizeof(u16)-1)

# HELPERS


# Converters are questionable part of language
# It is probably better to not use them too much or
# even at all
# But that realy takes much pain out
# and at least that works

converter toU16(c: Opcode): u16 = u16(c)
converter toU16(f: Flag): u16 = u16(f)
converter toAddr(i: int): MemAddr = MemAddr(i)

# Converter doesn't work for that case
func `[]`[I: range, R: Registers](
  a: var array[I, RegisterValue], r: R): var RegisterValue =
  a[int(r)]

# For that too
func `[]=`[I: range, R: Registers, V: RegisterValue](
  a: var array[I, V], r: R, v: V) =
  a[int(r)] = v

# At least that works well
# But nimsuggest doesn't see that :(, so used pragma was used
iterator items[R: range](r: typedesc[R]): R =
  for i in low(R)..high(R):
    yield i

proc intRegister(val: u8|u16): Option[InternalRegister] =
  if EnumSetType(val) in iRegisterValues:
    return some(InternalRegister(val))
  return none(InternalRegister)

proc extRegister(val: u8|u16): Option[ExternalRegister] =
  if EnumSetType(val) in eRegisterValues:
    return some(ExternalRegister(val))
  return none(ExternalRegister)

# No idea how reliable this is
func parts(value: u16): (u8, u8) =
  const mask8 = high(u8)
  let
    b1 = MemVal((value shr 8) and mask8)
    b2 = MemVal(value and mask8)
  when cpuEndian == littleEndian:
    let (highByte, lowByte) = (b1, b2)
  else:
    let (highByte, lowByte) = (b2, b1)
  return (highByte, lowByte)

func flag(value: u16): Flag =
  if value == 0: return equal
  elif (value and msb) != 0: return lesser
  return greater


# STATE MANIPULATION


func getMem(memory: Memory, address: MemAddr): MemVal {.inline.} =
  memory[address]
func getMem*(machine: Machine, address: MemAddr): MemVal {.inline.} =
  getMem(machine.memory, address)

# Hopefully it will just work
# Fucking endianness
func getMem(memory: Memory,
  address: MemAddr, container: var u16) {.inline.} =
  container = getMem(memory, address)
  container = container shl 8
  container += getMem(memory, address+1)

func getMem*(machine: Machine,
  address: MemAddr, container: var u16) {.inline.} =
  getMem(machine.memory, address, container)

func setMem*(machine: var Machine, address: MemAddr, value: MemVal) {.inline.} =
  machine.memory[address] = value

# I hope that correctness of that depends only on correctness of parts function
func setMem*(machine: var Machine, address: MemAddr, value: u16) {.inline.} =
  let (highByte, lowByte) = parts(value)
  setMem(machine, address+1, lowByte)
  setMem(machine, address, highByte)

# This separately becuase why not
func fetch(memory: Memory, ip: RegisterValue): Instruction =
  var instruction: Instruction
  getMem(memory, ip shl 1, instruction)
  return instruction

func fetch*(machine: Machine): Instruction =
  # Overloading with var type doesn't help, converter cant go here,
  # so only explicit cast left
  return fetch(machine.memory, machine.eregisters[int(IP)])


# WRITING


func dumpValue*(value: SomeInteger, format: BFormat, specifier: bool): string =
  var dumped = ""
  case format:
  of binary:
    if specifier: dumped &= "0b"
    dumped &= toBin(BiggestInt(value), 8*sizeof(value))
  of decimal: dumped = $value
  of hexadecimal:
    if specifier: dumped &= "0x"
    dumped &= toHex(value) # TODO C toHex warning
  return dumped

func dumpValue*(value: SomeInteger, format: BFormat): string {.inline.} =
  return dumpValue(value, format, false)
func dumpValue*(value: SomeInteger): string {.inline.} =
  return dumpValue(value, defaultBFormat, false)

func dumpERegisters*(machine: Machine, format: BFormat): string {.inline.} =
  var dumped = ""
  for r in items(ExternalRegister):
    dumped &= $ r
    dumped &= " "
    dumped &= dumpValue(machine.eregisters[int(r)], format, true)
    dumped &= "\n"
  return dumped

func dumpIRegisters*(machine: Machine, format: BFormat): string {.inline.} =
  var dumped = ""
  for r in items(InternalRegister):
    dumped &= $ r
    dumped &= " "
    dumped &= dumpValue(machine.iregisters[int(r)], format, true)
    dumped &= "\n"
  return dumped

# The most intelligent design ever
func dumpERegisters*(machine: Machine): string {.inline.} =
  return dumpERegisters(machine, defaultBFormat)
func dumpIRegisters*(machine: Machine): string {.inline.} =
  return dumpIRegisters(machine, defaultBFormat)

proc printRegisters*(machine: Machine, format: BFormat) {.inline.} =
  echo dumpIRegisters(machine, format)
  echo dumpERegisters(machine, format)
proc printRegisters*(machine: Machine) {.inline.} =
  printRegisters(machine, defaultBFormat)

proc printMemory*(
  machine: Machine, first, last: MemAddr, format: BFormat, blocks: bool) =
  let
    memory = machine.memory
    last = int(last)
  var
    address = int(first)
    line: string
    i: int

  if blocks:
    while address <= last:
      i = 0
      line = ""

      while i < 8 and address <= last:
        line &= dumpValue(memory[address], format)
        address += 1
        i += 1
        line &= ' '
      echo line

  else:
    for a in first..last:
      echo(dumpValue(address) & ": " & dumpValue(memory[address], format))

proc printMemory*(
  machine: Machine, first, last: MemAddr, blocks: bool) {.inline.} =
  printMemory(machine, first, last, defaultBFormat, blocks)

proc printMemory*(
  machine: Machine, first, last: MemAddr, format: BFormat) {.inline.} =
  printMemory(machine, first, last, format, true)

proc printMemory*(machine: Machine, first, last: MemAddr) {.inline.} =
  printMemory(machine, first, last, defaultBFormat, true)


# READING


# TODO D instruction checker at reading
# Don't know if this could be simpler, fucking endianness
# Also don't know how correct this is
func readOne(instruction: string): Instruction {.inline.} =
  var read: Instruction
  discard parseHex(instruction, read)
  let (highByte, lowByte) = parts(read)
  return u16(highByte) shl 8 + u16(lowByte)

proc read*(instructions: string | File): Instructions {.inline.} =
  var read: Instructions
  for i in lines(instructions):
    add(read, readOne(i))
  return read

proc initMachine*(
  machine: var Machine, instructions: Instructions) =
  var address = MemAddr(0)

  # TODO D cpu flags
  # Because i can simulate MUL
  machine.eregisters[CF] = mulInstrFlag

  # One must be set by default
  machine.eregisters[FL] = equal

  for i in instructions:
    setMem(machine, address, i)
    address += 2

proc initMachine*(instructions: Instructions): Machine =
  var machine: Machine
  initMachine(machine, instructions)
  return machine

proc initMachine*(
  machine: var Machine, code: string | File) {.inline.} =
  return initMachine(machine, read(code))

proc initMachine*(code: string | File): Machine {.inline.} =
  return initMachine(read(code))

proc resetMachine*(machine: var Machine, startAddr, endAddr: MemAddr) =
  for i in startAddr..endAddr: machine.memory[i] = MemVal(0)
  for r in machine.eregisters: machine.eregisters[r] = RegisterValue(0)
  for r in machine.iregisters: machine.iregisters[r] = RegisterValue(0)

proc resetMachine*(machine: var Machine, startAddr: MemAddr) =
  resetMachine(machine, startAddr, MemAddr(len(machine.memory)-1))

proc resetMachine*(machine: var Machine) =
  resetMachine(machine, MemAddr(0))

# TODO B test
proc resetMachine*(machine: var Machine, instructions: Instructions) =
  initMachine(machine, instructions)
  resetMachine(machine, MemAddr(len(instructions)))


# EXECUTION


# Most probably sign of bad design, but why not
# Most important is that this works well
template getRegister(option, register: untyped, msg: string) =
  if not isSome(option):
    return MachineInfo(status: exceptional, info: msg)
  let register = get(option)

#template getRegister(option, register: untyped) =
#  getRegister(option, register,
#    "Bad register in " & $ Opcode(opcode) & " instruction")

# return value tells if machine stopped
proc executeOne*(machine: var Machine, printInstruction = false, format = hexadecimal): MachineInfo =
  # TODO C may be better to keep machine state inside of object
  # And check it here

  machine.eregisters[IP] += 1
  let
    # TODO D may be good to split instruction using parts funtion
    instruction = fetch(machine.memory, machine.eregisters[IP]-1)
    ui = machine.eregisters[UI]

    # TODO D Maybe use u8 for opcodes
    ropcode = instruction and rFormatOpcodeMask
    iopcode = (instruction and iFormatOpcodeMask) shr iFormatShift
    opcode = if iopcode == 0: ropcode else: iopcode

    arg1 = (instruction and arg1Mask) shr arg1Shift
    arg2 = (instruction and arg2Mask) shr arg2Shift
    imm16 = (machine.eregisters[UI] shl 8) or u8(instruction)

  if printInstruction:
    # echo dumpValue(instruction, format, true)
    if iopcode == 0:
      echo "Opcode: " & $Opcode(opcode) & " " &
        $dumpValue(arg1, format, true) & " " &
        $dumpValue(arg2, format, true)
    else:
      echo "Opcode: " & $Opcode(opcode) & " " &
        $dumpValue(arg1, format, true) & " " &
        $dumpValue(imm16, format, true)

  block execution:
    case opcode:
    of NOP: break execution
    of HLT:
      when defined conformant:
        if (arg1 and machine.eregisters[FL]) != 0:
          return MachineInfo(status: halted)
        break execution
      else:
        return MachineInfo(status: halted)

    of RET:
      if (arg1 and machine.eregisters[FL]) != 0:
        machine.eregisters[IP] = machine.eregisters[LR]
      break execution

    of CAL, JMP:
      if (arg1 and machine.eregisters[FL]) == 0:
        break execution

      let v2 = block:
        if iopcode == 0:
          let reg1opt = intRegister(arg1)
          getRegister(reg1opt, reg1,
            "Bad first (internal) register in " &
            $Opcode(opcode) & " instruction")
          machine.iregisters[reg1]
        else: imm16

      if opcode == CAL: machine.eregisters[LR] = machine.eregisters[IP]
      machine.eregisters[IP] = v2
      break execution

    else: discard

    let reg1opt = intRegister(arg1)
    getRegister(reg1opt, reg1, "Bad first (internal register)")

    case opcode:
    of PSH:
      machine.eregisters[SP] -= 2
      setMem(machine, machine.eregisters[SP], machine.iregisters[reg1])
      break execution
    of POP:
      getMem(machine.memory, machine.eregisters[SP], machine.iregisters[reg1])
      machine.eregisters[SP] += 2
      break execution
    of RDX:
      let reg2opt = extRegister(arg2)
      getRegister(reg2opt, reg2,
        "Bad second (external) register in" &
        $Opcode(opcode) & " instruction")
      machine.iregisters[reg1] = machine.eregisters[reg2]
      break execution

    of WRX:
      let reg1opt = extRegister(arg1)
      getRegister(reg1opt, reg1,
        "Bad first (external) register in " &
        $Opcode(opcode) & " instruction")
      if iopcode == 0:
        let reg2opt = intRegister(arg2)
        getRegister(reg2opt, reg2,
          "Bad second (internal) register in " &
          $Opcode(opcode) & " instruction")
        machine.eregisters[reg1] = machine.iregisters[reg2]
      else:
        machine.eregisters[reg1] = imm16
      break execution

    else: discard

    let value = block:
      if iopcode == 0:
        let reg2opt = intRegister(arg2)
        getRegister(reg2opt, reg2,
          "Bad second (internal) register in " &
          $Opcode(opcode) & " instruction")
        machine.iregisters[reg2]
      else: imm16

    case opcode: # Those have two possible formats

    of MOV:
      machine.iregisters[reg1] = value
    of RDM:
      machine.iregisters[reg1] = getMem(machine.memory, value)
    of WRM:
      setMem(machine, value, machine.iregisters[reg1])

    of MUL:
      machine.iregisters[reg1] = u16(value) * u16(machine.iregisters[reg1])
      machine.eregisters[FL] = flag(machine.iregisters[reg1])
    of CMP:
      machine.eregisters[FL] = flag(machine.iregisters[reg1] - value)
    of TST:
      machine.eregisters[FL] = flag(machine.iregisters[reg1] and value)

    of ADD:
      machine.iregisters[reg1] = machine.iregisters[reg1] + value
      machine.eregisters[FL] = flag(machine.iregisters[reg1])
    of SUB:
      machine.iregisters[reg1] = machine.iregisters[reg1] - value
      machine.eregisters[FL] = flag(machine.iregisters[reg1])

    of NOT:
      machine.iregisters[reg1] = not value
      machine.eregisters[FL] = flag(machine.iregisters[reg1])
    of AND:
      machine.iregisters[reg1] = machine.iregisters[reg1] and value
      machine.eregisters[FL] = flag(machine.iregisters[reg1])
    of ORR:
      machine.iregisters[reg1] = machine.iregisters[reg1] or value
      machine.eregisters[FL] = flag(machine.iregisters[reg1])
    of XOR:
      machine.iregisters[reg1] = machine.iregisters[reg1] xor value
      machine.eregisters[FL] = flag(machine.iregisters[reg1])

    of SLL:
      machine.iregisters[reg1] =
        machine.iregisters[reg1] shl (value and imm4Mask)
      machine.eregisters[FL] = flag(machine.iregisters[reg1])
    of SLR:
      machine.iregisters[reg1] =
        machine.iregisters[reg1] shr (value and imm4Mask)
      machine.eregisters[FL] = flag(machine.iregisters[reg1])

    else:
      return MachineInfo(status: exceptional,
        info: "Invalid instruction")

  if ui == machine.eregisters[UI]: machine.eregisters[UI] = 0
  return MachineInfo(status: ready)

proc executeOne*(machine: var Machine): MachineInfo =
  return executeOne(machine, false)

proc execute*(machine: var Machine, stopOnInstruction = false,
  printRegisterSteps = false, printMemorySteps = false,
  printInstruction = false, instructionFormat = hexadecimal,
  format = hexadecimal): MachineInfo =
  var info = MachineInfo(status: ready)

  while true:
    info = executeOne(machine, printInstruction, instructionFormat)

    if info.status != ready:
      return info

    if printRegisterSteps or printMemorySteps:
      echo dumpPause
      echo ""

    if printRegisterSteps:
      printRegisters(machine, format)
      if printMemorySteps:
        echo ""

    if printMemorySteps:
      discard #TODO B print memory steps

    if stopOnInstruction:
      discard readline(stdin)

  return info

proc execute*(code: string | File, stopOnInstruction = false,
  printRegisterSteps = false, printMemorySteps = false,
  printInstruction = false, format = hexadecimal): MachineInfo =
  var machine = initMachine(code)
  return execute(machine, stopOnInstruction,
    printRegisters, printMemorySteps, printInstruction)


# MAIN


when isMainModule:
  var
    file = stdin
    printMsgs = true

    stopOnInstruction = false
    printInstruction = false
    doPrintRegisters = true
    doPrintMemory = false
    printRegisterSteps = false
    printMemorySteps = false
    format = decimal

  # TODO B parseopt
  for c in commandLineParams():
    case c:
    of "-h": format = hexadecimal
    of "-b": format = binary

    of "-r": printRegisterSteps = true
    of "-m": printMemorySteps = true

    of "-d": doPrintRegisters = false
    of "-v": doPrintMemory = true

    of "-i": printInstruction = true
    of "-s": stopOnInstruction = true

    of "-t":
      printMsgs = false
      stopOnInstruction = false
      printInstruction = false
      doPrintRegisters = true
      doPrintMemory = false
      printRegisterSteps = false
      printMemorySteps = false
      format = decimal

    else:
      if fileExists(c):
        file = open(c)
      else:
        if printMsgs:
          echo "Undefined option or unexistent file: " & c
          echo "Using stdin"

  if file == stdin:
    if printMsgs:
      echo "Warning: can't get user input when reading from stdin"
      echo ""
    stopOnInstruction = false

  var machine = initMachine(file)
  let info = execute(machine, stopOnInstruction,
    printRegisterSteps, printMemorySteps, printInstruction, format, format)

  if (doPrintRegisters or doPrintMemory) and printMsgs:
    echo dumpPause
    echo ""

  if printMsgs:
    case info.status:
      of halted: echo "Machine halted on HLT"
      of exceptional:
        echo "Machine halted exceptionally, info:"
        echo info.info
      of ready: echo "Impossible !!"

  if doPrintRegisters:
    if printMsgs: echo ""
    printRegisters(machine, format)

  if doPrintMemory:
    discard #TODO A print fucking memory

  close file
