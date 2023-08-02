# Package

version       = "0.8.0"
author        = "Rellikeht"
description   = "Simulator for sarvel's cpu"
license       = "MIT"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["simulator"]


# Dependencies

requires "nim >= 2.0.0"

# Tasks

## Helpful Definitions

import os, strutils

template todo(_: varargs[untyped]) {.used.} = discard
template mkIfExist(dir: string) =
  if not dirExists(dir): mkDir(dir)

func cppCmd(src, dest: string, flags = ""): string =
  "c++ -o " & dest & " " & flags & " " & src

const
  codeDir = "16bit_cpu"
  binDir = "orig"
  progDir = "programs"

  includes = "-include cstdint"
  origFlags = includes & " -std=c++20"

## Preparing and cleaning

task cleanOrig, "Clean build artifacts after building original files":
  rmFile(binDir / "asm")
  rmFile(binDir / "sim")

task cleanPrograms, "Clean assembled programs":
  todo ""

task cleanSimulator, "Clean simulator build artifacts":
  rmFile("simulator")

task totalClean, "Clean everything":
  cleanOrigTask()
  cleanProgramsTask()
  cleanSimulatorTask()

task prepare, "Prepare for building":
  mkIfExist("orig")
  mkIfExist("programs")

## Building

task compAsm, "Compile original assembler":
  prepareTask()
  exec cppCmd(codeDir / "assembly" / "assembler.cpp", binDir / "asm", origFlags)
  echo "Compiled original assembler"

task compSim, "Compile original simulator":
  prepareTask()
  exec cppCmd(codeDir / "simulator" / "simulator.cpp", binDir / "sim", origFlags)
  echo "Compiled original simulator"

task makePrograms, "Assembly programs":
  compAsmTask()

  const
    asmProg = binDir / "asm"
    asmSrc = codeDir / "assembly"

    outDir = codeDir / "assembly"
    outFile = "out.bin"
    debugFile = "debug.txt"
    outExt = ".bin"

  for file in listFiles(asmSrc):
    let parts = rsplit(file, '.', 1)
    if parts[1] == "asm":
       exec asmProg & " " & file
       let fname = rsplit(parts[0], DirSep, 1)[1]
       mvFile outFile, progDir / fname & outExt

    rmFile debugFile

  echo "Assembled programs"
