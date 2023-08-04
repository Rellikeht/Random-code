import os, osproc, strutils, parseutils

var
  code = 0
  dummy = ""

for f in walkFiles("programs/*"):
  let
    origOut = execCmdEx "orig" / "sim " & "n " & f
    myOut = execCmdEx "./simulator" & " -t " & f

  if origOut.exitCode == 0 and myOut.exitCode == 0:
    var origRegs, myRegs: array[8, uint]
    let
      origLns = splitLines(origOut.output)
      myLns = splitLines(myOut.output)

    for i in 0..<8:
      let
        n1 = parseUntil(origLns[i], dummy, ' ') + 1
        n2 = parseUntil(origLns[i], dummy, ' ', n1) + 1
      origRegs[i] = parseUInt(origLns[i][(n1+n2)..^1])

    for i in 0..<8:
      let nstart = parseUntil(myLns[i], dummy, ' ') + 1
      myRegs[i] = parseUInt(myLns[i][nstart..^1])

    if origRegs != myRegs:
      echo repeat("=", 60)
      echo "Tests failed for file: " & rsplit(f, '/', 1)[1]
      echo ""

      echo "Original simulator registers state:"
      echo origRegs
      echo ""
      echo "Simulator registers state:"
      echo myRegs

      echo repeat("=", 60)
      echo ""
      code += 1

  else:
    echo f & " has problems and won't be used for tests"
    echo ""

quit code
