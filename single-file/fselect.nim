#!/usr/bin/env -S nim c -d:script

import macros
import strutils

const asize = 10
var ntable {.compileTime.}: array[asize, string]

macro tabadd(num: int, fn: untyped): untyped =
  expectKind(fn, nnkProcDef)
  let name = toStrLit(fn[0])
  quote("@") do:
    static: ntable[@num] = @name
    @fn

macro tabgen(): untyped =
  var cs: seq[NimNode] = @[newIdentNode("fn")]
  for i, n in ntable:
    if len(n) > 0:
      add(cs):
        nnkOfBranch.newTree(
          newLit(i),
          nnkStmtList.newTree(
            nnkReturnStmt.newTree(
              nnkCall.newTree(
                newIdentNode(n),
                newIdentNode("a1"),
                newIdentNode("a2")))))

  add(cs):
    nnkElse.newTree(
      nnkStmtList.newTree(
        nnkCommand.newTree(
          newIdentNode("echo"),
          newLit("Nope"))))
  nnkCaseStmt.newTree(cs)

proc f1(a, b: int): int {.tabadd(1).} = a + b
proc f2(a, b: int): int {.tabadd(2).} = a - b
proc f(fn, a1, a2: int): int = tabgen()

var num = 0
while num >= 0:
  num = parseInt(readline(stdin))
  echo f(num, 2, 3)
