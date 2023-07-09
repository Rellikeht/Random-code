# Package

version       = "0.8.0"
author        = "Rellikeht"
description   = "Simulator for sarvel's cpu"
license       = "MIT"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["simulator"]


# Dependencies

requires "nim >= 1.6.10"

# Tasks
# TODO B move all work from Makefile here

task testTask, "Task for seeing what this can do":
  echo "Hello test 123"
