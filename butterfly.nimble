# Package

version       = "0.0.1"
author        = "Joey Payne"
description   = "Macro to make all your code step throughable."
license       = "The Unlicense"

srcDir = "src"

# Requires custom Nim compiler patch
# in order to patch procs on the fly
requires "nim >= 0.18.1"

task test, "Run tests":
  exec "nim c -r tests/test.nim"

task testjs, "Run tests on Node.js":
  exec "nim js -d:nodejs -r tests/test.nim"
