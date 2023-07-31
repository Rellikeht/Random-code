type Dummy = object
  thunks: seq[proc()]

proc `=destroy`(d: var Dummy) =
  for i in countdown(high(d.thunks), low(d.thunks)): d.thunks[i]()

template defr(code: untyped): untyped =
  when not defined(EndOfScopeFunctions):
    var EndOfScopeFunctions = static Dummy(thunks: @[])
  EndOfScopeFunctions.thunks.add(proc () = code)

proc cdeftest() =
  echo "a"
  defr(echo "b")

  block:
    defr:
      echo "end"
      echo "of"
      echo "block"
      echo ""

    block:
      defr(echo "")
      for i in 0..10:
        for j in 0..10:
          if i == j:
            stdout.write $i&" "
            defr(echo i*j)

  defr(echo "c")
  echo "d"

cdeftest()
