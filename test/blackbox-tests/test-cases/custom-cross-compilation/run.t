  $ dune build --root ./normal --display short file @install
  Entering directory 'normal'
      ocamldep .p.eobjs/p.ml.d [cross]
        ocamlc .p.eobjs/byte/p.{cmi,cmo,cmt} [cross]
      ocamlopt .p.eobjs/native/p.{cmx,o} [cross]
      ocamlopt p.exe [cross]
      ocamldep .p.eobjs/p.ml.d
        ocamlc .p.eobjs/byte/p.{cmi,cmo,cmt}
      ocamlopt .p.eobjs/native/p.{cmx,o}
      ocamlopt p.exe
             p file [cross]
             p file

  $ cat normal/_build/cross/file
  137

  $ dune build --root ./bad-configuration --display short file @install
  Entering directory 'bad-configuration'
  File "dune-workspace", line 3, characters 9-53:
  3 | (context (default
  4 |   (name cross-1)
  5 |   (host default)
  6 | ))
  Error: Context 'cross-1' is both a host (for 'cross-2') and a target (for 'default').
  [1]

  $ dune build --root ./topological-loop --display short file @install
  Entering directory 'topological-loop'
  File "dune-workspace", line 11, characters 9-53:
  11 | (context (default
  12 |   (name cross-3)
  13 |   (host cross-2)
  14 | ))
  Error: Context 'cross-3' is both a host (for 'cross-1') and a target (for 'cross-2').
  [1]
