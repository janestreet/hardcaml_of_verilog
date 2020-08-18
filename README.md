"Hardcaml of Verilog"
=====================

Use the opensource verilog synthesis tool [Yosys](http://www.clifford.at/yosys)
to read a synthesizable verilog design, convert it to a structural netlist
and save it in a JSON file.

This library can read the JSON netlist file and reconstruct the design in Hardcaml.

### Usage

The library can be used to convert and load JSON netlists. The yosys
tool must be in the PATH.

A tool is also provided which can be used to generate OCaml code to
wrap a Verilog design and load it at runtime (either by synthesis with
yosys, or from a pre-generated JSON file).

### Compatibility

The library has been tested mainly with yosys version 0.6 and 0.9.
Some testing was also done with 0.8. No compatibility issues have been
found between versions so far.

### Conversion status

Hardcaml does not support tri-state buffers in general.  Circuits
with tri-states will not work.

A few simlib primitives are not supported in the techlib.  These
either wont work in Hardcaml (ie latches) or have yet to be implemented.
In these cases a blackbox module is generated (the implementation of
which can be taken from the yosys simlib).

```
|Status              | Modules                                                             |
|--------------------|---------------------------------------------------------------------|
| to do              | shiftx, fsm, macc, alu                                              |
| bbox only          | sr, dlatch, dlatchsr                                                |
| no support planned | tribuf, div, mod, pow, memwr, memrd, meminit, assert, assume, equiv |
```

### Memories

Yosys can represent memories in a variety of ways

1. Synthesized into technology primitives (ie Xilinx block RAM)  `Supported by black boxes`
2. Converted to registers and muxes `fully supported`
3. As a $mem cell `supported with some limitations`
4. As a combination of $memwr, $memrd and $meminit cells `not supported`

The 2nd option is quite general and should be usable in most cases.  That said the
netlist will now implement all memories as registers so the design - as Hardcaml sees it -
may not be very efficient.  Uses the following command in yosys.

```
yosys> memory -dff
```

The third option will attempt to keep memories, but implement them using Hardcaml
memory primitives.  Hardcaml only supports memories with one read and one
write port whereas in general we need to support multi-port memories with

* N read ports
* M write ports
* Each write port may be in a different clock domain
* Each read port may be in a different clock domain
* Each read port may be synchronous or asynchronous
* Each read port may be read-before-write or write-before-read (also called fallthrough).

To support yosys we use a construction called a [LVT multi-port memory](http://fpgacpu.ca/multiport)
which builds more general memory structures from simpler single port memories.  The following
limitations are known

1. only supports 1 write clock domain
2. read-before-write and write-before-read behaviour only really makes sense if the read and
   write clocks are in the same clock domain.
3. Memory initialisation is not supported.

In yosys use;

```
yosys> memory -nomap; opt; clean
```

### Example

#### Yosys usage

A simple design with a single module may be converted with;

```
yosys> read_verilog design.v;     # load design
yosys> hierarchy; proc; flatten;  # structural conversion
yosys> write_json design.json     # write json netlist
```

In larger designs with multiple modules and/or memories this might be extended to;

```
yosys> read_verilog design.v       # load design
yosys> hierarchy -top <top_module> # select top level module
yosys> proc; flatten               # structural conversion
yosys> memory -nomap               # convert memories
yosys> opt -mux_undef; clean       # tidy up netlist
yosys> write_json design.json      # write json netlist
```

#### Hardcaml usage

```ocaml
let load_json_and_save_rtl ~black_box ~keep_names ~vhdl ~core_name ~json_file =
  (* read the json netlist *)
  let fns =
    Synthesized_designs.of_json_netlist_exn
      ~allow_blackboxes:black_box
      ~use_netlist_names:keep_names
      ~techlib:Techlib.Simlib.cells
      (Json_netlist.from_file json_file)
  in
  (* find the required design *)
  let design = Map.find_exn fns core_name in
  (* build the design *)
  let inputs =
    List.map (Synthesized_design.inputs design) ~f:(fun (n, b) -> n, Signal.input n b)
  in
  let outputs = Synthesized_design.create_fn design inputs in
  (* write back to vhdl or verilog *)
  let circ =
    Circuit.create_exn
      ~name:(Synthesized_design.name design)
      (List.map outputs ~f:(fun (n, s) -> Signal.output n s))
  in
  if vhdl then Rtl.print Vhdl circ else Rtl.print Verilog circ
;;
```

#### Yosys build notes

You can add a Makefile.conf to configure the build.  Set

```
CONFIG=gcc
ENABLE_TCL=0
```

The make all and install processes can both take a prefix so you can
install manually

```
make PREFIX=/final/localtion
make install PREFIX=/local/dir
do_my_install
```
