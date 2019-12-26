# microkorg-erl

an erlang module collection for reading and writing microkorg patches

## The **program** module

Currently, the **program** module is the main interface to all the provided functionalities. This is an overview of how it is organized:

### Reading and writing programs/patches directly to/from files:

- program:read_file(SysexFile) -> ProgramMap
- program:write_file(SysexFile, ProgramMap) -> ok
- program:write_file(ProgramMap) -> ok

the last function infers the filename from the program name

### Converting binaries to program maps, and viceversa:

- program:to_map(ProgramBinary) -> ProgramMap
- program:from_map(ProgramMap) -> ProgramBinary

### Generating random programs:

- program:random() -> NonVocoderProgramMap
- program:random_vocoder() -> VocoderProgramMap
- program:write_random(SysexFile) -> ok
- program:write_random() -> {ok, Filename}
- program:write_random_vocoder(SysexFile) -> ok
- program:write_random_vocoder() -> {ok, Filename}

### Merging two programs randomly:

- program:merge(ProgramMapA, ProgramMapB) -> ProgramMap

### Auxiliary functions

- program:name(ProgramMap) -> String
- program:mode(ProgramMap) -> single | double | vocoder
- program:set_name(ProgramMap, String) -> ProgramMap


### Overview

A program, in the microkorg documentantation, is the equivalent of a patch (like A11, A24, B12 etc). Sending a specific sysex command to the microkorg, makes it respond with the current program as sysex data. **program:read_file** transform a .syx encoded file into an explicit map (ProgramMap). The map can be re-encoded (as-is or modified) with **program:write_file**, that produces a .syx file.

The **program:write_random** function saves a .syx file with a random patch (trying to have reasonable values). The variant without parameters uses the randomly generated patch name (f.e. "budino.syx").

The function **program:merge** generates a child program from two parent programs, selecting each parameter at random from one of the two parents (like in genetic programs). Checks are in place so that all parameters are consistent. It could be used to alter slighly a pre-existing program, for example: **merge(A, merge(A, Random))** would result in a variant of the program A with 25% of the parameters changed.

## How to receive and send .syx patch files

note: these instructions are for linux, using the amidi utility

### Download the current patch

- connect the microkorg to pc with a midi interface
- find the hw port with **amidi -l** (let's say is hw:1)
- save the patch with **amidi -p hw:1 -S "F0 42 30 58 10 F7" -r programA11.syx**
- after a couple of seconds stop the application with Ctrl-C

### Upload a patch

- connect the microkorg to pc with a midi interface
- find the hw port with **amidi -l** (let's say is hw:1)
- send the patch with **amidi -p hw:1 -s programA11.syx**

note: the patch can be played immediately after uploading it, but is not permanently saved on the microkorg. Follow the standard procedure to save it permanently. When a patch is not saved, changing patch and going back restores the previous patch.

## Author

Giampaolo Guiducci <giampaolo.guiducci@gmail.com>

## License

microkorg-erl is licensed under the GPL v.3

## Todo

- TODO: -module(all_programs).
- TODO: -module(microkorg).
