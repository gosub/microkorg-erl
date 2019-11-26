## microkorg-erl

an erlang module collection for reading and writing microkorg patches

## How to use the **program** module

The **program** module has two functions for manipulating programs:

- program:read_file(SysexFile) -> ProgramMap
- program:write_file(SysexFile, ProgramMap) -> ok

And two function to generate random programs:

- program:write_random(SysexFile) -> ok
- program:write_random() -> {ok, Filename}
- program:merge(ProgramA, ProgramB) -> ProgramMap

A program, in the microkorg documentantation, is the equivalent of a patch (like A11, A24, B12 etc). Sending a specific sysex command to the microkorg, makes it respond with the current program as sysex data. **program:read_file** transform a .syx encoded file into an explicit map (ProgramMap). The map can be re-encoded (as-is or modified) with **program:write_file**, that produces a .syx file.

The **program:write_random** function saves a .syx file with a random patch (trying to have reasonable values). The variant without parameters uses the randomly generated patch name (f.e. "budino.syx").

The function **program:merge** generates a child program from two parent programs, selecting each parameter at random from one of the two parents (like in genetic programs). Checks are in place so that all parameters are consistent. It could be used to alter slighly a pre-existing program, for example: **merge(A, merge(A, Random))** would result in a variant of the program A with 25% of the parameters changed.

## Warning

the **program** module still doesn't neither encode/decode nor randomly generates vocoder patches.

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

Giampaolo Guiducci

## License

microkorg-erl is licensed under the GPL v.3

## Todo

- TODO: random patch generator
- TODO: complete program_decode:vocoder
- TODO: complete program_encode:vocoder
- TODO: patch mixer (mix two patches into one, randomly)
- TODO: -module(all_programs).
- TODO: -module(microkorg).
