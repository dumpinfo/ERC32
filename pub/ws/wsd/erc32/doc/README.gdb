How to use SIS with GDB
-----------------------

1. Building GDB with SIS

To build GDB with the SIS/ERC32 simulator, configure with option
'--target sparc-erc32-aout' and build as usual.

2. Attaching the simulator

To attach GDB to the simulator, use:

target sim [options] [files]

The following options are supported:

 -nfp		Disable FPU. FPops will cause an FPU disabled trap.

 -freq <f>	Set the simulated "system clock" to <f> MHz.

 -v		Verbose mode.

 -sparclite	Enable SMUL and DIVSCC instuctions

 -nogdb		Disable GDB stack and breakpoint handling (see below)

The listed [files] are expected to be in aout format and will be
loaded in the simulator memory prior. This could be used to load
a boot block at address 0x0 if the application is linked to run
from RAM (0x2000000).

To start debugging a program type 'load <program>' and debug as
usual. 

The native simulator commands can be reached using the GDB 'sim'
command:

sim <sis_command>

Direct simulator commands during a GDB session must be issued
with care not to disturb GDB's operation ... 

For info on supported ERC32 functionality, see README.sis.


3. Loading aout files

The GDB load command loads an aout file into the simulator
memory with the data section starting directly after the text
section regardless of wich start address was specified for the data
at link time! This means that your applications either has to include
a routine that initialise the data segment at the proper address or
link with the data placed directly after the text section.

A copying routine is fairly simple, just copy all data between
_etext and _data to a memory loaction starting at _environ. This
should be done at the same time as the bss is cleared (in srt0.s).


4. GDB stack and breakpoint handling

GDB inserts breakpoint in the form of the 'ta 1' instruction. The
GDB-integrated simulator will therefore recognize the breakpoint
instruction and return control to GDB. If the application uses
'ta 1', the breakpoint detection can be disabled with the -nogdb
switch. In this case however, GDB breakpoints will not work.

Before control is left to GDB, all register windows are flushed out to the 
stack.  Starting after the invalid window, flush all windows up to, and 
including the current window. This allows GDB to do backtraces and look at 
local variables for frames that are still in the register windows.  Note that 
strictly speaking, this behavior is *wrong* for several reasons.  First, it 
doesn't use the window overflow handlers.  It therefore assumes standard 
frame layouts and window handling policies. Second, it changes system state 
behind the back of the target program. Typically, this will only create
problems when debugging trap handlers. The '-nogdb' switch disables the
register flushing.

Report problems to Jiri Gaisler ESA/ESTEC (jgais@wd.estec.esa.nl)
