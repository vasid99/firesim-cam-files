# CAM Source Code For FireSim
This repository contains the source code for files that allow [FireSim][FireSim] to simulate content-addressable memories (CAMs) more efficiently on FPGAs by adding functionality to its Golden Gate compiler phase, enabling it to replace the registers used to store the tag values with RAMs. There are two kinds of files with respect to the main FireSim project - files that were not already present and have been added (such as HDL source code for generic CAM constructs, denoted by **(C)**) and files present in the original project edited to accommodate the former into the original project (such as the MIDAS Transform, denoted by **(M)**).

# Files
Here is a summary of the files in the repository and a brief explanation of the function served by them:
1. HDL
    - `sim/midas/src/main/scala/midas/models/cam/CAM.scala` **(C)** - HDL code for cycle-exact RTL and decoupled model written in Chisel3

2. Annotations for Transforms
    - `sim/midas/src/main/scala/midas/passes/fame/Annotations.scala` **(M)** - Internal annotations for labelling of CAM ports
    - `sim/midas/targetutils/src/main/scala/midas/annotations.scala` **(M)** - User-facing annotations for labelling of CAM instances in HDL written by user to be converted from cycle-exact to decoupled

3. Compilation to Verilog
    - `sim/midas/src/main/scala/midas/passes/fame/CAMTransforms.scala` **(C)** - FIRRTL transforms that replace the cycle-exact RTL with the decoupled model, making the appropriate connections in the process
    - `sim/midas/src/main/scala/midas/passes/MidasTransforms.scala` **(M)** - The entire sequence of decoupling transforms performed by FireSim's compiler to which the newly created transforms were added

4. Simulation
    - `sim/src/main/scala/midasexamples/CAMTest.scala` **(C)** - Testbench for generating a simple design with both cycle-exact and decoupled CAMs for simulation
    - `sim/src/main/cc/midasexamples/CAMRTLTest.h` **(C)** - Functionality to be executed by testbench simulating a cycle-exact non-decoupled CAM
    - `sim/src/main/cc/midasexamples/CAMModelTest.h` **(C)** - Functionality to be executed by testbench simulating a decoupled CAM
    - `sim/src/main/cc/midasexamples/Driver.cc` **(M)** - Driver for C++ simulation using Verilator (internal, not user-facing)
    - `sim/src/main/cc/midasexamples/CustomDesigns.h` **(C)** - Headers for user-created C++ testbenches to be included with the rest of the simulation framework

# Usage
Since this addition to FireSim has not been officially approved by the maintainers of the project, it was thought to be risky to maintain a complete fork of the FireSim repository which would not follow the updates made to the main repository. Hence the way to use these additions would be to first obtain a working copy of FireSim, available [here][FireSim] and then add these files to its source tree, which can be done by simply copying the contents of this repository to the root folder of the FireSim project. Of course this still does not completely protect from future additions to the project, but is more appropriate considering the alternative.

More detailed code commenting and documentation will be available with further additions to this repository. Also, folders named `ver` contain older versions of code that either failed to give results or were suboptimal.

[FireSim]: https://github.com/firesim/firesim/
