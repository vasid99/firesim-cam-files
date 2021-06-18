// See LICENSE for license details.

#include "simif.h"

#include <stdlib.h>
#include <stdio.h>

struct CAMRTLTest_t: virtual simif_t
{
	int num_cycles = 100;
  CAMRTLTest_t(int argc, char** argv) {
  	if(argc>1){
  		num_cycles = atoi(argv[1]);
		}
	}

  void run() {
    target_reset();
    poke(io_in_en, 1);
    for (size_t cycle = 0; cycle < num_cycles; cycle++) {
      do_iteration(cycle);
    }
  }

private:

  size_t depth = 32, tagWidth = 10, dataWidth = 32;
  size_t tagSize = depth < (1<<tagWidth) ? depth : 1<<tagWidth;
	
  void do_iteration(size_t cycle_num) {
    data_t rand_tag = rand() % tagSize;
    data_t write_data = cycle_num;
    data_t rand_wr = rand() % 2;
    
    poke(io_in_tag, rand_tag);
    poke(io_in_wrData, write_data);
    poke(io_in_wr, rand_wr);
    
    step(1);
    if(!rand_wr) peek(io_out_rdData);
    peek(io_out_hit);
  }
};
