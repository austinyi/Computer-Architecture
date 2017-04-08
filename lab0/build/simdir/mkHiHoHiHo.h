/*
 * Generated by Bluespec Compiler, version 2014.07.A (build 34078, 2014-07-30)
 * 
 * On Fri Mar 17 12:04:22 KST 2017
 * 
 */

/* Generation options: */
#ifndef __mkHiHoHiHo_h__
#define __mkHiHoHiHo_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkHiHoHiHo module */
class MOD_mkHiHoHiHo : public Module {
 
 /* Clock handles */
 private:
  tClock __clk_handle_0;
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
  MOD_Reg<tUInt8> INST_counter;
 
 /* Constructor */
 public:
  MOD_mkHiHoHiHo(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_x__h148;
 
 /* Local definitions */
 private:
 
 /* Rules */
 public:
  void RL_inc_counter();
  void RL_say_hi();
  void RL_say_ho();
  void RL_finish();
 
 /* Methods */
 public:
 
 /* Reset routines */
 public:
  void reset_RST_N(tUInt8 ARG_rst_in);
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
  void set_clk_0(char const *s);
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkHiHoHiHo &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkHiHoHiHo &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkHiHoHiHo &backing);
};

#endif /* ifndef __mkHiHoHiHo_h__ */
