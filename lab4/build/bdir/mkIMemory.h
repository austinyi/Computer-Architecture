/*
 * Generated by Bluespec Compiler, version 2014.07.A (build 34078, 2014-07-30)
 * 
 * On Tue May  2 15:54:47 KST 2017
 * 
 */

/* Generation options: */
#ifndef __mkIMemory_h__
#define __mkIMemory_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkIMemory module */
class MOD_mkIMemory : public Module {
 
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
  MOD_RegFile<tUInt32,tUInt32> INST_iMem;
 
 /* Constructor */
 public:
  MOD_mkIMemory(tSimStateHdl simHdl, char const *name, Module *parent);
 
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
 
 /* Local definitions */
 private:
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  tUInt64 METH_req(tUInt32 ARG_req_a);
  tUInt8 METH_RDY_req();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkIMemory &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkIMemory &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkIMemory &backing);
};

#endif /* ifndef __mkIMemory_h__ */
