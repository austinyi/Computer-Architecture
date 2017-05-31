/*
 * Generated by Bluespec Compiler, version 2014.07.A (build 34078, 2014-07-30)
 * 
 * On Thu May  4 14:26:44 KST 2017
 * 
 */
#include "bluesim_primitives.h"
#include "mkIMemory.h"


/* String declarations */
static std::string const __str_literal_1("memory.vmh", 10u);


/* Constructor */
MOD_mkIMemory::MOD_mkIMemory(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST_iMem(simHdl, "iMem", this, __str_literal_1, 26u, 32u, 0u, 67108863u, (tUInt8)0u),
    PORT_RST_N((tUInt8)1u)
{
  symbol_count = 1u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkIMemory::init_symbols_0()
{
  init_symbol(&symbols[0u], "iMem", SYM_MODULE, &INST_iMem);
}


/* Rule actions */


/* Methods */

tUInt64 MOD_mkIMemory::METH_req(tUInt32 ARG_req_a)
{
  tUInt64 DEF_iMem_sub_req_a_BITS_27_TO_2_CONCAT_iMem_sub_0__ETC___d11;
  tUInt32 DEF_idx__h238;
  tUInt64 DEF_iMem_sub_req_a_BITS_27_TO_2_BITS_23_TO_0_3_CON_ETC___d15;
  tUInt64 DEF_iMem_sub_req_a_BITS_27_TO_2_BITS_7_TO_0_9_CONC_ETC___d24;
  tUInt64 DEF_iMem_sub_req_a_BITS_27_TO_2_BITS_15_TO_0_7_CON_ETC___d18;
  tUInt8 DEF_offset__h239;
  tUInt32 DEF_x__h432;
  tUInt32 DEF_x__h451;
  tUInt32 DEF_i__h428;
  tUInt32 DEF_iMem_sub_0_CONCAT_req_a_BITS_31_TO_2_PLUS_1_BI_ETC___d9;
  tUInt32 DEF_i__h396;
  tUInt32 DEF_iMem_sub_req_a_BITS_27_TO_2___d4;
  tUInt32 DEF_i__h447;
  tUInt64 PORT_req;
  DEF_i__h396 = (tUInt32)(67108863u & (ARG_req_a >> 2u));
  DEF_iMem_sub_req_a_BITS_27_TO_2___d4 = INST_iMem.METH_sub(DEF_i__h396);
  DEF_offset__h239 = (tUInt8)((tUInt8)3u & ARG_req_a);
  DEF_idx__h238 = (((tUInt32)((tUInt8)0u)) << 30u) | (tUInt32)(ARG_req_a >> 2u);
  DEF_x__h451 = DEF_idx__h238 + 2u;
  DEF_i__h447 = (tUInt32)(67108863u & DEF_x__h451);
  DEF_x__h432 = DEF_idx__h238 + 1u;
  DEF_i__h428 = (tUInt32)(67108863u & DEF_x__h432);
  DEF_iMem_sub_0_CONCAT_req_a_BITS_31_TO_2_PLUS_1_BI_ETC___d9 = INST_iMem.METH_sub(DEF_i__h428);
  DEF_iMem_sub_req_a_BITS_27_TO_2_BITS_15_TO_0_7_CON_ETC___d18 = 281474976710655llu & ((((tUInt64)((tUInt32)(65535u & DEF_iMem_sub_req_a_BITS_27_TO_2___d4))) << 32u) | (tUInt64)(DEF_iMem_sub_0_CONCAT_req_a_BITS_31_TO_2_PLUS_1_BI_ETC___d9));
  DEF_iMem_sub_req_a_BITS_27_TO_2_BITS_7_TO_0_9_CONC_ETC___d24 = 281474976710655llu & (((((tUInt64)((tUInt8)((tUInt8)255u & DEF_iMem_sub_req_a_BITS_27_TO_2___d4))) << 40u) | (((tUInt64)(DEF_iMem_sub_0_CONCAT_req_a_BITS_31_TO_2_PLUS_1_BI_ETC___d9)) << 8u)) | (tUInt64)((tUInt8)(INST_iMem.METH_sub(DEF_i__h447) >> 24u)));
  DEF_iMem_sub_req_a_BITS_27_TO_2_BITS_23_TO_0_3_CON_ETC___d15 = 281474976710655llu & ((((tUInt64)((tUInt32)(16777215u & DEF_iMem_sub_req_a_BITS_27_TO_2___d4))) << 24u) | (tUInt64)((tUInt32)(DEF_iMem_sub_0_CONCAT_req_a_BITS_31_TO_2_PLUS_1_BI_ETC___d9 >> 8u)));
  DEF_iMem_sub_req_a_BITS_27_TO_2_CONCAT_iMem_sub_0__ETC___d11 = 281474976710655llu & ((((tUInt64)(DEF_iMem_sub_req_a_BITS_27_TO_2___d4)) << 16u) | (tUInt64)((tUInt32)(DEF_iMem_sub_0_CONCAT_req_a_BITS_31_TO_2_PLUS_1_BI_ETC___d9 >> 16u)));
  switch (DEF_offset__h239) {
  case (tUInt8)0u:
    PORT_req = DEF_iMem_sub_req_a_BITS_27_TO_2_CONCAT_iMem_sub_0__ETC___d11;
    break;
  case (tUInt8)1u:
    PORT_req = DEF_iMem_sub_req_a_BITS_27_TO_2_BITS_23_TO_0_3_CON_ETC___d15;
    break;
  case (tUInt8)2u:
    PORT_req = DEF_iMem_sub_req_a_BITS_27_TO_2_BITS_15_TO_0_7_CON_ETC___d18;
    break;
  default:
    PORT_req = DEF_iMem_sub_req_a_BITS_27_TO_2_BITS_7_TO_0_9_CONC_ETC___d24;
  }
  return PORT_req;
}

tUInt8 MOD_mkIMemory::METH_RDY_req()
{
  tUInt8 PORT_RDY_req;
  tUInt8 DEF_CAN_FIRE_req;
  DEF_CAN_FIRE_req = (tUInt8)1u;
  PORT_RDY_req = DEF_CAN_FIRE_req;
  return PORT_RDY_req;
}


/* Reset routines */

void MOD_mkIMemory::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkIMemory::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkIMemory::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST_iMem.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkIMemory::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 2u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  num = INST_iMem.dump_VCD_defs(num);
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkIMemory::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkIMemory &backing)
{
  vcd_defs(dt, backing);
  vcd_prims(dt, backing);
}

void MOD_mkIMemory::vcd_defs(tVCDDumpType dt, MOD_mkIMemory &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
  }
  else
    if (dt == VCD_DUMP_CHANGES)
    {
      if ((backing.PORT_RST_N) != PORT_RST_N)
      {
	vcd_write_val(sim_hdl, num, PORT_RST_N, 1u);
	backing.PORT_RST_N = PORT_RST_N;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, PORT_RST_N, 1u);
      backing.PORT_RST_N = PORT_RST_N;
    }
}

void MOD_mkIMemory::vcd_prims(tVCDDumpType dt, MOD_mkIMemory &backing)
{
  INST_iMem.dump_VCD(dt, backing.INST_iMem);
}
