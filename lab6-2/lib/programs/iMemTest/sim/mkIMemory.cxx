/*
 * Generated by Bluespec Compiler, version 2011.06.D (build 24470, 2011-06-30)
 * 
 * On Thu Jan  9 15:05:48 KST 2014
 * 
 */
#include "bluesim_primitives.h"
#include "mkIMemory.h"

namespace bluesim
{
  
  /* String declarations */
  static std::string const __str_literal_1("memory0.vmh", 11u);
  static std::string const __str_literal_2("memory1.vmh", 11u);
  
  
  /* Constructor */
  MOD_mkIMemory::MOD_mkIMemory(char const *name, Module *parent)
    : Module(name, parent),
      __clk_handle_0(BAD_CLOCK_HANDLE),
      INST_iMem_0("iMem_0", this, __str_literal_1, 26u, 8u, 0u, 67108863u, 0u),
      INST_iMem_1("iMem_1", this, __str_literal_2, 26u, 8u, 0u, 67108863u, 0u),
      PORT_RST_N((tUInt8)1u)
  {
    symbol_count = 2u;
    symbols = new tSym[symbol_count];
    init_symbols_0();
  }
  
  
  /* Symbol init fns */
  
  void MOD_mkIMemory::init_symbols_0()
  {
    init_symbol(&symbols[0u], "iMem_0", SYM_MODULE, &INST_iMem_0);
    init_symbol(&symbols[1u], "iMem_1", SYM_MODULE, &INST_iMem_1);
  }
  
  
  /* Rule actions */
  
  
  /* Methods */
  
  tUInt64 MOD_mkIMemory::METH_req(tUInt32 ARG_req_a)
  {
    tUInt64 PORT_req;
    tUInt32 DEF_iMem_0_sub_req_a_BITS_31_TO_1_PLUS_2_0_BITS_25_ETC___d22;
    tUInt64 DEF_iMem_0_sub_req_a_BITS_26_TO_1_0_CONCAT_iMem_1__ETC___d21;
    tUInt32 DEF_iMem_0_sub_req_a_BITS_31_TO_1_PLUS_1_BITS_25_T_ETC___d23;
    tUInt64 DEF_iMem_1_sub_req_a_BITS_26_TO_1_CONCAT_iMem_0_su_ETC___d19;
    tUInt8 DEF_req_a_BIT_0___d1;
    tUInt32 DEF_x__h770;
    tUInt32 DEF_x__h845;
    tUInt32 DEF_x__h920;
    tUInt32 DEF_idx__h429;
    tUInt8 DEF_x__h764;
    tUInt8 DEF_x__h664;
    tUInt8 DEF_x__h839;
    tUInt32 DEF_i__h913;
    tUInt8 DEF_x__h914;
    tUInt32 DEF_i__h763;
    tUInt8 DEF_x__h805;
    tUInt32 DEF_i__h729;
    tUInt8 DEF_x__h730;
    tUInt32 DEF_i__h838;
    tUInt8 DEF_x__h880;
    DEF_i__h729 = (tUInt32)(67108863u & (ARG_req_a >> 1u));
    DEF_x__h730 = INST_iMem_1.METH_sub(DEF_i__h729);
    DEF_x__h664 = INST_iMem_0.METH_sub(DEF_i__h729);
    DEF_idx__h429 = (tUInt32)(ARG_req_a >> 1u);
    DEF_x__h920 = 2147483647u & (DEF_idx__h429 + 3u);
    DEF_i__h913 = (tUInt32)(67108863u & DEF_x__h920);
    DEF_x__h914 = INST_iMem_0.METH_sub(DEF_i__h913);
    DEF_x__h845 = 2147483647u & (DEF_idx__h429 + 2u);
    DEF_i__h838 = (tUInt32)(67108863u & DEF_x__h845);
    DEF_x__h880 = INST_iMem_1.METH_sub(DEF_i__h838);
    DEF_x__h839 = INST_iMem_0.METH_sub(DEF_i__h838);
    DEF_x__h770 = 2147483647u & (DEF_idx__h429 + 1u);
    DEF_i__h763 = (tUInt32)(67108863u & DEF_x__h770);
    DEF_x__h805 = INST_iMem_1.METH_sub(DEF_i__h763);
    DEF_x__h764 = INST_iMem_0.METH_sub(DEF_i__h763);
    DEF_req_a_BIT_0___d1 = (tUInt8)((tUInt8)1u & ARG_req_a);
    DEF_iMem_0_sub_req_a_BITS_31_TO_1_PLUS_1_BITS_25_T_ETC___d23 = 65535u & ((((tUInt32)(DEF_x__h764)) << 8u) | (tUInt32)(DEF_x__h805));
    DEF_iMem_0_sub_req_a_BITS_31_TO_1_PLUS_2_0_BITS_25_ETC___d22 = 65535u & ((((tUInt32)(DEF_x__h839)) << 8u) | (tUInt32)(DEF_x__h880));
    DEF_iMem_1_sub_req_a_BITS_26_TO_1_CONCAT_iMem_0_su_ETC___d19 = 281474976710655llu & ((((tUInt64)(16777215u & ((((tUInt32)(DEF_x__h730)) << 16u) | DEF_iMem_0_sub_req_a_BITS_31_TO_1_PLUS_1_BITS_25_T_ETC___d23))) << 24u) | (tUInt64)(16777215u & ((DEF_iMem_0_sub_req_a_BITS_31_TO_1_PLUS_2_0_BITS_25_ETC___d22 << 8u) | (tUInt32)(DEF_x__h914))));
    DEF_iMem_0_sub_req_a_BITS_26_TO_1_0_CONCAT_iMem_1__ETC___d21 = 281474976710655llu & ((((tUInt64)(65535u & ((((tUInt32)(DEF_x__h664)) << 8u) | (tUInt32)(DEF_x__h730)))) << 32u) | (tUInt64)((DEF_iMem_0_sub_req_a_BITS_31_TO_1_PLUS_1_BITS_25_T_ETC___d23 << 16u) | DEF_iMem_0_sub_req_a_BITS_31_TO_1_PLUS_2_0_BITS_25_ETC___d22));
    PORT_req = DEF_req_a_BIT_0___d1 ? DEF_iMem_1_sub_req_a_BITS_26_TO_1_CONCAT_iMem_0_su_ETC___d19 : DEF_iMem_0_sub_req_a_BITS_26_TO_1_0_CONCAT_iMem_1__ETC___d21;
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
    __clk_handle_0 = bk_get_or_define_clock(s);
  }
  
  
  /* State dumping routine */
  void MOD_mkIMemory::dump_state(unsigned int indent)
  {
    printf("%*s%s:\n", indent, "", inst_name);
    INST_iMem_0.dump_state(indent + 2u);
    INST_iMem_1.dump_state(indent + 2u);
  }
  
  
  /* VCD dumping routines */
  
  unsigned int MOD_mkIMemory::dump_VCD_defs(unsigned int levels)
  {
    fprintf(vcd_file, "$scope module %s $end\n", inst_name);
    vcd_num = vcd_reserve_ids(3u);
    unsigned int num = vcd_num;
    for (unsigned int hdl = 0u; hdl < bk_num_clocks(); ++hdl)
      vcd_add_clock_def(this, bk_clock_name(hdl), bk_clock_vcd_num(hdl));
    vcd_write_def(bk_clock_vcd_num(__clk_handle_0), "CLK", 1u);
    vcd_write_def(num++, "RST_N", 1u);
    num = INST_iMem_0.dump_VCD_defs(num);
    num = INST_iMem_1.dump_VCD_defs(num);
    fprintf(vcd_file, "$upscope $end\n");
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
      vcd_write_x(num++, 1u);
    }
    else
      if (dt == VCD_DUMP_CHANGES)
      {
	if ((backing.PORT_RST_N) != PORT_RST_N)
	{
	  vcd_write_val(num, PORT_RST_N, 1u);
	  backing.PORT_RST_N = PORT_RST_N;
	}
	++num;
      }
      else
      {
	vcd_write_val(num++, PORT_RST_N, 1u);
	backing.PORT_RST_N = PORT_RST_N;
      }
  }
  
  void MOD_mkIMemory::vcd_prims(tVCDDumpType dt, MOD_mkIMemory &backing)
  {
    INST_iMem_0.dump_VCD(dt, backing.INST_iMem_0);
    INST_iMem_1.dump_VCD(dt, backing.INST_iMem_1);
  }
}