/*
 * Generated by Bluespec Compiler, version 2014.07.A (build 34078, 2014-07-30)
 * 
 * On Mon May 29 22:07:40 KST 2017
 * 
 */

/* Generation options: */
#ifndef __mkProc_h__
#define __mkProc_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"
#include "mkCop.h"
#include "mkDMemory.h"
#include "mkIMemory.h"
#include "mkBypassRFile.h"


/* Class declaration for the mkProc module */
class MOD_mkProc : public Module {
 
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
  MOD_Reg<tUInt8> INST_condFlag;
  MOD_mkCop INST_cop;
  MOD_Reg<tUWide> INST_d2e_data_0;
  MOD_Reg<tUInt8> INST_d2e_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_d2e_deqP_dummy2_1;
  MOD_Wire<tUInt8> INST_d2e_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_d2e_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_d2e_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_d2e_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_d2e_deqP_lat_0;
  MOD_Wire<tUInt8> INST_d2e_deqP_lat_1;
  MOD_Reg<tUInt8> INST_d2e_deqP_rl;
  MOD_Reg<tUInt8> INST_d2e_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_d2e_enqP_dummy2_1;
  MOD_Wire<tUInt8> INST_d2e_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_d2e_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_d2e_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_d2e_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_d2e_enqP_lat_0;
  MOD_Wire<tUInt8> INST_d2e_enqP_lat_1;
  MOD_Reg<tUInt8> INST_d2e_enqP_rl;
  MOD_mkDMemory INST_dMem;
  MOD_Reg<tUWide> INST_e2m_data_0;
  MOD_Reg<tUInt8> INST_e2m_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_e2m_deqP_dummy2_1;
  MOD_Wire<tUInt8> INST_e2m_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_e2m_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_e2m_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_e2m_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_e2m_deqP_lat_0;
  MOD_Wire<tUInt8> INST_e2m_deqP_lat_1;
  MOD_Reg<tUInt8> INST_e2m_deqP_rl;
  MOD_Reg<tUInt8> INST_e2m_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_e2m_enqP_dummy2_1;
  MOD_Wire<tUInt8> INST_e2m_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_e2m_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_e2m_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_e2m_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_e2m_enqP_lat_0;
  MOD_Wire<tUInt8> INST_e2m_enqP_lat_1;
  MOD_Reg<tUInt8> INST_e2m_enqP_rl;
  MOD_Reg<tUInt8> INST_eEpoch;
  MOD_Reg<tUInt8> INST_execRedirect_data_0_dummy2_0;
  MOD_Reg<tUInt8> INST_execRedirect_data_0_dummy2_1;
  MOD_Wire<tUInt64> INST_execRedirect_data_0_dummy_0_0;
  MOD_Wire<tUInt64> INST_execRedirect_data_0_dummy_0_1;
  MOD_Wire<tUInt64> INST_execRedirect_data_0_dummy_1_0;
  MOD_Wire<tUInt64> INST_execRedirect_data_0_dummy_1_1;
  MOD_Wire<tUInt32> INST_execRedirect_data_0_lat_0;
  MOD_Wire<tUInt32> INST_execRedirect_data_0_lat_1;
  MOD_Reg<tUInt32> INST_execRedirect_data_0_rl;
  MOD_Reg<tUInt8> INST_execRedirect_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_execRedirect_deqP_dummy2_1;
  MOD_Wire<tUInt8> INST_execRedirect_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_execRedirect_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_execRedirect_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_execRedirect_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_execRedirect_deqP_lat_0;
  MOD_Wire<tUInt8> INST_execRedirect_deqP_lat_1;
  MOD_Reg<tUInt8> INST_execRedirect_deqP_rl;
  MOD_Reg<tUInt8> INST_execRedirect_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_execRedirect_enqP_dummy2_1;
  MOD_Wire<tUInt8> INST_execRedirect_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_execRedirect_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_execRedirect_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_execRedirect_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_execRedirect_enqP_lat_0;
  MOD_Wire<tUInt8> INST_execRedirect_enqP_lat_1;
  MOD_Reg<tUInt8> INST_execRedirect_enqP_rl;
  MOD_Reg<tUWide> INST_f2d_data_0;
  MOD_Reg<tUInt8> INST_f2d_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_f2d_deqP_dummy2_1;
  MOD_Wire<tUInt8> INST_f2d_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_f2d_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_f2d_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_f2d_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_f2d_deqP_lat_0;
  MOD_Wire<tUInt8> INST_f2d_deqP_lat_1;
  MOD_Reg<tUInt8> INST_f2d_deqP_rl;
  MOD_Reg<tUInt8> INST_f2d_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_f2d_enqP_dummy2_1;
  MOD_Wire<tUInt8> INST_f2d_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_f2d_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_f2d_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_f2d_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_f2d_enqP_lat_0;
  MOD_Wire<tUInt8> INST_f2d_enqP_lat_1;
  MOD_Reg<tUInt8> INST_f2d_enqP_rl;
  MOD_Reg<tUInt8> INST_fEpoch;
  MOD_mkIMemory INST_iMem;
  MOD_Reg<tUWide> INST_m2w_data_0;
  MOD_Reg<tUInt8> INST_m2w_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_m2w_deqP_dummy2_1;
  MOD_Wire<tUInt8> INST_m2w_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_m2w_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_m2w_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_m2w_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_m2w_deqP_lat_0;
  MOD_Wire<tUInt8> INST_m2w_deqP_lat_1;
  MOD_Reg<tUInt8> INST_m2w_deqP_rl;
  MOD_Reg<tUInt8> INST_m2w_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_m2w_enqP_dummy2_1;
  MOD_Wire<tUInt8> INST_m2w_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_m2w_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_m2w_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_m2w_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_m2w_enqP_lat_0;
  MOD_Wire<tUInt8> INST_m2w_enqP_lat_1;
  MOD_Reg<tUInt8> INST_m2w_enqP_rl;
  MOD_Reg<tUInt8> INST_memRedirect_data_0_dummy2_0;
  MOD_Reg<tUInt8> INST_memRedirect_data_0_dummy2_1;
  MOD_Wire<tUInt64> INST_memRedirect_data_0_dummy_0_0;
  MOD_Wire<tUInt64> INST_memRedirect_data_0_dummy_0_1;
  MOD_Wire<tUInt64> INST_memRedirect_data_0_dummy_1_0;
  MOD_Wire<tUInt64> INST_memRedirect_data_0_dummy_1_1;
  MOD_Wire<tUInt32> INST_memRedirect_data_0_lat_0;
  MOD_Wire<tUInt32> INST_memRedirect_data_0_lat_1;
  MOD_Reg<tUInt32> INST_memRedirect_data_0_rl;
  MOD_Reg<tUInt8> INST_memRedirect_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_memRedirect_deqP_dummy2_1;
  MOD_Wire<tUInt8> INST_memRedirect_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_memRedirect_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_memRedirect_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_memRedirect_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_memRedirect_deqP_lat_0;
  MOD_Wire<tUInt8> INST_memRedirect_deqP_lat_1;
  MOD_Reg<tUInt8> INST_memRedirect_deqP_rl;
  MOD_Reg<tUInt8> INST_memRedirect_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_memRedirect_enqP_dummy2_1;
  MOD_Wire<tUInt8> INST_memRedirect_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_memRedirect_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_memRedirect_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_memRedirect_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_memRedirect_enqP_lat_0;
  MOD_Wire<tUInt8> INST_memRedirect_enqP_lat_1;
  MOD_Reg<tUInt8> INST_memRedirect_enqP_rl;
  MOD_Reg<tUInt32> INST_pc;
  MOD_mkBypassRFile INST_rf;
  MOD_Reg<tUInt8> INST_sb_fifoE_data_0;
  MOD_Reg<tUInt8> INST_sb_fifoE_data_1;
  MOD_Reg<tUInt8> INST_sb_fifoE_data_2;
  MOD_Reg<tUInt8> INST_sb_fifoE_data_3;
  MOD_Reg<tUInt8> INST_sb_fifoE_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_sb_fifoE_deqP_dummy2_1;
  MOD_Wire<tUInt8> INST_sb_fifoE_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_sb_fifoE_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_sb_fifoE_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_sb_fifoE_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_sb_fifoE_deqP_lat_0;
  MOD_Wire<tUInt8> INST_sb_fifoE_deqP_lat_1;
  MOD_Reg<tUInt8> INST_sb_fifoE_deqP_rl;
  MOD_Reg<tUInt8> INST_sb_fifoE_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_sb_fifoE_enqP_dummy2_1;
  MOD_Reg<tUInt8> INST_sb_fifoE_enqP_dummy2_2;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_lat_0;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_lat_1;
  MOD_Wire<tUInt8> INST_sb_fifoE_enqP_lat_2;
  MOD_Reg<tUInt8> INST_sb_fifoE_enqP_rl;
  MOD_Reg<tUInt8> INST_sb_fifoM_data_0;
  MOD_Reg<tUInt8> INST_sb_fifoM_data_1;
  MOD_Reg<tUInt8> INST_sb_fifoM_data_2;
  MOD_Reg<tUInt8> INST_sb_fifoM_data_3;
  MOD_Reg<tUInt8> INST_sb_fifoM_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_sb_fifoM_deqP_dummy2_1;
  MOD_Wire<tUInt8> INST_sb_fifoM_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_sb_fifoM_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_sb_fifoM_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_sb_fifoM_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_sb_fifoM_deqP_lat_0;
  MOD_Wire<tUInt8> INST_sb_fifoM_deqP_lat_1;
  MOD_Reg<tUInt8> INST_sb_fifoM_deqP_rl;
  MOD_Reg<tUInt8> INST_sb_fifoM_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_sb_fifoM_enqP_dummy2_1;
  MOD_Reg<tUInt8> INST_sb_fifoM_enqP_dummy2_2;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_lat_0;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_lat_1;
  MOD_Wire<tUInt8> INST_sb_fifoM_enqP_lat_2;
  MOD_Reg<tUInt8> INST_sb_fifoM_enqP_rl;
  MOD_Reg<tUInt8> INST_stat;
  MOD_Reg<tUInt8> INST_statRedirect_data_0_dummy2_0;
  MOD_Reg<tUInt8> INST_statRedirect_data_0_dummy2_1;
  MOD_Wire<tUInt8> INST_statRedirect_data_0_dummy_0_0;
  MOD_Wire<tUInt8> INST_statRedirect_data_0_dummy_0_1;
  MOD_Wire<tUInt8> INST_statRedirect_data_0_dummy_1_0;
  MOD_Wire<tUInt8> INST_statRedirect_data_0_dummy_1_1;
  MOD_Wire<tUInt8> INST_statRedirect_data_0_lat_0;
  MOD_Wire<tUInt8> INST_statRedirect_data_0_lat_1;
  MOD_Reg<tUInt8> INST_statRedirect_data_0_rl;
  MOD_Reg<tUInt8> INST_statRedirect_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_statRedirect_deqP_dummy2_1;
  MOD_Wire<tUInt8> INST_statRedirect_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_statRedirect_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_statRedirect_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_statRedirect_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_statRedirect_deqP_lat_0;
  MOD_Wire<tUInt8> INST_statRedirect_deqP_lat_1;
  MOD_Reg<tUInt8> INST_statRedirect_deqP_rl;
  MOD_Reg<tUInt8> INST_statRedirect_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_statRedirect_enqP_dummy2_1;
  MOD_Wire<tUInt8> INST_statRedirect_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_statRedirect_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_statRedirect_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_statRedirect_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_statRedirect_enqP_lat_0;
  MOD_Wire<tUInt8> INST_statRedirect_enqP_lat_1;
  MOD_Reg<tUInt8> INST_statRedirect_enqP_rl;
 
 /* Constructor */
 public:
  MOD_mkProc(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUWide PORT_cpuToHost;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_cop_started____d164;
  tUInt8 DEF_d2e_data_0_305_BIT_0_306_EQ_eEpoch_307___d1308;
  tUInt8 DEF_d2e_data_0_305_BITS_230_TO_227___d1310;
  tUInt8 DEF_d2e_data_0_305_BITS_223_TO_221___d1314;
  tUInt8 DEF_cnt1__h7231;
  tUInt8 DEF_d2e_data_0_305_BIT_98___d1312;
  tUInt8 DEF_IF_d2e_data_0_305_BITS_230_TO_227_310_EQ_9_311_ETC___d1360;
  tUInt8 DEF_IF_IF_sb_fifoE_enqP_dummy2_0_25_AND_sb_fifoE_e_ETC___d440;
  tUWide DEF_m2w_data_0___d1708;
  tUWide DEF_e2m_data_0___d1542;
  tUWide DEF_d2e_data_0___d1305;
  tUWide DEF_f2d_data_0___d351;
  tUInt8 DEF_sb_fifoM_data_3___d491;
  tUInt8 DEF_sb_fifoM_data_2___d489;
  tUInt8 DEF_sb_fifoM_data_1___d487;
  tUInt8 DEF_sb_fifoM_data_0___d485;
  tUInt8 DEF_sb_fifoE_data_3___d342;
  tUInt8 DEF_sb_fifoE_data_2___d340;
  tUInt8 DEF_sb_fifoE_data_1___d338;
  tUInt8 DEF_sb_fifoE_data_0___d336;
  tUInt8 DEF_upd__h51248;
  tUInt8 DEF_upd__h31952;
  tUInt8 DEF_upd__h32360;
  tUInt8 DEF_upd__h50915;
  tUInt8 DEF_upd__h29938;
  tUInt8 DEF_upd__h31017;
  tUInt8 DEF_condFlag___d1318;
  tUInt8 DEF_upd__h48683;
  tUInt8 DEF_upd__h47168;
  tUInt8 DEF_upd__h46854;
  tUInt8 DEF_upd__h45301;
  tUInt8 DEF_upd__h42032;
  tUInt8 DEF_upd__h41718;
  tUInt8 DEF_upd__h42454;
  tUInt8 DEF_upd__h37624;
  tUInt8 DEF_upd__h37310;
  tUInt8 DEF_upd__h28866;
  tUInt8 DEF_upd__h28397;
  tUInt8 DEF_upd__h28083;
  tUInt8 DEF_upd__h49147;
  tUInt8 DEF_upd__h51603;
  tUInt8 DEF_upd__h51661;
  tUInt8 DEF_upd__h40460;
  tUInt8 DEF_upd__h45913;
  tUInt8 DEF_upd__h40393;
  tUInt8 DEF_upd__h26505;
  tUInt8 DEF_upd__h40930;
  tUInt8 DEF_sb_fifoM_deqP_dummy2_1__h31920;
  tUInt8 DEF_sb_fifoM_deqP_dummy2_0__h51284;
  tUInt8 DEF_sb_fifoM_enqP_dummy2_2__h32529;
  tUInt8 DEF_sb_fifoM_enqP_dummy2_1__h32514;
  tUInt8 DEF_sb_fifoM_enqP_dummy2_0__h32501;
  tUInt8 DEF_sb_fifoE_deqP_dummy2_1__h29906;
  tUInt8 DEF_sb_fifoE_deqP_dummy2_0__h50951;
  tUInt8 DEF_sb_fifoE_enqP_dummy2_2__h31186;
  tUInt8 DEF_sb_fifoE_enqP_dummy2_1__h31171;
  tUInt8 DEF_sb_fifoE_enqP_dummy2_0__h31158;
  tUInt8 DEF_m2w_deqP_dummy2_1__h47136;
  tUInt8 DEF_m2w_deqP_dummy2_0__h48719;
  tUInt8 DEF_m2w_enqP_dummy2_1__h46974;
  tUInt8 DEF_m2w_enqP_dummy2_0__h46961;
  tUInt8 DEF_e2m_deqP_dummy2_1__h42000;
  tUInt8 DEF_e2m_deqP_dummy2_0__h45337;
  tUInt8 DEF_e2m_enqP_dummy2_1__h41838;
  tUInt8 DEF_e2m_enqP_dummy2_0__h41825;
  tUInt8 DEF_d2e_deqP_dummy2_1__h37592;
  tUInt8 DEF_d2e_deqP_dummy2_0__h42490;
  tUInt8 DEF_d2e_enqP_dummy2_1__h37430;
  tUInt8 DEF_d2e_enqP_dummy2_0__h37417;
  tUInt8 DEF_f2d_deqP_dummy2_1__h28365;
  tUInt8 DEF_f2d_deqP_dummy2_0__h28902;
  tUInt8 DEF_f2d_enqP_dummy2_1__h28203;
  tUInt8 DEF_f2d_enqP_dummy2_0__h28190;
  tUInt8 DEF_statRedirect_deqP_dummy2_1__h49267;
  tUInt8 DEF_statRedirect_deqP_dummy2_0__h49254;
  tUInt8 DEF_statRedirect_enqP_dummy2_1__h51629;
  tUInt8 DEF_statRedirect_enqP_dummy2_0__h49073;
  tUInt8 DEF_memRedirect_deqP_dummy2_1__h40580;
  tUInt8 DEF_memRedirect_deqP_dummy2_0__h40567;
  tUInt8 DEF_memRedirect_enqP_dummy2_1__h40361;
  tUInt8 DEF_memRedirect_enqP_dummy2_0__h45949;
  tUInt8 DEF_execRedirect_deqP_dummy2_1__h26625;
  tUInt8 DEF_execRedirect_deqP_dummy2_0__h26612;
  tUInt8 DEF_execRedirect_enqP_dummy2_1__h26406;
  tUInt8 DEF_execRedirect_enqP_dummy2_0__h40966;
  tUInt8 DEF_eEpoch__h40892;
  tUInt32 DEF_valP__h42687;
  tUInt32 DEF_d2e_data_0_305_BITS_97_TO_66___d1361;
  tUInt8 DEF_e2m_data_0_542_BITS_282_TO_279___d1545;
  tUInt8 DEF_iCode__h29044;
  tUInt8 DEF_fCode__h29045;
  tUInt8 DEF_regA__h29046;
  tUInt8 DEF_regB__h29047;
  tUInt8 DEF_sb_fifoM_data_3_91_BITS_3_TO_0___d510;
  tUInt8 DEF_sb_fifoM_data_2_89_BITS_3_TO_0___d509;
  tUInt8 DEF_sb_fifoM_data_1_87_BITS_3_TO_0___d508;
  tUInt8 DEF_sb_fifoM_data_0_85_BITS_3_TO_0___d507;
  tUInt8 DEF_sb_fifoE_data_3_42_BITS_3_TO_0___d418;
  tUInt8 DEF_sb_fifoE_data_2_40_BITS_3_TO_0___d417;
  tUInt8 DEF_sb_fifoE_data_0_36_BITS_3_TO_0___d415;
  tUInt8 DEF_sb_fifoE_data_1_38_BITS_3_TO_0___d416;
  tUInt8 DEF_n__read__h51247;
  tUInt8 DEF_n__read__h50914;
  tUInt8 DEF_x__h32576;
  tUInt8 DEF_x__h31777;
  tUInt8 DEF_x__h31233;
  tUInt8 DEF_x__h29763;
  tUInt8 DEF_m2w_data_0_708_BIT_283___d1709;
  tUInt8 DEF_e2m_data_0_542_BIT_283___d1543;
  tUInt8 DEF_sb_fifoM_data_3_91_BIT_5___d492;
  tUInt8 DEF_sb_fifoM_data_3_91_BIT_4___d503;
  tUInt8 DEF_sb_fifoM_data_2_89_BIT_5___d490;
  tUInt8 DEF_sb_fifoM_data_2_89_BIT_4___d502;
  tUInt8 DEF_sb_fifoM_data_1_87_BIT_5___d488;
  tUInt8 DEF_sb_fifoM_data_1_87_BIT_4___d501;
  tUInt8 DEF_sb_fifoM_data_0_85_BIT_5___d486;
  tUInt8 DEF_sb_fifoM_data_0_85_BIT_4___d500;
  tUInt8 DEF_sb_fifoE_data_3_42_BIT_5___d343;
  tUInt8 DEF_sb_fifoE_data_3_42_BIT_4___d403;
  tUInt8 DEF_sb_fifoE_data_2_40_BIT_5___d341;
  tUInt8 DEF_sb_fifoE_data_2_40_BIT_4___d402;
  tUInt8 DEF_sb_fifoE_data_0_36_BIT_5___d337;
  tUInt8 DEF_sb_fifoE_data_0_36_BIT_4___d400;
  tUInt8 DEF_sb_fifoE_data_1_38_BIT_5___d339;
  tUInt8 DEF_sb_fifoE_data_1_38_BIT_4___d401;
  tUInt8 DEF_condFlag_318_BIT_2___d1320;
  tUInt8 DEF_condFlag_318_BIT_1___d1325;
  tUInt8 DEF_condFlag_318_BIT_0___d1319;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BITS_3_TO_0_07_sb_f_ETC___d555;
  tUInt8 DEF_ptr__h32787;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BITS_3_TO_0_07_sb_f_ETC___d541;
  tUInt8 DEF_ptr__h32646;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BITS_3_TO_0_07_sb_f_ETC___d566;
  tUInt8 DEF_y__h32608;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BITS_3_TO_0_07_sb_f_ETC___d512;
  tUInt8 DEF_ptr__h31751;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BITS_3_TO_0_15_sb_f_ETC___d463;
  tUInt8 DEF_ptr__h31444;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BITS_3_TO_0_15_sb_f_ETC___d449;
  tUInt8 DEF_ptr__h31303;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BITS_3_TO_0_15_sb_f_ETC___d474;
  tUInt8 DEF_y__h31265;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BITS_3_TO_0_15_sb_f_ETC___d420;
  tUInt8 DEF_ptr__h29737;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BIT_4_00_sb_fifoM_d_ETC___d553;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BIT_4_00_sb_fifoM_d_ETC___d539;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BIT_4_00_sb_fifoM_d_ETC___d564;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BIT_4_00_sb_fifoM_d_ETC___d505;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BIT_5_86_sb_fifoM_d_ETC___d552;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BIT_5_86_sb_fifoM_d_ETC___d538;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BIT_5_86_sb_fifoM_d_ETC___d563;
  tUInt8 DEF_SEL_ARR_sb_fifoM_data_0_85_BIT_5_86_sb_fifoM_d_ETC___d499;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BIT_5_37_sb_fifoE_d_ETC___d460;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BIT_5_37_sb_fifoE_d_ETC___d446;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BIT_5_37_sb_fifoE_d_ETC___d471;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BIT_5_37_sb_fifoE_d_ETC___d350;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BIT_4_00_sb_fifoE_d_ETC___d461;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BIT_4_00_sb_fifoE_d_ETC___d447;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BIT_4_00_sb_fifoE_d_ETC___d472;
  tUInt8 DEF_SEL_ARR_sb_fifoE_data_0_36_BIT_4_00_sb_fifoE_d_ETC___d405;
  tUInt8 DEF_IF_IF_memRedirect_enqP_dummy2_1_295_THEN_IF_me_ETC___d1304;
  tUInt8 DEF_IF_e2m_data_0_542_BIT_283_543_THEN_e2m_data_0__ETC___d1547;
  tUInt32 DEF_IF_IF_d2e_data_0_305_BITS_230_TO_227_310_EQ_9__ETC___d1365;
  tUInt32 DEF_IF_d2e_data_0_305_BITS_230_TO_227_310_EQ_9_311_ETC___d1364;
  tUInt8 DEF_d2e_data_0_305_BITS_223_TO_221_314_EQ_0_315_OR_ETC___d1357;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1100___d372;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1000_69__ETC___d371;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1001___d370;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1000___d369;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b111___d365;
  tUInt8 DEF_IF_e2m_data_0_542_BIT_283_543_THEN_e2m_data_0__ETC___d1546;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d414;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d591;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_58_ETC___d667;
  tUInt8 DEF_cnt1__h26082;
  tUInt8 DEF_IF_sb_fifoM_deqP_lat_0_whas__49_THEN_sb_fifoM__ETC___d152;
  tUInt8 DEF_cnt1__h23033;
  tUInt8 DEF_f2d_data_0_51_BITS_108_TO_105_91_EQ_0b0___d392;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100___d360;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b10___d363;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b110___d364;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1010___d367;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1011___d368;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1001__ETC___d411;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b101___d361;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11___d358;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0___d353;
  tUInt8 DEF_IF_sb_fifoE_deqP_lat_0_whas__32_THEN_sb_fifoE__ETC___d135;
  tUInt8 DEF_y__h49300;
  tUInt8 DEF_x__h49299;
  tUInt8 DEF_IF_statRedirect_enqP_lat_0_whas__2_THEN_statRe_ETC___d55;
  tUInt8 DEF_n__read__h48682;
  tUInt8 DEF_n__read__h45912;
  tUInt8 DEF_IF_m2w_deqP_lat_0_whas__15_THEN_m2w_deqP_lat_0_ETC___d118;
  tUInt8 DEF_x__h47206;
  tUInt8 DEF_n__read__h45300;
  tUInt8 DEF_n__read__h40929;
  tUInt8 DEF_y__h40613;
  tUInt8 DEF_n__read__h40259;
  tUInt8 DEF_IF_memRedirect_enqP_lat_0_whas__1_THEN_memRedi_ETC___d34;
  tUInt8 DEF_IF_e2m_deqP_lat_0_whas__01_THEN_e2m_deqP_lat_0_ETC___d104;
  tUInt8 DEF_x__h42070;
  tUInt8 DEF_n__read__h42453;
  tUInt8 DEF_IF_d2e_deqP_lat_0_whas__7_THEN_d2e_deqP_lat_0__ETC___d90;
  tUInt8 DEF_x__h37662;
  tUInt8 DEF_n__read__h28865;
  tUInt8 DEF_y__h26658;
  tUInt8 DEF_IF_f2d_deqP_lat_0_whas__3_THEN_f2d_deqP_lat_0__ETC___d76;
  tUInt8 DEF_x__h28435;
  tUInt8 DEF_NOT_d2e_data_0_305_BIT_98_312___d1345;
  tUInt8 DEF_IF_d2e_data_0_305_BITS_230_TO_227_310_EQ_9_311_ETC___d1343;
  tUInt8 DEF_d2e_data_0_305_BITS_230_TO_227_310_EQ_10___d1340;
  tUInt8 DEF_IF_d2e_data_0_305_BITS_223_TO_221_314_EQ_1_317_ETC___d1356;
  tUInt8 DEF_NOT_condFlag_318_BIT_0_319_346_AND_condFlag_31_ETC___d1347;
  tUInt8 DEF_IF_d2e_data_0_305_BITS_223_TO_221_314_EQ_1_317_ETC___d1337;
  tUInt8 DEF_condFlag_318_BIT_0_319_OR_NOT_condFlag_318_BIT_ETC___d1322;
  tUInt8 DEF_NOT_condFlag_318_BIT_2_320___d1321;
  tUInt8 DEF_condFlag_318_BIT_1_325_AND_NOT_condFlag_318_BI_ETC___d1348;
  tUInt8 DEF_NOT_condFlag_318_BIT_1_325_326_OR_condFlag_318_ETC___d1327;
  tUInt8 DEF_NOT_condFlag_318_BIT_1_325_326_AND_NOT_condFla_ETC___d1350;
  tUInt8 DEF_condFlag_318_BIT_1_325_OR_condFlag_318_BIT_2_320___d1331;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1001_70___d389;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1000_69___d388;
  tUInt8 DEF_d2e_data_0_305_BITS_220_TO_189_344_EQ_IF_IF_d2_ETC___d1366;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_58_ETC___d713;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_58_ETC___d707;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_58_ETC___d701;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_58_ETC___d695;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_58_ETC___d686;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_58_ETC___d680;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_58_ETC___d674;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_58_ETC___d668;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d637;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d631;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d625;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d619;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d610;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d604;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d598;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d592;
  tUInt8 DEF_IF_IF_sb_fifoM_enqP_dummy2_0_17_AND_sb_fifoM_e_ETC___d571;
  tUInt8 DEF_x__h32609;
  tUInt8 DEF_IF_IF_sb_fifoE_enqP_dummy2_0_25_AND_sb_fifoE_e_ETC___d479;
  tUInt8 DEF_x__h31266;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d567;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d556;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d542;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d513;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d475;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d464;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d450;
  tUInt8 DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_6_ETC___d421;
  tUInt8 DEF_f2d_data_0_51_BITS_104_TO_101_10_EQ_SEL_ARR_sb_ETC___d765;
  tUInt8 DEF_f2d_data_0_51_BITS_104_TO_101_10_EQ_SEL_ARR_sb_ETC___d760;
  tUInt8 DEF_f2d_data_0_51_BITS_104_TO_101_10_EQ_SEL_ARR_sb_ETC___d755;
  tUInt8 DEF_f2d_data_0_51_BITS_104_TO_101_10_EQ_SEL_ARR_sb_ETC___d750;
  tUInt8 DEF_f2d_data_0_51_BITS_104_TO_101_10_EQ_SEL_ARR_sb_ETC___d742;
  tUInt8 DEF_f2d_data_0_51_BITS_104_TO_101_10_EQ_SEL_ARR_sb_ETC___d737;
  tUInt8 DEF_f2d_data_0_51_BITS_104_TO_101_10_EQ_SEL_ARR_sb_ETC___d732;
  tUInt8 DEF_f2d_data_0_51_BITS_104_TO_101_10_EQ_SEL_ARR_sb_ETC___d727;
  tUInt8 DEF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1___d355;
  tUInt8 DEF_d2e_data_0_305_BITS_223_TO_221_314_EQ_0___d1315;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_5_ETC___d712;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_5_ETC___d662;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_5_ETC___d706;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_5_ETC___d700;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_5_ETC___d694;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_5_ETC___d685;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_5_ETC___d679;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_5_ETC___d673;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_5_ETC___d663;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100__ETC___d565;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100__ETC___d399;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100__ETC___d554;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100__ETC___d540;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100__ETC___d506;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100__ETC___d473;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100__ETC___d462;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100__ETC___d448;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100__ETC___d406;
  tUInt8 DEF_NOT_m2w_data_0_708_BIT_283_709___d1710;
  tUInt8 DEF_NOT_e2m_data_0_542_BIT_283_543___d1544;
  tUInt8 DEF_NOT_IF_e2m_data_0_542_BIT_283_543_THEN_e2m_dat_ETC___d1548;
  tUInt8 DEF_NOT_d2e_data_0_305_BITS_223_TO_221_314_EQ_0_31_ETC___d1338;
  tUInt8 DEF_NOT_condFlag_318_BIT_1_325___d1326;
  tUInt8 DEF_IF_IF_sb_fifoM_enqP_dummy2_0_17_AND_sb_fifoM_e_ETC___d560;
  tUInt8 DEF_IF_IF_sb_fifoM_enqP_dummy2_0_17_AND_sb_fifoM_e_ETC___d546;
  tUInt8 DEF_IF_IF_sb_fifoM_enqP_dummy2_0_17_AND_sb_fifoM_e_ETC___d532;
  tUInt8 DEF_IF_IF_sb_fifoE_enqP_dummy2_0_25_AND_sb_fifoE_e_ETC___d468;
  tUInt8 DEF_IF_IF_sb_fifoE_enqP_dummy2_0_25_AND_sb_fifoE_e_ETC___d454;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0_53_ETC___d656;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0_53_ETC___d586;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_108_TO_105_91_EQ_0b0_92___d393;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b100_60___d383;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b101_61___d362;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b10_63___d384;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b110_64___d385;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b111_65___d366;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1010_67___d386;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1011_68___d387;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1000_ETC___d390;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0_53_ETC___d357;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b1_55___d356;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b11_58___d359;
  tUInt8 DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0_53___d354;
  tUInt8 DEF_x__h32607;
  tUInt8 DEF_x__h31264;
 
 /* Local definitions */
 private:
  tUInt32 DEF_upd__h41435;
  tUInt32 DEF_upd__h41568;
  tUInt32 DEF_upd__h27091;
  tUInt32 DEF_upd__h27224;
  tUInt8 DEF_statRedirect_data_0_rl__h8397;
  tUInt8 DEF_upd__h26438;
  tUWide DEF_IF_e2m_data_0_542_BIT_283_543_THEN_IF_e2m_data_ETC___d1645;
  tUWide DEF_IF_e2m_data_0_542_BIT_283_543_THEN_e2m_data_0__ETC___d1635;
  tUWide DEF_DONTCARE_CONCAT_DONTCARE_CONCAT_DONTCARE_636_C_ETC___d1644;
  tUInt32 DEF_IF_memRedirect_data_0_lat_0_whas__4_THEN_memRe_ETC___d27;
  tUInt32 DEF_IF_execRedirect_data_0_lat_0_whas_THEN_execRed_ETC___d6;
  tUInt8 DEF_IF_statRedirect_data_0_lat_0_whas__5_THEN_stat_ETC___d48;
  tUInt8 DEF_IF_execRedirect_enqP_lat_0_whas__0_THEN_execRe_ETC___d13;
  tUWide DEF_IF_IF_memRedirect_enqP_dummy2_1_295_THEN_IF_me_ETC___d1506;
  tUWide DEF_IF_IF_IF_memRedirect_enqP_dummy2_1_295_THEN_IF_ETC___d1505;
  tUWide DEF_e2m_data_0_542_BIT_283_543_CONCAT_IF_e2m_data__ETC___d1646;
  tUWide DEF_DONTCARE_CONCAT_DONTCARE_CONCAT_DONTCARE_CONCA_ETC___d1643;
  tUWide DEF__0_OR_e2m_data_0_542_BIT_272_569_570_CONCAT_IF__ETC___d1634;
  tUWide DEF_IF_IF_IF_memRedirect_enqP_dummy2_1_295_THEN_IF_ETC___d1504;
  tUWide DEF_DONTCARE_CONCAT_DONTCARE_CONCAT_DONTCARE_CONCA_ETC___d1642;
  tUWide DEF__0_OR_e2m_data_0_542_BIT_233_578_579_CONCAT_IF__ETC___d1633;
  tUWide DEF_IF_IF_IF_memRedirect_enqP_dummy2_1_295_THEN_IF_ETC___d1503;
  tUWide DEF_DONTCARE_CONCAT_DONTCARE_CONCAT_DONTCARE_CONCA_ETC___d1641;
  tUWide DEF_IF_e2m_data_0_542_BIT_283_543_THEN_e2m_data_0__ETC___d1632;
  tUWide DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0_53__ETC___d1273;
  tUWide DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0_53__ETC___d1271;
  tUWide DEF_IF_IF_IF_memRedirect_enqP_dummy2_1_295_THEN_IF_ETC___d1502;
  tUWide DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0_53__ETC___d1270;
  tUWide DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0_53__ETC___d1269;
  tUWide DEF_DONTCARE_CONCAT_DONTCARE_CONCAT_DONTCARE_CONCA_ETC___d1640;
  tUWide DEF_e2m_data_0_542_BITS_102_TO_98_612_CONCAT_IF_e2_ETC___d1631;
  tUWide DEF_IF_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0_53__ETC___d1268;
  tUWide DEF_NOT_f2d_data_0_51_BITS_112_TO_109_52_EQ_0b0_53_ETC___d1267;
  tUWide DEF_IF_IF_IF_memRedirect_enqP_dummy2_1_295_THEN_IF_ETC___d1501;
  tUWide DEF_iMem_req_IF_IF_IF_execRedirect_enqP_dummy2_1_6_ETC___d327;
  tUWide DEF_DONTCARE_CONCAT_DONTCARE_CONCAT_DONTCARE_CONCA_ETC___d1639;
  tUWide DEF_IF_IF_e2m_data_0_542_BIT_283_543_THEN_e2m_data_ETC___d1630;
  tUWide DEF_IF_IF_IF_memRedirect_enqP_dummy2_1_295_THEN_IF_ETC___d1500;
  tUWide DEF_NOT_IF_e2m_data_0_542_BIT_283_543_THEN_e2m_dat_ETC___d1667;
 
 /* Rules */
 public:
  void RL_execRedirect_data_0_canon();
  void RL_execRedirect_enqP_canon();
  void RL_execRedirect_deqP_canon();
  void RL_memRedirect_data_0_canon();
  void RL_memRedirect_enqP_canon();
  void RL_memRedirect_deqP_canon();
  void RL_statRedirect_data_0_canon();
  void RL_statRedirect_enqP_canon();
  void RL_statRedirect_deqP_canon();
  void RL_f2d_enqP_canon();
  void RL_f2d_deqP_canon();
  void RL_d2e_enqP_canon();
  void RL_d2e_deqP_canon();
  void RL_e2m_enqP_canon();
  void RL_e2m_deqP_canon();
  void RL_m2w_enqP_canon();
  void RL_m2w_deqP_canon();
  void RL_sb_fifoE_enqP_canon();
  void RL_sb_fifoE_deqP_canon();
  void RL_sb_fifoM_enqP_canon();
  void RL_sb_fifoM_deqP_canon();
  void RL_doFetch();
  void RL_doDecode();
  void RL_doExecute();
  void RL_doMemory();
  void RL_doWriteBack();
  void RL_upd_Stat();
  void RL_statHLT();
  void RL_statINS();
 
 /* Methods */
 public:
  tUWide METH_cpuToHost();
  tUInt8 METH_RDY_cpuToHost();
  void METH_hostToCpu(tUInt32 ARG_hostToCpu_startpc);
  tUInt8 METH_RDY_hostToCpu();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkProc &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkProc &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkProc &backing);
  void vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkProc &backing);
};

#endif /* ifndef __mkProc_h__ */
