/**
 *
 * Name:
 *   bp_be_director.v
 *
 * Description:
 *   Directs the PC for the FE and the calculator. Keeps track of the next PC
 *     and sends redirect signals to the FE when a misprediction is detected.
 *
 * Notes:
 *
 */

`include "bp_common_defines.svh"
`include "bp_be_defines.svh"

module bp_be_director
 import bp_common_pkg::*;
 import bp_be_pkg::*;
 #(parameter bp_params_e bp_params_p = e_bp_default_cfg
   `declare_bp_proc_params(bp_params_p)
   `declare_bp_core_if_widths(vaddr_width_p, paddr_width_p, asid_width_p, branch_metadata_fwd_width_p)

   // Generated parameters
   , localparam cfg_bus_width_lp = `bp_cfg_bus_width(vaddr_width_p, hio_width_p, core_id_width_p, cce_id_width_p, lce_id_width_p, did_width_p)
   , localparam issue_pkt_width_lp = `bp_be_issue_pkt_width(vaddr_width_p, branch_metadata_fwd_width_p)
   , localparam branch_pkt_width_lp = `bp_be_branch_pkt_width(vaddr_width_p)
   , localparam commit_pkt_width_lp = `bp_be_commit_pkt_width(vaddr_width_p, paddr_width_p)
   )
  (input                                clk_i
   , input                              reset_i

   , input [cfg_bus_width_lp-1:0]       cfg_bus_i

   // Dependency information
   , input [issue_pkt_width_lp-1:0]     issue_pkt_i
   , output logic [vaddr_width_p-1:0]   expected_npc_o
   , output logic                       poison_isd_o
   , output logic                       clear_iss_o
   , output logic                       suppress_iss_o
   , output logic                       resume_o
   , input                              irq_waiting_i
   , input                              mem_busy_i
   , output logic                       cmd_full_n_o
   , output logic                       cmd_full_r_o

   // FE-BE interface
   , output logic [fe_cmd_width_lp-1:0] fe_cmd_o
   , output logic                       fe_cmd_v_o
   , input                              fe_cmd_yumi_i

   , input [branch_pkt_width_lp-1:0]    br_pkt_i
   , input [commit_pkt_width_lp-1:0]    commit_pkt_i
   );

  // Declare parameterized structures
  `declare_bp_cfg_bus_s(vaddr_width_p, hio_width_p, core_id_width_p, cce_id_width_p, lce_id_width_p, did_width_p);
  `declare_bp_core_if(vaddr_width_p, paddr_width_p, asid_width_p, branch_metadata_fwd_width_p);
  `declare_bp_be_internal_if_structs(vaddr_width_p, paddr_width_p, asid_width_p, branch_metadata_fwd_width_p);

  `bp_cast_i(bp_cfg_bus_s, cfg_bus);
  `bp_cast_i(bp_be_issue_pkt_s, issue_pkt);
  `bp_cast_i(bp_be_branch_pkt_s, br_pkt);
  `bp_cast_i(bp_be_commit_pkt_s, commit_pkt);

  bp_fe_cmd_s fe_cmd_li;
  bp_fe_cmd_pc_redirect_operands_s fe_cmd_pc_redirect_operands;
  logic fe_cmd_v_li;
  logic cmd_empty_n_lo, cmd_empty_r_lo;
  logic cmd_full_n_lo, cmd_full_r_lo;

  // Declare intermediate signals
  enum logic [3:0] {e_freeze, e_fencei, e_run, e_cmd_fence, e_wait} state_n, state_r;
  wire is_freeze    = (state_r == e_freeze);
  wire is_fencei    = (state_r == e_fencei);
  wire is_run       = (state_r == e_run);
  wire is_cmd_fence = (state_r == e_cmd_fence);
  wire is_wait      = (state_r == e_wait);

//================================================

// the director, is concerned with
// freeze, fencei, e_run, e_cmd_fence, e_wait
//

// The provided SystemVerilog code snippet declares intermediate signals and defines an enumerated type for different states. Here's a detailed explanation of what is implied by the instructions:
// 
// 1. **Enumerated Type Declaration**:
//    ```systemverilog
//    enum logic [3:0] {e_freeze, e_fencei, e_run, e_cmd_fence, e_wait} state_n, state_r;
//    ```
//    - This line defines an enumerated type with five possible states: `e_freeze`, `e_fencei`, `e_run`, `e_cmd_fence`, and `e_wait`.
//    - `state_n` and `state_r` are variables of this enumerated type, representing the next state and the current state, respectively.
//    - The `[3:0]` indicates that the enumerated type is represented using 4 bits.
// 
// 2. **State Comparison Wires**:
//    ```systemverilog
//    wire is_freeze    = (state_r == e_freeze);
//    wire is_fencei    = (state_r == e_fencei);
//    wire is_run       = (state_r == e_run);
//    wire is_cmd_fence = (state_r == e_cmd_fence);
//    wire is_wait      = (state_r == e_wait);
//    ```
//    - These lines define five wires (`is_freeze`, `is_fencei`, `is_run`, `is_cmd_fence`, `is_wait`).
//    - Each wire is assigned a boolean value based on whether `state_r` (the current state) matches one of the enumerated states.
//    - For example, `is_freeze` will be `1` (true) if `state_r` is `e_freeze`, and `0` (false) otherwise.
// 
// **Implications**:
// - The enumerated type and state comparison wires are used to manage and track the state of the module.
// - By comparing `state_r` with the enumerated states, the module can easily determine its current state and take appropriate actions based on that state.
// - This approach improves code readability and maintainability by using meaningful names for states and simplifying state comparisons.
// 
// If you have any specific questions or need further clarification on any part of this code, feel free to ask!




//================================================

// Yes, I understand this code. It's a Verilog module named `bp_be_director` 
// that is part of a larger system, likely a processor or a similar digital system. 
// Here's a brief breakdown of what it does:
// 
// it's a PC watcher. For the FE and Calc.
// handles redirect signals which it sends back to the FE
//
// here is some volcabulary
// clock, reset
// configuration bus
// issue packet
// branch packet
// commit packet
//
// - **Purpose**: The module directs the Program Counter (PC) for the 
// Front End (FE) and the calculator. It keeps track of the next PC and 
// sends redirect signals to the FE when a misprediction is detected.
// - **Parameters and Imports**: It imports several packages and defines 
// parameters for configuration.
// - **Inputs and Outputs**: It has various inputs and outputs for clock, 
// reset, configuration bus, issue packet, branch packet, commit packet, 
// and several control signals.
// - **Internal Signals and States**: It declares several internal signals 
// and states to manage the control flow and interactions between different parts of the system.
// 
// If you have any specific questions about this code or need further 
// explanation on any part of it, feel free to ask!



//================================================



  // Module instantiations
  // Update the NPC on a valid instruction in ex1 or upon commit

  // declare a address object, it's size is vaddr_width-1 to 0

  logic [vaddr_width_p-1:0] npc_n, npc_r;

// set a valid value for npc_w_v, it's either 1 or 2
  wire npc_w_v = commit_pkt_cast_i.npc_w_v | br_pkt_cast_i.v;

  assign npc_n = commit_pkt_cast_i.npc_w_v ? commit_pkt_cast_i.npc : br_pkt_cast_i.bspec ? issue_pkt_cast_i.pc : br_pkt_cast_i.npc;
  bsg_dff_reset_en_bypass
   #(.width_p(vaddr_width_p))
   npc_reg
    (.clk_i(clk_i)
     ,.reset_i(reset_i)
     ,.en_i(npc_w_v)

     ,.data_i(npc_n)
     ,.data_o(npc_r)
     );
  assign expected_npc_o = npc_r;

  wire instr_v = issue_pkt_cast_i.v & ~commit_pkt_cast_i.npc_w_v;
  wire npc_mismatch_v = instr_v & (expected_npc_o != issue_pkt_cast_i.pc);
  wire npc_match_v    = instr_v & (expected_npc_o == issue_pkt_cast_i.pc);
  assign poison_isd_o = npc_mismatch_v;
//================================
// Yes, I understand this code. Here's a breakdown of what it does:
// 
// 1. **Next Program Counter (NPC) Assignment**:
//    ```systemverilog
//    assign npc_n = commit_pkt_cast_i.npc_w_v ? commit_pkt_cast_i.npc : 
// br_pkt_cast_i.bspec ? issue_pkt_cast_i.pc : br_pkt_cast_i.npc;
//    ```
//    - This line assigns the next program counter (`npc_n`) based on the conditions:
//      - If `commit_pkt_cast_i.npc_w_v` is true, `npc_n` is set to `commit_pkt_cast_i.npc`.
//      - Otherwise, if `br_pkt_cast_i.bspec` is true, `npc_n` is set to `issue_pkt_cast_i.pc`.
//      - If neither condition is true, `npc_n` is set to `br_pkt_cast_i.npc`.
// 
// 2. **NPC Register**:
//    ```systemverilog
//    bsg_dff_reset_en_bypass
//     #(.width_p(vaddr_width_p))
//     npc_reg
//      (.clk_i(clk_i)
//       ,.reset_i(reset_i)
//       ,.en_i(npc_w_v)
//       ,.data_i(npc_n)
//       ,.data_o(npc_r)
//       );
//    ```
//    - This block instantiates a register (`npc_reg`) with 
// a reset and enable signal.
//    - The register stores the value of `npc_n` and outputs it as `npc_r`.
//    - The register is updated on the clock edge (`clk_i`) 
// if `npc_w_v` is true, and it can be reset with `reset_i`.
// 
// 3. **Expected NPC Output**:
//    ```systemverilog
//    assign expected_npc_o = npc_r;
//    ```
//    - This line assigns the value of `npc_r` to the output `expected_npc_o`.
// 
// 4. **Instruction Validity and NPC Mismatch Detection**:
//    ```systemverilog
//    wire instr_v = issue_pkt_cast_i.v & ~commit_pkt_cast_i.npc_w_v;
//    wire npc_mismatch_v = instr_v & (expected_npc_o != issue_pkt_cast_i.pc);
//    wire npc_match_v    = instr_v & (expected_npc_o == issue_pkt_cast_i.pc);
//    assign poison_isd_o = npc_mismatch_v;
//    ```
//    - `instr_v` is true if `issue_pkt_cast_i.v` is true 
// and `commit_pkt_cast_i.npc_w_v` is false.
//    - `npc_mismatch_v` is true if `instr_v` is true 
// and `expected_npc_o` does not match `issue_pkt_cast_i.pc`.
//    - `npc_match_v` is true if `instr_v` is true 
// and `expected_npc_o` matches `issue_pkt_cast_i.pc`.
//    - `poison_isd_o` is assigned the value of `npc_mismatch_v`, 
// indicating whether there is an NPC mismatch.
// 
// In summary, this code manages the next program counter (NPC) 
// and detects mismatches between the expected NPC 
// and the actual program counter from the issue packet. 
// If a mismatch is detected, it sets the `poison_isd_o` signal. 
// If you have any specific questions or need further clarification, feel free to ask!



//================================




  logic btaken_pending, attaboy_pending;
  bsg_dff_reset_set_clear
   #(.width_p(2), .clear_over_set_p(1))
   attaboy_pending_reg
    (.clk_i(clk_i)
     ,.reset_i(reset_i)

     ,.set_i({br_pkt_cast_i.btaken, br_pkt_cast_i.branch})
     ,.clear_i({instr_v, instr_v})
     ,.data_o({btaken_pending, attaboy_pending})
     );
  wire last_instr_was_branch = attaboy_pending | br_pkt_cast_i.branch;
  wire last_instr_was_btaken = btaken_pending  | br_pkt_cast_i.btaken;

  wire fe_cmd_nonattaboy_v = fe_cmd_v_li & (fe_cmd_li.opcode != e_op_attaboy);

  // Boot logic
  wire freeze_li = cfg_bus_cast_i.freeze | reset_i;
  always_comb
    begin
      unique casez (state_r)
        e_freeze
        ,e_fencei
        ,e_wait
        ,e_cmd_fence
        ,e_run  : state_n = commit_pkt_cast_i.wfi
                            ? e_wait
                            : commit_pkt_cast_i.fencei
                              ? e_fencei
                              : (commit_pkt_cast_i.resume | fe_cmd_nonattaboy_v)
                                ? e_cmd_fence
                                : freeze_li
                                  ? e_freeze
                                  : clear_iss_o
                                    ? e_run
                                    : state_r;
        // e_cmd_fence:
        default : state_n = cmd_empty_r_lo ? e_run : state_r;
      endcase
    end

  // synopsys sync_set_reset "reset_i"
  always_ff @(posedge clk_i)
    if (reset_i)
      state_r <= e_freeze;
    else
      state_r <= state_n;

  assign suppress_iss_o = !is_run;
  assign clear_iss_o    = is_cmd_fence & cmd_empty_r_lo;
  assign resume_o       = (is_freeze & ~freeze_li)
                          || (is_wait & irq_waiting_i)
                          || (is_fencei & ~mem_busy_i);

  always_comb
    begin
      fe_cmd_li = 'b0;
      fe_cmd_v_li = 1'b0;
      fe_cmd_pc_redirect_operands = '0;

      if (commit_pkt_cast_i.resume)
        begin
          fe_cmd_li.opcode = is_freeze ? e_op_state_reset : e_op_pc_redirection;
          fe_cmd_li.npc = npc_n;

          fe_cmd_pc_redirect_operands.priv           = commit_pkt_cast_i.priv_n;
          fe_cmd_pc_redirect_operands.translation_en = commit_pkt_cast_i.translation_en_n;
          fe_cmd_pc_redirect_operands.subopcode      = e_subop_resume;
          fe_cmd_li.operands.pc_redirect_operands    = fe_cmd_pc_redirect_operands;

          fe_cmd_v_li = ~cmd_full_r_lo;
        end
      else if (commit_pkt_cast_i.itlb_fill_v)
        begin
          fe_cmd_li.opcode                               = (compressed_support_p & commit_pkt_cast_i.partial) ? e_op_itlb_fill_resume : e_op_itlb_fill_restart;
          fe_cmd_li.npc                                  = commit_pkt_cast_i.vaddr;
          fe_cmd_li.operands.itlb_fill_response.pte_leaf = commit_pkt_cast_i.pte_leaf;
          fe_cmd_li.operands.itlb_fill_response.instr    = compressed_support_p ? commit_pkt_cast_i.instr : '0;

          fe_cmd_v_li = ~cmd_full_r_lo;
        end
      else if (commit_pkt_cast_i.sfence)
        begin
          fe_cmd_li.opcode = e_op_itlb_fence;
          fe_cmd_li.npc = commit_pkt_cast_i.npc;

          fe_cmd_v_li = ~cmd_full_r_lo;
        end
      else if (commit_pkt_cast_i.csrw)
        begin
          fe_cmd_li.opcode                            = e_op_pc_redirection;
          fe_cmd_li.npc                               = commit_pkt_cast_i.npc;
          fe_cmd_pc_redirect_operands.subopcode       = e_subop_translation_switch;
          fe_cmd_pc_redirect_operands.translation_en  = commit_pkt_cast_i.translation_en_n;
          fe_cmd_li.operands.pc_redirect_operands     = fe_cmd_pc_redirect_operands;

          fe_cmd_v_li = ~cmd_full_r_lo;
        end
      else if (commit_pkt_cast_i.wfi)
        begin
          fe_cmd_li.opcode = e_op_wait;
          fe_cmd_li.npc = commit_pkt_cast_i.npc;

          fe_cmd_v_li = ~cmd_full_r_lo;
        end
      else if (commit_pkt_cast_i.fencei)
        begin
          fe_cmd_li.opcode = e_op_icache_fence;
          fe_cmd_li.npc = commit_pkt_cast_i.npc;

          fe_cmd_v_li = !icache_features_p[e_cfg_coherent];
        end
      else if (commit_pkt_cast_i.icache_miss)
        begin
          fe_cmd_li.opcode = (compressed_support_p & commit_pkt_cast_i.partial) ? e_op_icache_fill_resume : e_op_icache_fill_restart;
          fe_cmd_li.npc    = commit_pkt_cast_i.vaddr;
          fe_cmd_li.operands.icache_fill_response.instr = compressed_support_p ? commit_pkt_cast_i.instr : '0;

          fe_cmd_v_li = ~cmd_full_r_lo;
        end
      else if (commit_pkt_cast_i.eret)
        begin
          fe_cmd_li.opcode                                 = e_op_pc_redirection;
          fe_cmd_li.npc                                    = commit_pkt_cast_i.npc;
          fe_cmd_pc_redirect_operands.subopcode            = e_subop_eret;
          fe_cmd_pc_redirect_operands.priv                 = commit_pkt_cast_i.priv_n;
          fe_cmd_pc_redirect_operands.translation_en       = commit_pkt_cast_i.translation_en_n;
          fe_cmd_li.operands.pc_redirect_operands          = fe_cmd_pc_redirect_operands;

          fe_cmd_v_li = ~cmd_full_r_lo;
        end
      else if (commit_pkt_cast_i.exception | commit_pkt_cast_i._interrupt)
        begin
          fe_cmd_li.opcode                                 = e_op_pc_redirection;
          fe_cmd_li.npc                                    = commit_pkt_cast_i.npc;
          fe_cmd_pc_redirect_operands.subopcode            = commit_pkt_cast_i.exception ? e_subop_trap : e_subop_interrupt;
          fe_cmd_pc_redirect_operands.priv                 = commit_pkt_cast_i.priv_n;
          fe_cmd_pc_redirect_operands.translation_en       = commit_pkt_cast_i.translation_en_n;
          fe_cmd_li.operands.pc_redirect_operands          = fe_cmd_pc_redirect_operands;

          fe_cmd_v_li = ~cmd_full_r_lo;
        end
      else if (npc_mismatch_v)
        begin
          fe_cmd_li.opcode                                 = e_op_pc_redirection;
          fe_cmd_li.npc                                    = expected_npc_o;
          fe_cmd_pc_redirect_operands.subopcode            = e_subop_branch_mispredict;
          fe_cmd_pc_redirect_operands.branch_metadata_fwd  = issue_pkt_cast_i.branch_metadata_fwd;
          fe_cmd_pc_redirect_operands.misprediction_reason = last_instr_was_branch
                                                             ? last_instr_was_btaken
                                                               ? e_incorrect_pred_taken
                                                               : e_incorrect_pred_ntaken
                                                             : e_not_a_branch;
          fe_cmd_li.operands.pc_redirect_operands          = fe_cmd_pc_redirect_operands;

          fe_cmd_v_li = ~cmd_full_r_lo;
        end
      // Send an attaboy if there's a correct prediction
      else if (npc_match_v & last_instr_was_branch)
        begin
          fe_cmd_li.opcode                               = e_op_attaboy;
          fe_cmd_li.npc                                  = expected_npc_o;
          fe_cmd_li.operands.attaboy.taken               = last_instr_was_btaken;
          fe_cmd_li.operands.attaboy.branch_metadata_fwd = issue_pkt_cast_i.branch_metadata_fwd;

          fe_cmd_v_li = ~cmd_full_r_lo;
        end
    end

  bp_be_cmd_queue
   #(.bp_params_p(bp_params_p))
   fe_cmd_fifo
    (.clk_i(clk_i)
     ,.reset_i(reset_i)

     ,.fe_cmd_i(fe_cmd_li)
     ,.fe_cmd_v_i(fe_cmd_v_li)

     ,.fe_cmd_o(fe_cmd_o)
     ,.fe_cmd_v_o(fe_cmd_v_o)
     ,.fe_cmd_yumi_i(fe_cmd_yumi_i)

     ,.empty_n_o(cmd_empty_n_lo)
     ,.empty_r_o(cmd_empty_r_lo)
     ,.full_n_o(cmd_full_n_lo)
     ,.full_r_o(cmd_full_r_lo)
     );
  assign cmd_full_n_o = cmd_full_n_lo;
  assign cmd_full_r_o = cmd_full_r_lo;

endmodule

