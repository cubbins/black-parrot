/**
 *
 * Name:
 *   bp_be_instr_decode.v
 *
 * Description:
 *   BlackParrot instruction decoder for translating RISC-V instructions into pipeline control
 *     signals. Currently supports most of rv64i with the exception of fences and csrs.
 *
 * Notes:
 *   We may want to break this up into a decoder for each standard extension.
 *   Each pipe may need different signals. Use a union in decode_s to save bits?
 */

`include "bp_common_defines.svh"
`include "bp_be_defines.svh"

module bp_be_instr_decoder
 import bp_common_pkg::*;
 import bp_be_pkg::*;
 #(parameter bp_params_e bp_params_p = e_bp_default_cfg
   `declare_bp_proc_params(bp_params_p)

   // Generated parameters
   , localparam decode_width_lp = $bits(bp_be_decode_s)
   , localparam decode_info_width_lp = `bp_be_decode_info_width
   , localparam preissue_pkt_width_lp = `bp_be_preissue_pkt_width
   )
  (input [preissue_pkt_width_lp-1:0]    preissue_pkt_i
   , input [decode_info_width_lp-1:0]   decode_info_i

   , output logic [decode_width_lp-1:0] decode_o
   , output logic                       illegal_instr_o
   , output logic                       ecall_m_o
   , output logic                       ecall_s_o
   , output logic                       ecall_u_o
   , output logic                       ebreak_o
   , output logic                       dbreak_o
   , output logic                       dret_o
   , output logic                       mret_o
   , output logic                       sret_o
   , output logic                       wfi_o
   , output logic                       sfence_vma_o
   , output logic                       fencei_o
   , output logic                       csrw_o

   , output logic [dword_width_gp-1:0]  imm_o
   );

  `declare_bp_be_internal_if_structs(vaddr_width_p, paddr_width_p, asid_width_p, branch_metadata_fwd_width_p);
  `bp_cast_i(bp_be_preissue_pkt_s, preissue_pkt);
  `bp_cast_i(bp_be_decode_info_s, decode_info);
  `bp_cast_o(bp_be_decode_s, decode);

  rv64_instr_fmatype_s instr;
  assign instr = preissue_pkt_cast_i.instr;

  // Decode logic
  always_comb
    begin
      decode_cast_o = '0;
      decode_cast_o.compressed = preissue_pkt_cast_i.compressed;

      decode_cast_o.irs1_r_v = preissue_pkt_cast_i.irs1_v;
      decode_cast_o.irs2_r_v = preissue_pkt_cast_i.irs2_v;
      decode_cast_o.frs1_r_v = preissue_pkt_cast_i.frs1_v;
      decode_cast_o.frs2_r_v = preissue_pkt_cast_i.frs2_v;
      decode_cast_o.frs3_r_v = preissue_pkt_cast_i.frs3_v;

      illegal_instr_o = '0;
      ecall_m_o       = '0;
      ecall_s_o       = '0;
      ecall_u_o       = '0;
      ebreak_o        = '0;
      dbreak_o        = '0;
      dret_o          = '0;
      mret_o          = '0;
      sret_o          = '0;
      wfi_o           = '0;
      sfence_vma_o    = '0;
      fencei_o        = '0;
      csrw_o          = '0;

      imm_o           = '0;



      unique casez (instr.opcode)
        `RV64_OP_OP, `RV64_OP_32_OP:
          begin
            if (instr inside {`RV64_MUL, `RV64_MULW})
begin
              decode_cast_o.pipe_mul_v = 1'b1;
          //$display("bp_be_instr_decoder line 92 - Instruction: %0s - Setting pipe_mul_v", instr.opcode.name());


end

            else if (instr inside {`RV64_MULH, `RV64_MULHSU, `RV64_MULHU
                                   ,`RV64_DIV, `RV64_DIVU, `RV64_DIVW, `RV64_DIVUW
                                   ,`RV64_REM, `RV64_REMU, `RV64_REMW, `RV64_REMUW
                                   })
              begin
                decode_cast_o.pipe_long_v = 1'b1;
                decode_cast_o.score_v     = 1'b1;
              end
            else
              decode_cast_o.pipe_int_v = 1'b1;

            // The writeback for long latency ops comes out of band
            decode_cast_o.irf_w_v   = (instr.rd_addr != '0);
            if (instr.opcode == `RV64_OP_32_OP)
              decode_cast_o.int_tag = e_int_word;

            if (instr inside {`RV64_MULHU, `RV64_DIVU, `RV64_DIVUW, `RV64_REMU, `RV64_REMUW})
              decode_cast_o.rs1_unsigned = 1'b1;

            if (instr inside {`RV64_MULHSU, `RV64_MULHU, `RV64_DIVU, `RV64_DIVUW, `RV64_REMU, `RV64_REMUW})
              decode_cast_o.rs2_unsigned = 1'b1;

            unique casez (instr)
              `RV64_ADD, `RV64_ADDW : decode_cast_o.fu_op = e_int_op_add;
              `RV64_SUB, `RV64_SUBW : decode_cast_o.fu_op = e_int_op_sub;
              `RV64_SLL, `RV64_SLLW : decode_cast_o.fu_op = e_int_op_sll;
              `RV64_SRL, `RV64_SRLW : decode_cast_o.fu_op = e_int_op_srl;
              `RV64_SRA, `RV64_SRAW : decode_cast_o.fu_op = e_int_op_sra;
              `RV64_SLT             : decode_cast_o.fu_op = e_int_op_slt;
              `RV64_SLTU            : decode_cast_o.fu_op = e_int_op_sltu;
              `RV64_XOR             : decode_cast_o.fu_op = e_int_op_xor;
              `RV64_OR              : decode_cast_o.fu_op = e_int_op_or;
              `RV64_AND             : decode_cast_o.fu_op = e_int_op_and;

              `RV64_MUL, `RV64_MULW   : decode_cast_o.fu_op = e_fma_op_imul;
              `RV64_MULH              : decode_cast_o.fu_op = e_long_op_mulh;
              `RV64_MULHSU            : decode_cast_o.fu_op = e_long_op_mulhsu;
              `RV64_MULHU             : decode_cast_o.fu_op = e_long_op_mulhu;
              `RV64_DIV, `RV64_DIVW   : decode_cast_o.fu_op = e_long_op_div;
              `RV64_DIVU, `RV64_DIVUW : decode_cast_o.fu_op = e_long_op_divu;
              `RV64_REM, `RV64_REMW   : decode_cast_o.fu_op = e_long_op_rem;
              `RV64_REMU, `RV64_REMUW : decode_cast_o.fu_op = e_long_op_remu;
              default : illegal_instr_o = 1'b1;
            endcase

            decode_cast_o.src1_sel   = e_src1_is_rs1;
            decode_cast_o.src2_sel   = e_src2_is_rs2;
          end
        `RV64_OP_IMM_OP, `RV64_OP_IMM_32_OP:
          begin
            decode_cast_o.pipe_int_v = 1'b1;
            decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
            if (instr.opcode == `RV64_OP_IMM_32_OP)
              decode_cast_o.int_tag = e_int_word;
            unique casez (instr)
              `RV64_ADDI, `RV64_ADDIW : decode_cast_o.fu_op = e_int_op_add;
              `RV64_SLLI, `RV64_SLLIW : decode_cast_o.fu_op = e_int_op_sll;
              `RV64_SRLI, `RV64_SRLIW : decode_cast_o.fu_op = e_int_op_srl;
              `RV64_SRAI, `RV64_SRAIW : decode_cast_o.fu_op = e_int_op_sra;
              `RV64_SLTI              : decode_cast_o.fu_op = e_int_op_slt;
              `RV64_SLTIU             : decode_cast_o.fu_op = e_int_op_sltu;
              `RV64_XORI              : decode_cast_o.fu_op = e_int_op_xor;
              `RV64_ORI               : decode_cast_o.fu_op = e_int_op_or;
              `RV64_ANDI              : decode_cast_o.fu_op = e_int_op_and;
              default : illegal_instr_o = 1'b1;
            endcase

            decode_cast_o.src1_sel   = e_src1_is_rs1;
            decode_cast_o.src2_sel   = e_src2_is_imm;
          end
        `RV64_LUI_OP:
          begin
            decode_cast_o.pipe_int_v = 1'b1;
            decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
            decode_cast_o.fu_op      = e_int_op_pass_src2;
            decode_cast_o.src2_sel   = e_src2_is_imm;
          end
        `RV64_AUIPC_OP:
          begin
            decode_cast_o.pipe_int_v = 1'b1;
            decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
            decode_cast_o.fu_op      = e_int_op_add;
            decode_cast_o.src1_sel   = e_src1_is_pc;
            decode_cast_o.src2_sel   = e_src2_is_imm;
          end
        `RV64_JAL_OP:
          begin
            decode_cast_o.pipe_int_v = 1'b1;
            decode_cast_o.branch_v   = 1'b1;
            decode_cast_o.jump_v     = 1'b1;
            decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
            decode_cast_o.fu_op      = e_int_op_pass_one;
            decode_cast_o.baddr_sel  = e_baddr_is_pc;
          end
        `RV64_JALR_OP:
          begin
            decode_cast_o.pipe_int_v = 1'b1;
            decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
            decode_cast_o.branch_v   = 1'b1;
            decode_cast_o.jump_v     = 1'b1;
            decode_cast_o.fu_op      = e_int_op_pass_one;
            decode_cast_o.baddr_sel  = e_baddr_is_rs1;

            illegal_instr_o = ~(instr inside {`RV64_JALR});
          end
        `RV64_BRANCH_OP:
          begin
            decode_cast_o.pipe_int_v = 1'b1;
            decode_cast_o.branch_v   = 1'b1;
            unique casez (instr)
              `RV64_BEQ  : decode_cast_o.fu_op = e_int_op_eq;
              `RV64_BNE  : decode_cast_o.fu_op = e_int_op_ne;
              `RV64_BLT  : decode_cast_o.fu_op = e_int_op_slt;
              `RV64_BGE  : decode_cast_o.fu_op = e_int_op_sge;
              `RV64_BLTU : decode_cast_o.fu_op = e_int_op_sltu;
              `RV64_BGEU : decode_cast_o.fu_op = e_int_op_sgeu;
              default : illegal_instr_o = 1'b1;
            endcase
            decode_cast_o.baddr_sel  = e_baddr_is_pc;
          end






        `RV64_LOAD_OP:
          begin
            decode_cast_o.pipe_mem_early_v = 1'b1;
            decode_cast_o.irf_w_v          = (instr.rd_addr != '0);
            decode_cast_o.spec_w_v         = 1'b1;
            decode_cast_o.score_v          = 1'b1;
            decode_cast_o.dcache_r_v       = 1'b1;
            decode_cast_o.mem_v            = 1'b1;
            unique casez (instr)
              `RV64_LB : decode_cast_o.fu_op = e_dcache_op_lb;
              `RV64_LH : decode_cast_o.fu_op = e_dcache_op_lh;
              `RV64_LW : decode_cast_o.fu_op = e_dcache_op_lw;
              `RV64_LBU: decode_cast_o.fu_op = e_dcache_op_lbu;
              `RV64_LHU: decode_cast_o.fu_op = e_dcache_op_lhu;
              `RV64_LWU: decode_cast_o.fu_op = e_dcache_op_lwu;
              `RV64_LD : decode_cast_o.fu_op = e_dcache_op_ld;
              default : illegal_instr_o = 1'b1;
            endcase



unique casez (instr)
  `RV64_LB : begin
    decode_cast_o.fu_op = e_dcache_op_lb;
    $write("lb rd, ");
    //$write("Immediate value (imm_o): %h\n", imm_o);
  end
  `RV64_LH : begin
    decode_cast_o.fu_op = e_dcache_op_lh;
    $write("lh rd, ");
    //$write("Immediate value (imm_o): %h\n", imm_o);
  end

//lw rd, imm(rs1)

  `RV64_LW : begin
    decode_cast_o.fu_op = e_dcache_op_lw;
    $write("lw rd, ");
    //$write("e_dcache_op_lw has been found\n");
    //$write("Immediate value (imm_o): %h\n", imm_o);
  end
  `RV64_LBU: begin
    decode_cast_o.fu_op = e_dcache_op_lbu;
    $write("lbu rd, ");
    //$write("Immediate value (imm_o): %h\n", imm_o);
  end
  `RV64_LHU: begin
    decode_cast_o.fu_op = e_dcache_op_lhu;
    $write("lhu rd, ");
    //$write("Immediate value (imm_o): %h\n", imm_o);
  end


  //ld rd, imm(rs1)

  `RV64_LD : begin
    decode_cast_o.fu_op = e_dcache_op_ld;
    $write("ld rd, ");
    //$write("Immediate value (imm_o): %h\n", imm_o);
  end
  default : begin
    illegal_instr_o = 1'b1;
    $write("Illegal instr ");
    //$write("Immediate value (imm_o): %h\n", imm_o);
  end
endcase

// First, write out the initial state of decode_cast_o
//$write("Initial decode_cast_o: %p\n", decode_cast_o);


case (instr.opcode)
  `RV64_LUI_OP, `RV64_AUIPC_OP:
    $write("U-type imm %h", `rv64_signext_u_imm(instr));
  `RV64_JAL_OP:
    $write("J-type imm %h", `rv64_signext_j_imm(instr));
  `RV64_BRANCH_OP:
    $write("B-type imm %h", `rv64_signext_b_imm(instr));
  `RV64_STORE_OP, `RV64_FSTORE_OP:
    $write("S-type imm %h", `rv64_signext_s_imm(instr));
  `RV64_JALR_OP, `RV64_LOAD_OP, `RV64_OP_IMM_OP, `RV64_OP_IMM_32_OP, `RV64_FLOAD_OP:
    $write("I-type imm %h", `rv64_signext_i_imm(instr));
  default:
    $write("Default case, no immediate extracted\n");
endcase




// Write out the register read valid flags
if (decode_cast_o.compressed)
  $write(" comp: %b", decode_cast_o.compressed);

if (decode_cast_o.irs1_r_v) $write(" irs1 \n");

  //$write("decode_cast_o.irs1_r_v: %b\n", decode_cast_o.irs1_r_v);

if (decode_cast_o.irs2_r_v) $write(" irs2 \n");

  //$write("decode_cast_o.irs2_r_v: %b\n", decode_cast_o.irs2_r_v);

if (decode_cast_o.frs1_r_v) $write(" frs1 \n");

  //$write("decode_cast_o.frs1_r_v: %b\n", decode_cast_o.frs1_r_v);

if (decode_cast_o.frs2_r_v) $write(" frs2 \n");

  //$write("decode_cast_o.frs2_r_v: %b\n", decode_cast_o.frs2_r_v);

if (decode_cast_o.frs3_r_v) $write(" frs3 \n");

  //$write("decode_cast_o.frs3_r_v: %b\n", decode_cast_o.frs3_r_v);








 end //the true end of the above case statement

// Yes, I understand this code snippet. It appears to be a part of a decoder for RISC-V 64-bit (RV64) instructions, specifically handling load operations. Here's a breakdown of what this code does:
// 
// 1. It's a case for `RV64_LOAD_OP`, which likely represents all load operations in the RV64 instruction set.
// 
// 2. Inside this case, it sets various control signals:
//    - `pipe_mem_early_v` is set to 1, possibly indicating an early memory operation.
//    - `irf_w_v` is set to 1 if the destination register (rd) is not r0 (zero register).
//    - `spec_w_v`, `score_v`, `dcache_r_v`, and `mem_v` are all set to 1, likely enabling speculative execution, scoreboarding, data cache read, and memory operation respectively.
// 
// 3. It then uses a `unique casez` statement to further decode the specific load instruction:
//    - `LB`: Load Byte
//    - `LH`: Load Halfword
//    - `LW`: Load Word
//    - `LBU`: Load Byte Unsigned
//    - `LHU`: Load Halfword Unsigned
//    - `LWU`: Load Word Unsigned
//    - `LD`: Load Doubleword
// 
// 4. For each of these instructions, it sets the `fu_op` (functional unit operation) to a corresponding operation code (e.g., `e_dcache_op_lb` for Load Byte).
// 
// 5. If the instruction doesn't match any of these load operations, it sets `illegal_instr_o` to 1, indicating an illegal instruction.
// 
// This code is likely part of a larger instruction decoder in a RISC-V processor implementation, possibly written in Verilog or SystemVerilog.

// The `decode_cast_o.pipe_mem_early_v` signal, set to 1'b1 in the provided code snippet, likely indicates an early memory operation in the instruction pipeline. This signal is part of the decoding process for RISC-V 64-bit (RV64) load operations[1].
// 
// In the context of instruction pipelining, this signal might be used to initiate memory-related operations earlier in the pipeline stages. Pipelining is a technique used in processor design to improve instruction throughput by dividing the instruction execution into several stages that can be executed in parallel[3].
// 
// For load operations, which require memory access, setting `pipe_mem_early_v` could allow the processor to start preparing for the memory access stage earlier in the pipeline. This early indication might help in:
// 
// 1. Reducing pipeline stalls by initiating memory access preparations sooner.
// 2. Optimizing memory access timing within the pipeline stages.
// 3. Potentially overlapping memory operations with other pipeline stages for improved efficiency.
// 
// While the exact implementation details may vary depending on the specific processor design, the `pipe_mem_early_v` signal is likely part of the processor's strategy to optimize memory operations within its pipelined architecture[1][3].
// 
// Citations:
// [1] https://docs.kernel.org/userspace-api/media/v4l/dev-decoder.html
// [2] https://developers.google.com/mpact-sim/guides/riscv_binary_decoder
// [3] https://en.wikipedia.org/wiki/Instruction_pipelining
// [4] https://d2l.ai/chapter_recurrent-modern/encoder-decoder.html
// [5] https://trac.ffmpeg.org/wiki/FFprobeTips
// [6] https://www.gnu.org/software/libc/manual/html_node/Basic-Signal-Handling.html
// [7] https://www.intel.com/content/www/us/en/docs/programmable/683667/23-4/pipe-interface.html
// [8] https://docs.rs/ffav-sys/latest/ffav_sys/struct.AVCodecContext.html

// In the context of RISC-V 64-bit (RV64) load operations, setting `decode_cast_o.score_v` to 1'b1 likely indicates that the instruction should be scored or tracked by the processor's scoreboarding mechanism.
// 
// Scoreboarding is a technique used in computer architecture to dynamically schedule instructions and manage data hazards. By setting this signal, the processor is marking the load instruction as one that needs to be monitored for dependencies and execution status. This is particularly important for load operations because:
// 
// 1. Load instructions may have longer latencies due to memory access.
// 2. They can potentially cause data hazards with subsequent instructions that depend on the loaded value.
// 3. Out-of-order execution may require careful tracking of load instructions to maintain program correctness.
// 
// By enabling scoreboarding for load operations, the processor can:
// 
// - Track the progress of the load instruction through the pipeline stages.
// - Prevent dependent instructions from executing prematurely.
// - Optimize instruction scheduling by allowing independent instructions to proceed while waiting for the load to complete.
// 
// This mechanism helps maintain the correct execution order and data consistency in the processor pipeline, especially when dealing with memory operations that may have variable latencies.
// 
// Citations:
// [1] https://docs.rs/ffav-sys/latest/ffav_sys/struct.AVCodecContext.html
// [2] https://cloud.google.com/speech-to-text/docs/reference/rest/v1/RecognitionConfig
// [3] https://developer.nvidia.com/blog/categorical-features-in-xgboost-without-manual-encoding/
// [4] https://docs.ray.io/en/latest/train/examples/transformers/huggingface_text_classification.html
// [5] https://d2l.ai/chapter_recurrent-modern/encoder-decoder.html
// [6] https://trac.ffmpeg.org/wiki/FFprobeTips
// [7] https://aomediacodec.github.io/av1-spec/
// [8] https://huggingface.co/nvidia/NV-Embed-v2/blob/main/README.md



        `RV64_FLOAD_OP:
          begin
            decode_cast_o.pipe_mem_final_v = 1'b1;
            decode_cast_o.frf_w_v          = 1'b1;
            decode_cast_o.spec_w_v         = 1'b1;
            decode_cast_o.score_v          = 1'b1;
            decode_cast_o.dcache_r_v       = 1'b1;
            decode_cast_o.mem_v            = 1'b1;
            if (instr inside {`RV64_FL_W})
              decode_cast_o.fp_tag = e_fp_sp;

            illegal_instr_o = ~decode_info_cast_i.fpu_en;

            unique casez (instr)
              `RV64_FL_W: decode_cast_o.fu_op = e_dcache_op_flw;
              `RV64_FL_D: decode_cast_o.fu_op = e_dcache_op_fld;
              default: illegal_instr_o = 1'b1;
            endcase
          end
        `RV64_STORE_OP:
          begin
            decode_cast_o.pipe_mem_early_v = 1'b1;
            decode_cast_o.dcache_w_v = 1'b1;
            decode_cast_o.mem_v      = 1'b1;
            unique casez (instr)
              `RV64_SB : decode_cast_o.fu_op = e_dcache_op_sb;
              `RV64_SH : decode_cast_o.fu_op = e_dcache_op_sh;
              `RV64_SW : decode_cast_o.fu_op = e_dcache_op_sw;
              `RV64_SD : decode_cast_o.fu_op = e_dcache_op_sd;
              default : illegal_instr_o = 1'b1;
            endcase
          end
        `RV64_FSTORE_OP:
          begin
            decode_cast_o.pipe_mem_early_v = 1'b1;
            decode_cast_o.dcache_w_v       = 1'b1;
            decode_cast_o.mem_v            = 1'b1;
            decode_cast_o.fp_raw           = 1'b1;
            //if (instr inside {`RV64_FS_W})
            //  decode_cast_o.fp_tag = e_fp_sp;

            illegal_instr_o = ~decode_info_cast_i.fpu_en;

            unique casez (instr)
              `RV64_FS_W : decode_cast_o.fu_op = e_dcache_op_fsw;
              `RV64_FS_D : decode_cast_o.fu_op = e_dcache_op_fsd;
              default: illegal_instr_o = 1'b1;
            endcase
          end
        `RV64_MISC_MEM_OP:
          begin
            unique casez (instr)
              `RV64_FENCE   :
                begin
                  decode_cast_o.fence_v = 1'b1;
                end
              `RV64_FENCE_I :
                begin
                  decode_cast_o.fence_v          = 1'b1;
                  decode_cast_o.pipe_mem_early_v = !dcache_features_p[e_cfg_coherent];
                  decode_cast_o.fu_op            = e_dcache_op_clean;
                  fencei_o                       = 1'b1;
                end
              `RV64_CMO_INVAL_ALL:
                begin
                  decode_cast_o.pipe_mem_early_v = 1'b1;
                  decode_cast_o.fu_op            = e_dcache_op_inval;
                  // TODO: Implement for multicore
                  illegal_instr_o = dcache_features_p[e_cfg_coherent];
                end
              `RV64_CMO_CLEAN_ALL:
                begin
                  decode_cast_o.pipe_mem_early_v = 1'b1;
                  decode_cast_o.fu_op            = e_dcache_op_clean;
                  // TODO: Implement for multicore
                  illegal_instr_o = dcache_features_p[e_cfg_coherent];
                end
              `RV64_CMO_FLUSH_ALL:
                begin
                  decode_cast_o.pipe_mem_early_v = 1'b1;
                  decode_cast_o.fu_op            = e_dcache_op_flush;
                  // TODO: Implement for multicore
                  illegal_instr_o = dcache_features_p[e_cfg_coherent];
                end
              `RV64_CBO_ZERO:
                begin
                  decode_cast_o.pipe_mem_early_v = 1'b1;
                  decode_cast_o.dcache_cbo_v     = 1'b1;
                  decode_cast_o.dcache_w_v       = 1'b1;
                  decode_cast_o.fu_op            = e_dcache_op_bzero;
                end
              `RV64_CBO_CLEAN:
                begin
                  decode_cast_o.pipe_mem_early_v = 1'b1;
                  decode_cast_o.dcache_cbo_v     = 1'b1;
                  decode_cast_o.fu_op            = e_dcache_op_bclean;
                  // TODO: Implement for ucode
                  illegal_instr_o = (cce_type_p == e_cce_ucode);
                end
              `RV64_CBO_INVAL:
                begin
                  decode_cast_o.pipe_mem_early_v = 1'b1;
                  decode_cast_o.dcache_cbo_v     = 1'b1;
                  decode_cast_o.fu_op            = e_dcache_op_binval;
                  // TODO: Implement for ucode
                  illegal_instr_o = (cce_type_p == e_cce_ucode);
                end
              `RV64_CBO_FLUSH:
                begin
                  decode_cast_o.pipe_mem_early_v = 1'b1;
                  decode_cast_o.dcache_cbo_v     = 1'b1;
                  decode_cast_o.fu_op            = e_dcache_op_bflush;
                  // TODO: Implement for ucode
                  illegal_instr_o = (cce_type_p == e_cce_ucode);
                end
              `RV64_CMO_PREFETCHI:
                begin
                  // NOP for now
                end
              `RV64_CMO_PREFETCHR:
                begin
                  // NOP for now
                end
              `RV64_CMO_PREFETCHW:
                begin
                  // NOP for now
                end
              default : illegal_instr_o = 1'b1;
            endcase
          end
        `RV64_SYSTEM_OP:
          begin
            decode_cast_o.pipe_sys_v = 1'b1;
            unique casez (instr)
              `RV64_ECALL:
                begin
                  ecall_m_o = decode_info_cast_i.m_mode;
                  ecall_s_o = decode_info_cast_i.s_mode;
                  ecall_u_o = decode_info_cast_i.u_mode;
                end
              `RV64_EBREAK:
                begin
                  dbreak_o = decode_info_cast_i.debug_mode
                            | (decode_info_cast_i.ebreakm & decode_info_cast_i.m_mode)
                            | (decode_info_cast_i.ebreaks & decode_info_cast_i.s_mode)
                            | (decode_info_cast_i.ebreaku & decode_info_cast_i.u_mode);
                  ebreak_o = ~dbreak_o;
                end
              `RV64_DRET:
                begin
                  illegal_instr_o = ~decode_info_cast_i.debug_mode;
                  dret_o = ~illegal_instr_o;
                end
              `RV64_MRET:
                begin
                  illegal_instr_o = (decode_info_cast_i.s_mode | decode_info_cast_i.u_mode);
                  mret_o = ~illegal_instr_o;
                end
              `RV64_SRET:
                begin
                  illegal_instr_o = decode_info_cast_i.u_mode | (decode_info_cast_i.tsr & decode_info_cast_i.s_mode);
                  sret_o = ~illegal_instr_o;
                end
              `RV64_WFI:
                begin
                  // WFI operates as NOP in debug mode
                  illegal_instr_o = decode_info_cast_i.tw;
                  wfi_o = ~illegal_instr_o & ~decode_info_cast_i.debug_mode;
                end
              `RV64_SFENCE_VMA:
                begin
                  decode_cast_o.fence_v = 1'b1;
                  illegal_instr_o = (decode_info_cast_i.s_mode & decode_info_cast_i.tvm) | decode_info_cast_i.u_mode;
                  sfence_vma_o = ~illegal_instr_o;
                end
              `RV64_CSRRW, `RV64_CSRRWI, `RV64_CSRRS, `RV64_CSRRSI, `RV64_CSRRC, `RV64_CSRRCI:
                begin
                  decode_cast_o.csr_w_v = instr inside {`RV64_CSRRW, `RV64_CSRRWI} || (instr.rs1_addr != '0);
                  decode_cast_o.csr_r_v = ~(instr inside {`RV64_CSRRW, `RV64_CSRRWI}) || (instr.rd_addr != '0);
                  decode_cast_o.irf_w_v = (instr.rd_addr != '0);
                  csrw_o = decode_cast_o.csr_w_v;

                  casez (instr[31-:12])
                    `CSR_ADDR_FCSR
                    ,`CSR_ADDR_FFLAGS
                    ,`CSR_ADDR_FRM      : illegal_instr_o = !decode_info_cast_i.fpu_en;
                    `CSR_ADDR_CYCLE     : illegal_instr_o = !decode_info_cast_i.cycle_en;
                    `CSR_ADDR_INSTRET   : illegal_instr_o = !decode_info_cast_i.instret_en;
                    `CSR_ADDR_SATP      : illegal_instr_o = decode_info_cast_i.s_mode & decode_info_cast_i.tvm;
                    {12'b11??_????_????}: illegal_instr_o = csrw_o;
                    {12'b??01_????_????}: illegal_instr_o = decode_info_cast_i.u_mode;
                    {12'b??10_????_????}: illegal_instr_o = decode_info_cast_i.s_mode | decode_info_cast_i.u_mode;
                    {12'b??11_????_????}: illegal_instr_o = decode_info_cast_i.s_mode | decode_info_cast_i.u_mode;
                  endcase
                end
              default: illegal_instr_o = 1'b1;
            endcase
          end
        `RV64_FP_OP:
          begin
            illegal_instr_o = ~decode_info_cast_i.fpu_en;

            if (instr inside {`RV64_FCVT_DS, `RV64_FCVT_WS, `RV64_FCVT_LS, `RV64_FCVT_WUS, `RV64_FCVT_LUS
                              ,`RV64_FCVT_SW, `RV64_FCVT_SL, `RV64_FCVT_SWU, `RV64_FCVT_SLU
                              ,`RV64_FSGNJ_S, `RV64_FSGNJN_S, `RV64_FSGNJX_S
                              ,`RV64_FMIN_S, `RV64_FMAX_S, `RV64_FEQ_S, `RV64_FLT_S, `RV64_FLE_S, `RV64_FCLASS_S
                              ,`RV64_FADD_S, `RV64_FSUB_S, `RV64_FMUL_S, `RV64_FDIV_S, `RV64_FSQRT_S
                              })
              decode_cast_o.fp_tag = e_fp_sp;

            if (instr inside {`RV64_FCVT_WS, `RV64_FCVT_WUS, `RV64_FCVT_SW, `RV64_FCVT_SWU
                              ,`RV64_FCVT_WD, `RV64_FCVT_WUD, `RV64_FCVT_DW, `RV64_FCVT_DWU
                              ,`RV64_FMV_XW, `RV64_FMV_WX
                              })
              decode_cast_o.int_tag = e_int_word;

            unique casez (instr)
              `RV64_FCVT_SD, `RV64_FCVT_DS:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fu_op        = e_aux_op_f2f;
                end
              `RV64_FCVT_WS, `RV64_FCVT_LS, `RV64_FCVT_WD, `RV64_FCVT_LD:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
                  decode_cast_o.fu_op        = e_aux_op_f2i;
                end
              `RV64_FCVT_WUS, `RV64_FCVT_LUS, `RV64_FCVT_WUD, `RV64_FCVT_LUD:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
                  decode_cast_o.fu_op        = e_aux_op_f2iu;
                end
              `RV64_FCVT_SW, `RV64_FCVT_SL, `RV64_FCVT_DW, `RV64_FCVT_DL:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fu_op        = e_aux_op_i2f;
                end
              `RV64_FCVT_SWU, `RV64_FCVT_SLU, `RV64_FCVT_DWU, `RV64_FCVT_DLU:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.rs1_unsigned = 1'b1;
                  decode_cast_o.fu_op        = e_aux_op_iu2f;
                end
              `RV64_FMV_XW, `RV64_FMV_XD:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.irf_w_v      = (instr.rd_addr != '0);
                  decode_cast_o.fp_raw       = 1'b1;
                  decode_cast_o.fu_op        = e_aux_op_fmvi;
                end
              `RV64_FMV_WX, `RV64_FMV_DX:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fp_raw       = 1'b1;
                  decode_cast_o.fu_op        = e_aux_op_imvf;
                end
              `RV64_FSGNJ_S, `RV64_FSGNJ_D:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fp_raw       = 1'b1;
                  decode_cast_o.fu_op        = e_aux_op_fsgnj;
                end
              `RV64_FSGNJN_S, `RV64_FSGNJN_D:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fp_raw       = 1'b1;
                  decode_cast_o.fu_op        = e_aux_op_fsgnjn;
                end
              `RV64_FSGNJX_S, `RV64_FSGNJX_D:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fp_raw       = 1'b1;
                  decode_cast_o.fu_op        = e_aux_op_fsgnjx;
                end
              `RV64_FMIN_S, `RV64_FMIN_D:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fu_op        = e_aux_op_fmin;
                end
              `RV64_FMAX_S, `RV64_FMAX_D:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fu_op        = e_aux_op_fmax;
                end
              `RV64_FEQ_S, `RV64_FEQ_D:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
                  decode_cast_o.fu_op        = e_aux_op_feq;
                end
              `RV64_FLT_S, `RV64_FLT_D:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
                  decode_cast_o.fu_op        = e_aux_op_flt;
                end
              `RV64_FLE_S, `RV64_FLE_D:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
                  decode_cast_o.fu_op        = e_aux_op_fle;
                end
              `RV64_FCLASS_S, `RV64_FCLASS_D:
                begin
                  decode_cast_o.pipe_aux_v   = 1'b1;
                  decode_cast_o.irf_w_v    = (instr.rd_addr != '0);
                  decode_cast_o.fu_op        = e_aux_op_fclass;
                end
              `RV64_FADD_S, `RV64_FADD_D:
                begin
                  decode_cast_o.pipe_fma_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fu_op        = e_fma_op_fadd;
                end
              `RV64_FSUB_S, `RV64_FSUB_D:
                begin
                  decode_cast_o.pipe_fma_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fu_op        = e_fma_op_fsub;
                end
              `RV64_FMUL_S, `RV64_FMUL_D:
                begin
                  decode_cast_o.pipe_fma_v   = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.fu_op        = e_fma_op_fmul;
                end
              `RV64_FDIV_S, `RV64_FDIV_D:
                begin
                  decode_cast_o.pipe_long_v   = 1'b1;
                  decode_cast_o.frf_w_v       = 1'b1;
                  decode_cast_o.score_v       = 1'b1;
                  decode_cast_o.fu_op         = e_long_op_fdiv;
                end
              `RV64_FSQRT_S, `RV64_FSQRT_D:
                begin
                  decode_cast_o.pipe_long_v  = 1'b1;
                  decode_cast_o.frf_w_v      = 1'b1;
                  decode_cast_o.score_v      = 1'b1;
                  decode_cast_o.fu_op        = e_long_op_fsqrt;
                end
              default: illegal_instr_o = 1'b1;
            endcase
          end


        `RV64_FMADD_OP, `RV64_FMSUB_OP, `RV64_FNMSUB_OP, `RV64_FNMADD_OP:
          begin
            decode_cast_o.pipe_fma_v = 1'b1;
            decode_cast_o.frf_w_v    = 1'b1;
            if (instr.fmt == e_fmt_single)
              decode_cast_o.fp_tag = e_fp_sp;

            casez (instr.opcode)
              `RV64_FMADD_OP : decode_cast_o.fu_op = e_fma_op_fmadd;
              `RV64_FMSUB_OP : decode_cast_o.fu_op = e_fma_op_fmsub;
              `RV64_FNMSUB_OP: decode_cast_o.fu_op = e_fma_op_fnmsub;
              `RV64_FNMADD_OP: decode_cast_o.fu_op = e_fma_op_fnmadd;
              default: decode_cast_o.fu_op = e_fma_op_fmadd;
            endcase

            illegal_instr_o = ~decode_info_cast_i.fpu_en;
          end

        `RV64_AMO_OP:
          begin
            decode_cast_o.pipe_mem_early_v = 1'b1;
            decode_cast_o.irf_w_v          = (instr.rd_addr != '0);
            decode_cast_o.spec_w_v         = 1'b1;
            decode_cast_o.score_v          = 1'b1;
            decode_cast_o.dcache_r_v       =  (instr inside {`RV64_LRD, `RV64_LRW});
            decode_cast_o.dcache_w_v       = ~(instr inside {`RV64_LRD, `RV64_LRW});
            decode_cast_o.mem_v            = 1'b1;
            // Note: could do a more efficent decoding here by having atomic be a flag
            //   And having the op simply taken from funct3
            unique casez (instr)
              `RV64_LRD      : decode_cast_o.fu_op = e_dcache_op_lrd;
              `RV64_LRW      : decode_cast_o.fu_op = e_dcache_op_lrw;
              `RV64_SCD      : decode_cast_o.fu_op = e_dcache_op_scd;
              `RV64_SCW      : decode_cast_o.fu_op = e_dcache_op_scw;
              `RV64_AMOSWAPD : decode_cast_o.fu_op = e_dcache_op_amoswapd;
              `RV64_AMOSWAPW : decode_cast_o.fu_op = e_dcache_op_amoswapw;
              `RV64_AMOADDD  : decode_cast_o.fu_op = e_dcache_op_amoaddd;
              `RV64_AMOADDW  : decode_cast_o.fu_op = e_dcache_op_amoaddw;
              `RV64_AMOXORD  : decode_cast_o.fu_op = e_dcache_op_amoxord;
              `RV64_AMOXORW  : decode_cast_o.fu_op = e_dcache_op_amoxorw;
              `RV64_AMOANDD  : decode_cast_o.fu_op = e_dcache_op_amoandd;
              `RV64_AMOANDW  : decode_cast_o.fu_op = e_dcache_op_amoandw;
              `RV64_AMOORD   : decode_cast_o.fu_op = e_dcache_op_amoord;
              `RV64_AMOORW   : decode_cast_o.fu_op = e_dcache_op_amoorw;
              `RV64_AMOMIND  : decode_cast_o.fu_op = e_dcache_op_amomind;
              `RV64_AMOMINW  : decode_cast_o.fu_op = e_dcache_op_amominw;
              `RV64_AMOMAXD  : decode_cast_o.fu_op = e_dcache_op_amomaxd;
              `RV64_AMOMAXW  : decode_cast_o.fu_op = e_dcache_op_amomaxw;
              `RV64_AMOMINUD : decode_cast_o.fu_op = e_dcache_op_amominud;
              `RV64_AMOMINUW : decode_cast_o.fu_op = e_dcache_op_amominuw;
              `RV64_AMOMAXUD : decode_cast_o.fu_op = e_dcache_op_amomaxud;
              `RV64_AMOMAXUW : decode_cast_o.fu_op = e_dcache_op_amomaxuw;
              default : illegal_instr_o = 1'b1;
            endcase

            // Detect AMO support level
            unique casez (instr)
              `RV64_LRD, `RV64_LRW, `RV64_SCD, `RV64_SCW:
                illegal_instr_o =
                  ~|{dcache_features_p[e_cfg_lr_sc], l2_features_p[e_cfg_lr_sc]};
              `RV64_AMOSWAPD, `RV64_AMOSWAPW:
                illegal_instr_o =
                  ~|{dcache_features_p[e_cfg_amo_swap], l2_features_p[e_cfg_amo_swap]};
              `RV64_AMOANDD, `RV64_AMOANDW
              ,`RV64_AMOORD, `RV64_AMOORW
              ,`RV64_AMOXORD, `RV64_AMOXORW:
                illegal_instr_o =
                  ~|{dcache_features_p[e_cfg_amo_fetch_logic], l2_features_p[e_cfg_amo_fetch_logic]};
              `RV64_AMOADDD, `RV64_AMOADDW
              ,`RV64_AMOMIND, `RV64_AMOMINW, `RV64_AMOMAXD, `RV64_AMOMAXW
              ,`RV64_AMOMINUD, `RV64_AMOMINUW, `RV64_AMOMAXUD, `RV64_AMOMAXUW:
                illegal_instr_o =
                  ~|{dcache_features_p[e_cfg_amo_fetch_arithmetic], l2_features_p[e_cfg_amo_fetch_arithmetic]};
              default: begin end
            endcase
          end
        default : illegal_instr_o = 1'b1;
      endcase

      // Immediate extraction
      // This may be overwritten by exception injection
      unique casez (instr.opcode)
        `RV64_LUI_OP, `RV64_AUIPC_OP:
          imm_o = `rv64_signext_u_imm(instr);
        `RV64_JAL_OP:
          imm_o = `rv64_signext_j_imm(instr);
        `RV64_BRANCH_OP:
          imm_o = `rv64_signext_b_imm(instr);
        `RV64_STORE_OP, `RV64_FSTORE_OP:
          imm_o = `rv64_signext_s_imm(instr);
        `RV64_JALR_OP, `RV64_LOAD_OP, `RV64_OP_IMM_OP, `RV64_OP_IMM_32_OP, `RV64_FLOAD_OP:
          imm_o = `rv64_signext_i_imm(instr);
        //`RV64_AMO_OP:
        default: imm_o = '0;
      endcase
    end


  // Monitor the values on changes
  //always_comb begin
    //$display("Values at time %0t:\r\n", $time);
    //$display("preissue_pkt_i: %0h\r\n", preissue_pkt_i);
    //$display("decode_info_i: %0h\r\n", decode_info_i);
    //$display("decode_o: %0h\r\n", decode_o);
    //$display("illegal_instr_o: %0b\r\n", illegal_instr_o);
    //$display("ecall_m_o: %0b\r\n", ecall_m_o);
    //$display("ecall_s_o: %0b\r\n", ecall_s_o);
    //$display("ecall_u_o: %0b\r\n", ecall_u_o);
    //$display("ebreak_o: %0b\r\n", ebreak_o);
    //$display("dbreak_o: %0b\r\n", dbreak_o);
    //$display("dret_o: %0b\r\n", dret_o);
    //$display("mret_o: %0b\r\n", mret_o);
    //$display("sret_o: %0b\r\n", sret_o);
    //$display("wfi_o: %0b\r\n", wfi_o);
    //$display("sfence_vma_o: %0b\r\n", sfence_vma_o);
    //$display("fencei_o: %0b\r\n", fencei_o);
    //$display("csrw_o: %0b\r\n", csrw_o);
    //$display("imm_o: %0h\r\n", imm_o);
  //end

    // Monitor the values on changes
  //always_comb begin
  // $display("At time %0t:\r\n preissue_pkt_i: %0h  decode_info_i: %0h  decode_o: %0h  illegal_instr_o: %0b  ecall_m_o: %0b  ecall_s_o: %0b  ecall_u_o: %0b  ebreak_o: %0b  dbreak_o: %0b  dret_o: %0b  mret_o: %0b  sret_o: %0b  wfi_o: %0b  sfence_vma_o: %0b  fencei_o: %0b  csrw_o: %0b  imm_o: %0h\r\n",
  //            $time, preissue_pkt_i, decode_info_i, decode_o, illegal_instr_o, ecall_m_o, ecall_s_o, ecall_u_o, ebreak_o, dbreak_o, dret_o, mret_o, sret_o, wfi_o, sfence_vma_o, fencei_o, csrw_o, imm_o);
  //end




   // File descriptor
  integer file;

  initial begin

 $display("'''''''''''''''''''''''''''''''''Starting Values:'''''''''''''''''''''''''''''''''\n");
 $display("'''''''''''''''' bp_cce_inst_decode.sv line 826''''''''''''''''''''''''''''''''''\n"); 
 $display("RV64_OP_OP value: %0h", `RV64_OP_OP);
 $display("RV64_OP_32_OP value: %0h", `RV64_OP_32_OP);

 
    // Open the file in append mode
    file = $fopen("/home/cubbins/black-parrot-sim/black-parrot/bp_top/syn/results/verilator/bp_tethered.e_bp_default_cfg.none.sim.bp-tests.hello_world/2_6_out_bp_be_instr_decode.txt", "w");
    if (!file) begin
      $display("Error: Could not open file for appending.");
      $finish;
    end
  end


  initial begin
    $display("RV64_LUI_OP        = %b", `RV64_LUI_OP);
    $display("RV64_AUIPC_OP      = %b", `RV64_AUIPC_OP);
    $display("RV64_JAL_OP        = %b", `RV64_JAL_OP);
    $display("RV64_JALR_OP       = %b", `RV64_JALR_OP);
    $display("RV64_BRANCH_OP     = %b", `RV64_BRANCH_OP);
    $display("RV64_LOAD_OP       = %b", `RV64_LOAD_OP);
    $display("RV64_STORE_OP      = %b", `RV64_STORE_OP);
    $display("RV64_OP_IMM_OP     = %b", `RV64_OP_IMM_OP);
    $display("RV64_OP_OP         = %b", `RV64_OP_OP);
    $display("RV64_MISC_MEM_OP   = %b", `RV64_MISC_MEM_OP);
    $display("RV64_SYSTEM_OP     = %b", `RV64_SYSTEM_OP);
    $display("RV64_OP_IMM_32_OP  = %b", `RV64_OP_IMM_32_OP);
    $display("RV64_OP_32_OP      = %b", `RV64_OP_32_OP);
    $display("RV64_AMO_OP        = %b", `RV64_AMO_OP);
    $display("RV64_FLOAD_OP      = %b", `RV64_FLOAD_OP);
    $display("RV64_FSTORE_OP     = %b", `RV64_FSTORE_OP);
    $display("RV64_FP_OP         = %b", `RV64_FP_OP);
    $display("RV64_FMADD_OP      = %b", `RV64_FMADD_OP);
    $display("RV64_FMSUB_OP      = %b", `RV64_FMSUB_OP);
    $display("RV64_FNMSUB_OP     = %b", `RV64_FNMSUB_OP);
    $display("RV64_FNMADD_OP     = %b", `RV64_FNMADD_OP);
  end

//=======================================================
//=======================================================

reg [31:0] prev_opcode; // Register to store the previous opcode

always_comb begin
    unique casez (instr.opcode)
        `RV64_OP_OP, `RV64_OP_32_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_OP_OP or RV64_OP_32_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_LUI_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_LUI_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_AUIPC_OP:
            if (instr.opcode != prev_opcode) begin
               // $display("Match found at time %0t: instr.opcode = %0h (RV64_AUIPC_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_JAL_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_JAL_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_JALR_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_JALR_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_BRANCH_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_BRANCH_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_LOAD_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_LOAD_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_STORE_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_STORE_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_OP_IMM_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_OP_IMM_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_MISC_MEM_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_MISC_MEM_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_SYSTEM_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_SYSTEM_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_OP_IMM_32_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_OP_IMM_32_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_AMO_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_AMO_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_FLOAD_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_FLOAD_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_FSTORE_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_FSTORE_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_FP_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_FP_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_FMADD_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_FMADD_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_FMSUB_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_FMSUB_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_FNMSUB_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_FNMSUB_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        `RV64_FNMADD_OP:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (RV64_FNMADD_OP)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
        default:
            if (instr.opcode != prev_opcode) begin
                //$display("Match found at time %0t: instr.opcode = %0h (Unsupported)", $time, instr.opcode);
                prev_opcode = instr.opcode;
            end
    endcase
end

//================================================
//================================================


reg [31:0] prev_preissue_pkt_i; // Register to store the previous preissue_pkt_i
reg [31:0] temp_preissue_pkt_i; // Temporary variable to store the previous value

always_comb begin
    temp_preissue_pkt_i = prev_preissue_pkt_i; // Initialize with the previous value
    if (file) begin
        if (preissue_pkt_i != temp_preissue_pkt_i) begin
            $fwrite(file, "%0t  %0h  %0h  %0h  %0b  %0b  %0b  %0b  %0b  %0b  %0b  %0b  %0b  %0b  %0b  %0b  %0b  %0h\r\n",
                    $time, preissue_pkt_i, decode_info_i, decode_o, illegal_instr_o, ecall_m_o, ecall_s_o, ecall_u_o, ebreak_o, dbreak_o, dret_o, mret_o, sret_o, wfi_o, sfence_vma_o, fencei_o, csrw_o, imm_o);
            prev_preissue_pkt_i = preissue_pkt_i; // Update the previous value
        end
    end
end


//=============================================

  // Monitor and display opcode matches
  always_comb begin
    unique casez (instr.opcode)
      `RV64_OP_OP, `RV64_OP_32_OP: begin
        //$display("Match found at time %0t: instr.opcode = %0h", $time, instr.opcode);
      end
      default: begin
        // Handle other cases if needed
      end
    endcase
  end


// The macro `RV64_MUL` is defined using another macro `rv64_r_type`, which likely constructs 
// a specific instruction format for the RISC-V architecture. This macro encapsulates 
// the opcode, funct3, and funct7 fields, which are used to identify specific instructions 
// in the RISC-V instruction set.
// 
// Here's a breakdown of the macro:
// - `RV64_OP_OP` is the opcode for standard 64-bit integer register-register operations.
// - `3'b000` is the funct3 field, which specifies the type of operation (in this case, multiplication).
// - `7'b0000001` is the funct7 field, which further specifies the operation (in this case, multiplication).
// 
// The `RV64_MUL` macro can be used in various parts of your code where you need 
// to identify or handle the multiplication instruction. Here are some examples 
// of how you can use this macro:
// 
// 1. **Instruction Decoding**:
//    You can use the macro in an instruction decoder to identify when a multiplication instruction is encountered.
// 
//    ```systemverilog
//    always_comb begin
//        unique casez (instr.opcode)
//            `RV64_OP_OP: begin
//                if (instr inside {`RV64_MUL, `RV64_MULH}) begin
//                    decode_cast_o.pipe_mul_v = 1'b1;
//                    //$display("Instruction: %0s - Setting pipe_mul_v", instr.opcode.name());
//                end
//                // Additional logic here
//            end
//            // Other cases here
//        endcase
//    end
//    ```
// 
// 2. **ALU Operation**:
//    You can use the macro in an ALU (Arithmetic Logic Unit) to perform 
// the multiplication operation.
// 
//    ```systemverilog
//    always_comb begin
//        case (alu_op)
//            `RV64_MUL: begin
//                alu_result = src1 * src2;
//            end
//            // Other ALU operations here
//        endcase
//    end
//    ```
// 
// 3. **Testbench**:
//    You can use the macro in a testbench to generate specific instructions for testing.
// 
//    ```systemverilog
//    initial begin
//        // Generate a multiplication instruction
//        instr = `RV64_MUL;
//        // Apply the instruction to the DUT (Device Under Test)
//        // ...
//    end
//    ```
// 
// These are just a few examples of how you can use the `RV64_MUL` macro in your code. 
// The macro helps to make your code more readable and maintainable by encapsulating 
// the specific instruction format in a single definition.
// 
// If you have any more questions or need further assistance, feel free to ask!

//=======================================
//=======================================

// `ifndef BP_BE_CTL_PKGDEF_SVH
// `define BP_BE_CTL_PKGDEF_SVH
// 
//   typedef enum logic [5:0]
//   {
//     e_int_op_add        = 6'b000000
//     ,e_int_op_sub       = 6'b001000
//     ,e_int_op_sll       = 6'b000001
//     ,e_int_op_slt       = 6'b000010
//     ,e_int_op_sge       = 6'b001010
//     ,e_int_op_sltu      = 6'b000011
//     ,e_int_op_sgeu      = 6'b001011
//     ,e_int_op_xor       = 6'b000100
//     ,e_int_op_eq        = 6'b001100
//     ,e_int_op_srl       = 6'b000101
//     ,e_int_op_sra       = 6'b001101
//     ,e_int_op_or        = 6'b000110
//     ,e_int_op_ne        = 6'b001110
//     ,e_int_op_and       = 6'b000111
//     ,e_int_op_pass_src2 = 6'b001111
//     ,e_int_op_pass_one  = 6'b111110
//     ,e_int_op_pass_zero = 6'b111111
//   } bp_be_int_fu_op_e;
// 
//   typedef enum logic [5:0]
//   {
//     // Movement instructions
//     e_aux_op_f2f        = 6'b000000
//     ,e_aux_op_f2i       = 6'b000001
//     ,e_aux_op_i2f       = 6'b000010
//     ,e_aux_op_f2iu      = 6'b000011
//     ,e_aux_op_iu2f      = 6'b000100
//     ,e_aux_op_imvf      = 6'b000101
//     ,e_aux_op_fmvi      = 6'b000110
//     ,e_aux_op_fsgnj     = 6'b000111
//     ,e_aux_op_fsgnjn    = 6'b001000
//     ,e_aux_op_fsgnjx    = 6'b001001
// 
//     // FCMP instructions
//     ,e_aux_op_feq       = 6'b001010
//     ,e_aux_op_flt       = 6'b001011
//     ,e_aux_op_fle       = 6'b001100
//     ,e_aux_op_fmax      = 6'b001101
//     ,e_aux_op_fmin      = 6'b001110
//     ,e_aux_op_fclass    = 6'b001111
//   } bp_be_aux_fu_op_e;
// 
//   typedef enum logic [5:0]
//   {
//     e_dcache_op_lb        = 6'b000000
//     ,e_dcache_op_lh       = 6'b000001
//     ,e_dcache_op_lw       = 6'b000010
//     ,e_dcache_op_ld       = 6'b000011
//     ,e_dcache_op_lbu      = 6'b000100
//     ,e_dcache_op_lhu      = 6'b000101
//     ,e_dcache_op_lwu      = 6'b000110
// 
//     ,e_dcache_op_sb       = 6'b001000
//     ,e_dcache_op_sh       = 6'b001001
//     ,e_dcache_op_sw       = 6'b001010
//     ,e_dcache_op_sd       = 6'b001011
// 
//     ,e_dcache_op_lrw      = 6'b000111
//     ,e_dcache_op_scw      = 6'b001100
// 
//     ,e_dcache_op_lrd      = 6'b001101
//     ,e_dcache_op_scd      = 6'b001110
// 
//     ,e_dcache_op_flw      = 6'b100010
//     ,e_dcache_op_fld      = 6'b100011
// 
//     ,e_dcache_op_fsw      = 6'b100100
//     ,e_dcache_op_fsd      = 6'b100101
// 
//     ,e_dcache_op_amoswapw = 6'b010000
//     ,e_dcache_op_amoaddw  = 6'b010001
//     ,e_dcache_op_amoxorw  = 6'b010010
//     ,e_dcache_op_amoandw  = 6'b010011
//     ,e_dcache_op_amoorw   = 6'b010100
//     ,e_dcache_op_amominw  = 6'b010101
//     ,e_dcache_op_amomaxw  = 6'b010110
//     ,e_dcache_op_amominuw = 6'b010111
//     ,e_dcache_op_amomaxuw = 6'b011000
// 
//     ,e_dcache_op_amoswapd = 6'b011001
//     ,e_dcache_op_amoaddd  = 6'b011010
//     ,e_dcache_op_amoxord  = 6'b011011
//     ,e_dcache_op_amoandd  = 6'b011100
//     ,e_dcache_op_amoord   = 6'b011101
//     ,e_dcache_op_amomind  = 6'b011110
//     ,e_dcache_op_amomaxd  = 6'b011111
//     ,e_dcache_op_amominud = 6'b100000
//     ,e_dcache_op_amomaxud = 6'b100001
// 
//     ,e_dcache_op_ptw      = 6'b111000
// 
//     ,e_dcache_op_bzero    = 6'b110000
//     ,e_dcache_op_bclean   = 6'b110001
//     ,e_dcache_op_binval   = 6'b110010
//     ,e_dcache_op_bflush   = 6'b110100
//     ,e_dcache_op_clean    = 6'b111110
//     ,e_dcache_op_inval    = 6'b111101
//     ,e_dcache_op_flush    = 6'b111111
//   } bp_be_dcache_fu_op_e;
// 
//   typedef enum logic [5:0]
//   {
//     e_fma_op_fadd    = 6'b000000
//     ,e_fma_op_fsub   = 6'b000001
//     ,e_fma_op_fmul   = 6'b000010
//     ,e_fma_op_fmadd  = 6'b000011
//     ,e_fma_op_fmsub  = 6'b000100
//     ,e_fma_op_fnmsub = 6'b000101
//     ,e_fma_op_fnmadd = 6'b000110
//     ,e_fma_op_imul   = 6'b000111
//   } bp_be_fma_fu_op_e;
// 
//   typedef enum logic [5:0]
//   {
//     e_long_op_div     = 6'b000001
//     ,e_long_op_divu   = 6'b000010
//     ,e_long_op_rem    = 6'b000011
//     ,e_long_op_remu   = 6'b000100
//     ,e_long_op_mulh   = 6'b000101
//     ,e_long_op_mulhsu = 6'b000110
//     ,e_long_op_mulhu  = 6'b000111
//     ,e_long_op_fdiv   = 6'b001000
//     ,e_long_op_fsqrt  = 6'b001001
//   } bp_be_long_fu_op_e;
// 
//   typedef struct packed
//   {
//     union packed
//     {
//       bp_be_int_fu_op_e      int_fu_op;
//       bp_be_aux_fu_op_e      aux_fu_op;
//       bp_be_dcache_fu_op_e   dcache_fu_op;
//       bp_be_fma_fu_op_e      fma_fu_op;
//       bp_be_long_fu_op_e     long_fu_op;
//     }  t;
//   }  bp_be_fu_op_s;
// 
//   typedef enum logic
//   {
//     e_src1_is_rs1 = 1'b0
//     ,e_src1_is_pc = 1'b1
//   } bp_be_src1_e;
// 
//   typedef enum logic
//   {
//     e_src2_is_rs2  = 1'b0
//     ,e_src2_is_imm = 1'b1
//   } bp_be_src2_e;
// 
//   typedef enum logic
//   {
//     e_baddr_is_pc   = 1'b0
//     ,e_baddr_is_rs1 = 1'b1
//   } bp_be_baddr_e;
// 
//   typedef struct packed
//   {
//     logic                              compressed;
// 
//     logic                              pipe_int_v;
//     logic                              pipe_mem_early_v;
//     logic                              pipe_aux_v;
//     logic                              pipe_mem_final_v;
//     logic                              pipe_sys_v;
//     logic                              pipe_mul_v;
//     logic                              pipe_fma_v;
//     logic                              pipe_long_v;
// 
//     logic                              irs1_r_v;
//     logic                              irs2_r_v;
//     logic                              frs1_r_v;
//     logic                              frs2_r_v;
//     logic                              frs3_r_v;
//     logic                              irf_w_v;
//     logic                              frf_w_v;
//     logic                              branch_v;
//     logic                              jump_v;
//     logic                              fence_v;
//     logic                              dcache_r_v;
//     logic                              dcache_w_v;
//     logic                              dcache_cbo_v;
//     logic                              dcache_mmu_v;
//     logic                              csr_w_v;
//     logic                              csr_r_v;
//     logic                              mem_v;
//     logic                              score_v;
//     logic                              spec_w_v;
// 
//     logic                              fp_raw;
//     logic                              rs1_unsigned;
//     logic                              rs2_unsigned;
// 
//     bp_be_fu_op_s                      fu_op;
//     logic [$bits(bp_be_fp_tag_e)-1:0]  fp_tag;
//     logic [$bits(bp_be_int_tag_e)-1:0] int_tag;
// 
//     logic [$bits(bp_be_src1_e)-1:0]    src1_sel;
//     logic [$bits(bp_be_src2_e)-1:0]    src2_sel;
//     logic [$bits(bp_be_baddr_e)-1:0]   baddr_sel;
//   }  bp_be_decode_s;




endmodule

