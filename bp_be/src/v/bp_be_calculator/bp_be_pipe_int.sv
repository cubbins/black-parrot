/**
 *
 * Name:
 *   bp_be_pipe_int.v
 *
 * Description:
 *   Pipeline for RISC-V integer instructions. Handles integer computation.
 *
 * Notes:
 *
 */
`include "bp_common_defines.svh"
`include "bp_be_defines.svh"

module bp_be_pipe_int
 import bp_common_pkg::*;
 import bp_be_pkg::*;
 #(parameter bp_params_e bp_params_p = e_bp_default_cfg
   `declare_bp_proc_params(bp_params_p)

   , localparam reservation_width_lp = `bp_be_reservation_width(vaddr_width_p)
   )
  (input                                    clk_i
   , input                                  reset_i

   , input                                  en_i
   , input [reservation_width_lp-1:0]       reservation_i
   , input                                  flush_i

   // Pipeline results
   , output logic [dpath_width_gp-1:0]      data_o
   , output logic                           v_o
   , output logic                           branch_o
   , output logic                           btaken_o
   , output logic [vaddr_width_p-1:0]       npc_o
   , output logic                           instr_misaligned_v_o
   );

  // Suppress unused signal warning
  wire unused = &{clk_i, reset_i, flush_i};

  `declare_bp_be_internal_if_structs(vaddr_width_p, paddr_width_p, asid_width_p, branch_metadata_fwd_width_p);
  bp_be_reservation_s reservation;
  bp_be_decode_s decode;
  rv64_instr_s instr;

  assign reservation = reservation_i;
  assign decode = reservation.decode;
  assign instr = reservation.instr;
  wire [vaddr_width_p-1:0] pc  = reservation.pc;
  wire [dword_width_gp-1:0] rs1 = reservation.isrc1;
  wire [dword_width_gp-1:0] rs2 = reservation.isrc2;
  wire [dword_width_gp-1:0] imm = reservation.isrc3;
  wire word_op = (decode.int_tag == e_int_word);

  // Sign-extend PC for calculation
  wire [dword_width_gp-1:0] pc_sext_li = `BSG_SIGN_EXTEND(pc, dword_width_gp);
  wire [dword_width_gp-1:0] pc_plus4   = pc_sext_li + dword_width_gp'(4);

  wire [dword_width_gp-1:0] src1  = decode.src1_sel  ? pc_sext_li : rs1;
  wire [dword_width_gp-1:0] src2  = decode.src2_sel  ? imm        : rs2;

  wire [rv64_shamt_width_gp-1:0] shamt = word_op ? src2[0+:rv64_shamtw_width_gp] : src2[0+:rv64_shamt_width_gp];

  // ALU
  logic [dword_width_gp-1:0] alu_result;
  always_comb
    unique case (decode.fu_op)
      e_int_op_add       : alu_result = src1 +  src2;
      e_int_op_sub       : alu_result = src1 -  src2;
      e_int_op_xor       : alu_result = src1 ^  src2;
      e_int_op_or        : alu_result = src1 |  src2;
      e_int_op_and       : alu_result = src1 &  src2;
      e_int_op_sll       : alu_result = src1 << shamt;
      e_int_op_srl       : alu_result = word_op ? $unsigned(src1[0+:word_width_gp]) >>> shamt : $unsigned(src1) >>> shamt;
      // TODO: not a final solution
      e_int_op_sra       : alu_result = word_op ? $signed(src1[0+:word_width_gp]) >>> shamt : $signed(src1) >>> shamt;
      e_int_op_pass_src2 : alu_result = src2;
      e_int_op_pass_one  : alu_result = 1'b1;
      e_int_op_pass_zero : alu_result = 1'b0;

      // Single bit results
      e_int_op_eq   : alu_result = (dword_width_gp)'(src1 == src2);
      e_int_op_ne   : alu_result = (dword_width_gp)'(src1 != src2);
      e_int_op_slt  : alu_result = (dword_width_gp)'($signed(src1) <  $signed(src2));
      e_int_op_sltu : alu_result = (dword_width_gp)'(src1 <  src2);
      e_int_op_sge  : alu_result = (dword_width_gp)'($signed(src1) >= $signed(src2));
      e_int_op_sgeu : alu_result = (dword_width_gp)'(src1 >= src2);
      default       : alu_result = '0;
    endcase



  logic [dword_width_gp-1:0] alu_result22;
  integer file;

 // Open the file initially
  initial begin
    file = $fopen("intOutput.txt", "w");
    if (file == 0) begin
      $display("Error: Could not open file.");
      $finish;
    end
  end

  // Write results to the file
  always_comb begin
    unique case (decode.fu_op)
      e_int_op_add: begin
        alu_result22 = src1 + src2;
        $fwrite(file, "Time: %0t, Case e_int_op_add satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_sub: begin
        alu_result22 = src1 - src2;
        $fwrite(file, "Time: %0t, Case e_int_op_sub satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_xor: begin
        alu_result22 = src1 ^ src2;
        $fwrite(file, "Time: %0t, Case e_int_op_xor satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_or: begin
        alu_result22 = src1 | src2;
        $fwrite(file, "Time: %0t, Case e_int_op_or satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_and: begin
        alu_result22 = src1 & src2;
        $fwrite(file, "Time: %0t, Case e_int_op_and satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_sll: begin
        alu_result22 = src1 << shamt;
        $fwrite(file, "Time: %0t, Case e_int_op_sll satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_srl: begin
        alu_result22 = word_op ? $unsigned(src1[0+:dword_width_gp]) >>> shamt : $unsigned(src1) >>> shamt;
        $fwrite(file, "Time: %0t, Case e_int_op_srl satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_sra: begin
        alu_result22 = word_op ? $signed(src1[0+:dword_width_gp]) >>> shamt : $signed(src1) >>> shamt;
        $fwrite(file, "Time: %0t, Case e_int_op_sra satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_pass_src2: begin
        alu_result22 = src2;
        $fwrite(file, "Time: %0t, Case e_int_op_pass_src2 satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_pass_one: begin
        alu_result22 = 1'b1;
        $fwrite(file, "Time: %0t, Case e_int_op_pass_one satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_pass_zero: begin
        alu_result22 = 1'b0;
        $fwrite(file, "Time: %0t, Case e_int_op_pass_zero satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_eq: begin
        alu_result22 = (dword_width_gp)'(src1 == src2);
        $fwrite(file, "Time: %0t, Case e_int_op_eq satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_ne: begin
        alu_result22 = (dword_width_gp)'(src1 != src2);
        $fwrite(file, "Time: %0t, Case e_int_op_ne satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_slt: begin
        alu_result22 = (dword_width_gp)'($signed(src1) < $signed(src2));
        $fwrite(file, "Time: %0t, Case e_int_op_slt satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_sltu: begin
        alu_result22 = (dword_width_gp)'(src1 < src2);
        $fwrite(file, "Time: %0t, Case e_int_op_sltu satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_sge: begin
        alu_result22 = (dword_width_gp)'($signed(src1) >= $signed(src2));
        $fwrite(file, "Time: %0t, Case e_int_op_sge satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      e_int_op_sgeu: begin
        alu_result22 = (dword_width_gp)'(src1 >= src2);
        $fwrite(file, "Time: %0t, Case e_int_op_sgeu satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
      default: begin
        alu_result22 = '0;
        $fwrite(file, "Time: %0t, Default case satisfied: alu_result22 = %0d\n", $time, alu_result22);
      end
    endcase
  end

  // Close the file when the module exits
  final begin
    $fclose(file);
  end

//======================================
integer file_posedge;

// Open the file initially
initial begin

  file_posedge = $fopen("intOutput_posedge.txt", "w");
  if (file_posedge == 0) begin
    $display("Error: Could not open file.");
    $finish;
  end
end

//====

// Variables to store previous values and operation names
logic [dword_width_gp-1:0] prev_alu_result_posedge;
logic [dword_width_gp-1:0] alu_result_posedge;
string prev_op_name;
string op_name;

// Initialize previous values
initial begin
  prev_alu_result_posedge = '0;
  prev_op_name = "";
end

// Write results to the file on the positive edge of the clock
always_ff @(posedge clk_i) begin
  unique case (decode.fu_op)
    e_int_op_add: begin
      alu_result_posedge = src1 + src2;
      op_name = "e_int_op_add";
    end
    e_int_op_sub: begin
      alu_result_posedge = src1 - src2;
      op_name = "e_int_op_sub";
    end
    e_int_op_xor: begin
      alu_result_posedge = src1 ^ src2;
      op_name = "e_int_op_xor";
    end
    e_int_op_or: begin
      alu_result_posedge = src1 | src2;
      op_name = "e_int_op_or";
    end
    e_int_op_and: begin
      alu_result_posedge = src1 & src2;
      op_name = "e_int_op_and";
    end
    e_int_op_sll: begin
      alu_result_posedge = src1 << shamt;
      op_name = "e_int_op_sll";
    end
    e_int_op_srl: begin
      alu_result_posedge = word_op ? $unsigned(src1[0+:dword_width_gp]) >>> shamt : $unsigned(src1) >>> shamt;
      op_name = "e_int_op_srl";
    end
    e_int_op_sra: begin
      alu_result_posedge = word_op ? $signed(src1[0+:dword_width_gp]) >>> shamt : $signed(src1) >>> shamt;
      op_name = "e_int_op_sra";
    end
    e_int_op_pass_src2: begin
      alu_result_posedge = src2;
      op_name = "e_int_op_pass_src2";
    end
    e_int_op_pass_one: begin
      alu_result_posedge = 1'b1;
      op_name = "e_int_op_pass_one";
    end
    e_int_op_pass_zero: begin
      alu_result_posedge = 1'b0;
      op_name = "e_int_op_pass_zero";
    end
    e_int_op_eq: begin
      alu_result_posedge = (dword_width_gp)'(src1 == src2);
      op_name = "e_int_op_eq";
    end
    e_int_op_ne: begin
      alu_result_posedge = (dword_width_gp)'(src1 != src2);
      op_name = "e_int_op_ne";
    end
    e_int_op_slt: begin
      alu_result_posedge = (dword_width_gp)'($signed(src1) < $signed(src2));
      op_name = "e_int_op_slt";
    end
    e_int_op_sltu: begin
      alu_result_posedge = (dword_width_gp)'(src1 < src2);
      op_name = "e_int_op_sltu";
    end
    e_int_op_sge: begin
      alu_result_posedge = (dword_width_gp)'($signed(src1) >= $signed(src2));
      op_name = "e_int_op_sge";
    end
    e_int_op_sgeu: begin
      alu_result_posedge = (dword_width_gp)'(src1 >= src2);
      op_name = "e_int_op_sgeu";
    end
    default: begin
      alu_result_posedge = '0;
      op_name = "default";
    end
  endcase

  // Write to file only if the value or operation name has changed
  if (alu_result_posedge != prev_alu_result_posedge || op_name != prev_op_name) begin
    $fwrite(file_posedge, "Time: %0t, Case %s satisfied: alu_result_posedge = %0d\n", $time, op_name, alu_result_posedge);
    prev_alu_result_posedge = alu_result_posedge;
    prev_op_name = op_name;
  end
end


//=====


// Close the file when the module exits
final begin
  $fclose(file_posedge);
end



//======================================

//analyze the code and write out, using $fwrite, 
//the names of the variables in the code, and their values, 
//if they are given as the widths and lengths of vectors, 
//please provide your answer in System Verilog
//change all instances of file to variable_analysis_file


//====================

    integer variable_analysis_file;

  // Open the file initially
  initial begin
    variable_analysis_file = $fopen("variable_analysis.txt", "w");
    if (variable_analysis_file == 0) begin
      $display("Error: Could not open file.");
      $finish;
    end

    // Write variable names and values to the file
    $fwrite(variable_analysis_file, "Variable Analysis:\n");

    // Inputs
    $fwrite(variable_analysis_file, "clk_i: %0d\n", clk_i);
    $fwrite(variable_analysis_file, "reset_i: %0d\n", reset_i);
    $fwrite(variable_analysis_file, "en_i: %0d\n", en_i);
    $fwrite(variable_analysis_file, "reservation_i: %0d (width: %0d)\n", reservation_i, reservation_width_lp);
    $fwrite(variable_analysis_file, "flush_i: %0d\n", flush_i);

    // Outputs
    $fwrite(variable_analysis_file, "data_o: %0d (width: %0d)\n", data_o, dpath_width_gp);
    $fwrite(variable_analysis_file, "v_o: %0d\n", v_o);
    $fwrite(variable_analysis_file, "branch_o: %0d\n", branch_o);
    $fwrite(variable_analysis_file, "btaken_o: %0d\n", btaken_o);
    $fwrite(variable_analysis_file, "npc_o: %0d (width: %0d)\n", npc_o, vaddr_width_p);
    $fwrite(variable_analysis_file, "instr_misaligned_v_o: %0d\n", instr_misaligned_v_o);

    // Internal signals
    $fwrite(variable_analysis_file, "unused: %0d\n", unused);
    $fwrite(variable_analysis_file, "reservation: %0d\n", reservation);
    $fwrite(variable_analysis_file, "decode: %0d\n", decode);
    $fwrite(variable_analysis_file, "instr: %0d\n", instr);
    $fwrite(variable_analysis_file, "pc: %0d (width: %0d)\n", pc, vaddr_width_p);
    $fwrite(variable_analysis_file, "rs1: %0d (width: %0d)\n", rs1, dword_width_gp);
    $fwrite(variable_analysis_file, "rs2: %0d (width: %0d)\n", rs2, dword_width_gp);
    $fwrite(variable_analysis_file, "imm: %0d (width: %0d)\n", imm, dword_width_gp);
    $fwrite(variable_analysis_file, "word_op: %0d\n", word_op);
    $fwrite(variable_analysis_file, "pc_sext_li: %0d (width: %0d)\n", pc_sext_li, dword_width_gp);
    $fwrite(variable_analysis_file, "pc_plus4: %0d (width: %0d)\n", pc_plus4, dword_width_gp);
    $fwrite(variable_analysis_file, "src1: %0d (width: %0d)\n", src1, dword_width_gp);
    $fwrite(variable_analysis_file, "src2: %0d (width: %0d)\n", src2, dword_width_gp);
    $fwrite(variable_analysis_file, "shamt: %0d (width: %0d)\n", shamt, rv64_shamt_width_gp);
    $fwrite(variable_analysis_file, "alu_result: %0d (width: %0d)\n", alu_result, dword_width_gp);
    $fwrite(variable_analysis_file, "baddr: %0d (width: %0d)\n", baddr, vaddr_width_p);
    $fwrite(variable_analysis_file, "taken_raw: %0d (width: %0d)\n", taken_raw, vaddr_width_p);
    $fwrite(variable_analysis_file, "taken_tgt: %0d (width: %0d)\n", taken_tgt, vaddr_width_p);
    $fwrite(variable_analysis_file, "ntaken_tgt: %0d (width: %0d)\n", ntaken_tgt, vaddr_width_p);
    $fwrite(variable_analysis_file, "ird_data_lo: %0d (width: %0d)\n", ird_data_lo, dpath_width_gp);
    $fwrite(variable_analysis_file, "br_result: %0d (width: %0d)\n", br_result, dpath_width_gp);
    $fwrite(variable_analysis_file, "int_result: %0d (width: %0d)\n", int_result, dword_width_gp);

    // Close the file
    $fclose(variable_analysis_file);
  end
//====================


  wire [vaddr_width_p-1:0] baddr = decode.baddr_sel ? rs1 : pc;
  wire [vaddr_width_p-1:0] taken_raw = baddr + imm;
  wire [vaddr_width_p-1:0] taken_tgt = {taken_raw[vaddr_width_p-1:1], 1'b0};
  wire [vaddr_width_p-1:0] ntaken_tgt = pc + (decode.compressed ? 4'd2 : 4'd4);

  logic [dpath_width_gp-1:0] ird_data_lo;
  wire [dpath_width_gp-1:0] br_result = dpath_width_gp'($signed(ntaken_tgt));
  wire [dword_width_gp-1:0] int_result = decode.branch_v ? br_result : alu_result;
  bp_be_int_box
   #(.bp_params_p(bp_params_p))
   box
    (.raw_i(int_result)
     ,.tag_i(decode.int_tag)
     ,.unsigned_i(1'b0)
     ,.reg_o(ird_data_lo)
     );

  assign data_o = ird_data_lo;
  assign v_o    = en_i & reservation.v & reservation.decode.pipe_int_v;

  assign instr_misaligned_v_o = en_i & btaken_o & (taken_tgt[1:0] != 2'b00) & !compressed_support_p;

  assign branch_o = decode.branch_v;
  assign btaken_o = decode.branch_v & (decode.jump_v | alu_result[0]);
  assign npc_o = btaken_o ? taken_tgt : ntaken_tgt;


// //analyze the code and write out, using $fwrite, 
// //the names of the variables in the code, and their values, 
// //if they are given as the widths and lengths of vectors, 
// //please provide your answer in System Verilog
// //change all instances of file to variable_analysis_file
// 
// 
// These instructions are part of the pipeline control logic for handling branch instructions and updating the program counter in the RISC-V integer pipeline. Here's a breakdown of their use:
// Branch Address Calculation:
//    wire [vaddr_width_p-1:0] baddr = decode.baddr_sel ? rs1 : pc;
// 
// 
//  - `baddr` is the base address for the branch calculation. It is either the value of `rs1` (source register 1) or the program counter (`pc`), depending on the `baddr_sel` signal from the decode stage.2. **Target Address Calculation**:
//    wire [vaddr_width_p-1:0] taken_raw = baddr + imm;
//    wire [vaddr_width_p-1:0] taken_tgt = {taken_raw[vaddr_width_p-1:1], 1'b0};
// 
// 
//  - `taken_raw` is the raw target address for the branch, calculated by adding the immediate value (`imm`) to the base address (`baddr`). - `taken_tgt` is the aligned target address, ensuring that the least significant bit is zero (since instructions are word-aligned).3. **Next Program Counter Calculation**:
//    wire [vaddr_width_p-1:0] ntaken_tgt = pc + (decode.compressed ? 4'd2 : 4'd4);
// 
// 
//  - `ntaken_tgt` is the next program counter value if the branch is not taken. It increments the program counter by 2 if the instruction is compressed, otherwise by 4.4. **Intermediate Data and Result Calculation**:
//    logic [dpath_width_gp-1:0] ird_data_lo;
//    wire [dpath_width_gp-1:0] br_result = dpath_width_gp'($signed(ntaken_tgt));
//    wire [dword_width_gp-1:0] int_result = decode.branch_v ? br_result : alu_result;
// 
// 
//  - `br_result` is the result of the branch calculation, sign-extended to the data path 
// width. - `int_result` is the final result, which is either the branch result (`br_result`) 
// if a branch is valid, or the ALU result (`alu_result`).5. **Box Module**:
//    bp_be_int_box
//     #(.bp_params_p(bp_params_p))
//     box
//      (.raw_i(int_result)
//       ,.tag_i(decode.int_tag)
//       ,.unsigned_i(1'b0)
//       ,.reg_o(ird_data_lo)
//       );
// 
// 
//  - This instantiates a module `bp_be_int_box` that processes the integer result (`int_result`) 
// and outputs the processed data (`ird_data_lo`).6. **Output Assignments**:
//    assign data_o = ird_data_lo;
//    assign v_o    = en_i & reservation.v & reservation.decode.pipe_int_v;
//    assign instr_misaligned_v_o = en_i & btaken_o & (taken_tgt[1:0] != 2'b00) & !compressed_support_p;
//    assign branch_o = decode.branch_v;
//    assign btaken_o = decode.branch_v & (decode.jump_v | alu_result[0]);
//    assign npc_o = btaken_o ? taken_tgt : ntaken_tgt;
// 
// 
//  - `data_o` is assigned the processed data from `ird_data_lo`. - `v_o` indicates 
// if the pipeline stage is valid. - `instr_misaligned_v_o` indicates 
// if the instruction is misaligned. - `branch_o` indicates if a branch
//  is valid. - `btaken_o` indicates if the branch is taken. - `npc_o` 
// is the next program counter value, either the taken target (`taken_tgt`) 
// or the not-taken target (`ntaken_tgt`).These instructions collectively manage 
// the control flow for branch instructions, ensuring the correct program counter 
// updates and handling of branch targets in the pipeline. 
// If you have any further questions or need more details, feel free to ask!



endmodule

