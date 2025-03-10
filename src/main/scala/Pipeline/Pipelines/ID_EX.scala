package Pipeline
import chisel3._
import chisel3.util._

class ID_EX extends Module {
  val io = IO(new Bundle {
    val rs1_in              = Input(UInt(5.W))
    val rs2_in              = Input(UInt(5.W))
    val rs3_in              = Input(UInt(5.W))
    val rs1_data_in         = Input(SInt(32.W))
    val rs2_data_in         = Input(SInt(32.W))
    val rs3_data_in         = Input(SInt(32.W))
    val imm                 = Input(SInt(32.W))
    val rd_in               = Input(UInt(5.W))
    val func3_in            = Input(UInt(3.W))
    val func7_in            = Input(Bool())
    val ctrl_MemWr_in       = Input(Bool())
    val ctrl_Branch_in      = Input(Bool())
    val ctrl_MemRd_in       = Input(Bool())
    val ctrl_Reg_W_in       = Input(Bool())
    val ctrl_MemToReg_in    = Input(Bool())
    val ctrl_AluOp_in       = Input(UInt(3.W))
    //RV32F
    val ctrl_FPU_en_in      = Input(Bool())
    val ctrl_FPU_Op_in      = Input(UInt(3.W))
    val FPU_func7_in        = Input(UInt(5.W))
    val FPU_fmt_in          = Input(UInt(2.W))
    val FPU_op5_in          = Input(UInt(5.W))
    val FPU_rm_in           = Input(UInt(3.W))
    //
    val ctrl_OpA_in         = Input(UInt(2.W))
    val ctrl_OpB_in         = Input(Bool())
    //RV32F opc
    val ctrl_OpC_in         = Input(Bool())
    //
    val ctrl_nextpc_in      = Input(UInt(2.W))
    val IFID_pc4_in         = Input(UInt(32.W))

    val rs1_out             = Output(UInt(5.W))
    val rs2_out             = Output(UInt(5.W))
    val rs3_out             = Output(UInt(5.W))
    val rs1_data_out        = Output(SInt(32.W))
    val rs2_data_out        = Output(SInt(32.W))
    val rs3_data_out        = Output(SInt(32.W))
    val rd_out              = Output(UInt(5.W))
    val imm_out             = Output(SInt(32.W))
    val func3_out           = Output(UInt(3.W))
    val func7_out           = Output(Bool())
    val ctrl_MemWr_out      = Output(Bool())
    val ctrl_Branch_out     = Output(Bool())
    val ctrl_MemRd_out      = Output(Bool())
    val ctrl_Reg_W_out      = Output(Bool())
    val ctrl_MemToReg_out   = Output(Bool())
    val ctrl_AluOp_out      = Output(UInt(3.W))
    //RV32F
    val ctrl_FPU_en_out     = Output(Bool())
    val ctrl_FPU_Op_out     = Output(UInt(3.W))
    val FPU_func7_out       = Output(UInt(5.W))
    val FPU_fmt_out         = Output(UInt(2.W))
    val FPU_op5_out         = Output(UInt(5.W))
    val FPU_rm_out          = Output(UInt(3.W))
    //
    val ctrl_OpA_out        = Output(UInt(2.W))
    val ctrl_OpB_out        = Output(Bool())
    //RV32F opc
    val ctrl_OpC_out        = Output(Bool())
    //
    val ctrl_nextpc_out     = Output(UInt(2.W))
    val IFID_pc4_out        = Output(UInt(32.W))
  })

  io.rs1_out            :=  RegNext(io.rs1_in)
  io.rs2_out            :=  RegNext(io.rs2_in)
  io.rs3_out            :=  RegNext(io.rs3_in)
  io.rs1_data_out       :=  RegNext(io.rs1_data_in)
  io.rs2_data_out       :=  RegNext(io.rs2_data_in)
  io.rs3_data_out       :=  RegNext(io.rs3_data_in)
  io.imm_out            :=  RegNext(io.imm)
  io.rd_out             :=  RegNext(io.rd_in)
  io.func3_out          :=  RegNext(io.func3_in)
  io.func7_out          :=  RegNext(io.func7_in)
  io.ctrl_MemWr_out     :=  RegNext(io.ctrl_MemWr_in)
  io.ctrl_Branch_out    :=  RegNext(io.ctrl_Branch_in)
  io.ctrl_MemRd_out     :=  RegNext(io.ctrl_MemRd_in)
  io.ctrl_Reg_W_out     :=  RegNext(io.ctrl_Reg_W_in)
  io.ctrl_MemToReg_out  :=  RegNext(io.ctrl_MemToReg_in)
  io.ctrl_AluOp_out     :=  RegNext(io.ctrl_AluOp_in)
  //fpu
  io.ctrl_FPU_en_out    :=  RegNext(io.ctrl_FPU_en_in)
  io.ctrl_FPU_Op_out    :=  RegNext(io.ctrl_FPU_Op_in)
  io.FPU_func7_out      :=  RegNext(io.FPU_func7_in)
  io.FPU_fmt_out        :=  RegNext(io.FPU_fmt_in)
  io.FPU_op5_out        :=  RegNext(io.FPU_op5_in)
  io.FPU_rm_out         :=  RegNext(io.FPU_rm_in)
  //
  io.ctrl_OpA_out       :=  RegNext(io.ctrl_OpA_in)
  io.ctrl_OpB_out       :=  RegNext(io.ctrl_OpB_in)
  io.ctrl_OpC_out       :=  RegNext(io.ctrl_OpC_in)
  io.ctrl_nextpc_out    :=  RegNext(io.ctrl_nextpc_in)
  io.IFID_pc4_out       :=  RegNext(io.IFID_pc4_in)
}
//0120: rs3
