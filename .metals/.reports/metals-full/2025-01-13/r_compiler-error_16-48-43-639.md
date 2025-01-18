file://<WORKSPACE>/src/main/scala/Pipeline/UNits/Control.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 4736
uri: file://<WORKSPACE>/src/main/scala/Pipeline/UNits/Control.scala
text:
```scala
package Pipeline
import chisel3._
import chisel3.util._

class Control extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(7.W))         // 7-bit opcode
    val mem_write = Output(Bool())        // whether a write to memory
    val branch = Output(Bool())           // whether a branch instruction
    val mem_read = Output(Bool())         // whether a read from memory
    val reg_write = Output(Bool())        // whether a register write
    val men_to_reg = Output(Bool())       // whether the value written to a register (for load instructions)
    val alu_operation = Output(UInt(3.W))
    val FPU_operation = Output(UInt(3.W))
    val operand_A = Output(UInt(2.W))  // Operand A source selection for the ALU
    val operand_B = Output(Bool()) // Operand B source selection for the ALU

    // Indicates the type of extension to be used (e.g., sign-extend, zero-extend)
    val extend = Output(UInt(2.W))   
    val next_pc_sel = Output(UInt(2.W)) // next PC value (e.g., PC+4, branch target, jump target)
  })
  io.mem_write := 0.B
  io.branch := 0.B
  io.mem_read := 0.B
  io.reg_write := 0.B
  io.men_to_reg := 0.B
  io.alu_operation := 0.U
  io.FPU_operation := 0.U
  io.operand_A := 0.U
  io.operand_B := 0.B
  io.extend := 0.U
  io.next_pc_sel := 0.U

  switch(io.opcode) {
    // R type instructions (e.g., add, sub)
    is(51.U) {
      io.mem_write := 0.B
      io.branch := 0.B
      io.mem_read := 0.B
      io.reg_write := 1.B
      io.men_to_reg := 0.B
      io.alu_operation := 0.U
      io.operand_A := 0.U
      io.operand_B := 0.B
      io.extend := 0.U
      io.next_pc_sel := 0.U
    }

    // I type instructions (e.g., immediate operations)
    is(19.U) {
      io.mem_write := 0.B
      io.branch := 0.B
      io.mem_read := 0.B
      io.reg_write := 1.B
      io.men_to_reg := 0.B
      io.alu_operation := 1.U
      io.operand_A := 0.U
      io.operand_B := 1.B
      io.extend := 0.U
      io.next_pc_sel := 0.U
    }

    // S type instructions (e.g., store operations)
    is(35.U) {
      io.mem_write := 1.B
      io.branch := 0.B
      io.mem_read := 0.B
      io.reg_write := 0.B
      io.men_to_reg := 0.B
      io.alu_operation := 5.U
      io.operand_A := 0.U
      io.operand_B := 1.B
      io.extend := 1.U
      io.next_pc_sel := 0.U
    }

    // Load instructions (e.g., load data from memory)
    is(3.U) {
      io.mem_write := 0.B
      io.branch := 0.B
      io.mem_read := 1.B
      io.reg_write := 1.B
      io.men_to_reg := 1.B
      io.alu_operation := 4.U
      io.operand_A := 0.U
      io.operand_B := 1.B
      io.extend := 0.U
      io.next_pc_sel := 0.U
    }

    // SB type instructions (e.g., conditional branch)
    is(99.U) {
      io.mem_write := 0.B
      io.branch := 1.B
      io.mem_read := 0.B
      io.reg_write := 0.B
      io.men_to_reg := 0.B
      io.alu_operation := 2.U
      io.operand_A := 0.U
      io.operand_B := 0.B
      io.extend := 0.U
      io.next_pc_sel := 1.U
    }

    // UJ type instructions (e.g., jump and link)
    is(111.U) {
      io.mem_write := 0.B
      io.branch := 0.B
      io.mem_read := 0.B
      io.reg_write := 1.B
      io.men_to_reg := 0.B
      io.alu_operation := 3.U
      io.operand_A := 1.U
      io.operand_B := 0.B
      io.extend := 0.U
      io.next_pc_sel := 2.U
    }

    // Jalr instruction (e.g., jump and link register)
    is(103.U) {
      io.mem_write := 0.B
      io.branch := 0.B
      io.mem_read := 0.B
      io.reg_write := 1.B
      io.men_to_reg := 0.B
      io.alu_operation := 3.U
      io.operand_A := 1.U
      io.operand_B := 0.B
      io.extend := 0.U
      io.next_pc_sel := 3.U
    }

    // U type (LUI) instructions (e.g., load upper immediate)
    is(55.U) {
      io.mem_write := 0.B
      io.branch := 0.B
      io.mem_read := 0.B
      io.reg_write := 1.B
      io.men_to_reg := 0.B
      io.alu_operation := 6.U
      io.operand_A := 3.U
      io.operand_B := 1.B
      io.extend := 2.U
      io.next_pc_sel := 0.U
    }

    // U type (AUIPC) instructions (e.g., add immediate to PC)
    is(23.U) {
      io.mem_write := 0.B
      io.branch := 0.B
      io.mem_read := 0.B
      io.reg_write := 1.B
      io.men_to_reg := 0.B
      io.alu_operation := 7.U
      io.operand_A := 2.U
      io.operand_B := 1.B
      io.extend := 2.U
      io.next_pc_sel := 0.U
    }

    // for RV32F/RV32D
    // R-type instructions (opcode = 0b1010011)
    is(83.U) {
      io.mem_write := 0.B
      io.branch := 0.B
      io.mem_read := 0.B
      io.reg_write := 1.B
      io.men_to_reg := 0.B
      io.FPU_operation := 1.U
      io.operand_A := 0.U
      io.operand_B := 0.B
      io.extend := 0.U
      io.next_pc_sel := 0.U
    }

    //R4-type instructions (opcode = 0b1000011,@@)
    is(67.U){

    }

    //I-type instructions
    is(7.U){

    }

    //S-type instructions
    is(39.U){

    }

  }
}

```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:244)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:101)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:47)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:422)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1