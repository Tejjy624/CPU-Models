// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop, 00 for ld/st, 10 for R-type, 01 for branch
 * Input:  funct7, the most significant bits of the instruction
 * Input:  funct3, the middle three bits of the instruction (12-14)
 * Output: operation, What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop     = Input(UInt(2.W))
    val itype     = Input(Bool())
    val funct7    = Input(UInt(7.W))
    val funct3    = Input(UInt(3.W))

    val operation = Output(UInt(4.W))
  })
  when (io.itype === true.B){
    when (io.aluop === 0.U) {
      when (io.funct3 === "b010".U) {io.operation := "b0010".U} //LW (ADD)
      .elsewhen (io.funct3 === "b001".U) {io.operation := "b0010".U}
      .otherwise {io.operation := "b0010".U}
    }
    .elsewhen (io.aluop === 2.U){
      when (io.funct3 === "b000".U) {io.operation := "b0010".U} //ADDI
      .elsewhen (io.funct3 === "b010".U) { io.operation := "b1000".U } //SLTI
      .elsewhen (io.funct3 === "b011".U) { io.operation := "b0101".U } //SLTIU
      .elsewhen (io.funct3 === "b100".U) { io.operation := "b0110".U } //XORI
      .elsewhen (io.funct3 === "b110".U) { io.operation := "b0001".U } //ORI
      .elsewhen (io.funct3 === "b001".U) { io.operation := "b1001".U } //SLLI
      .elsewhen (io.funct3 === "b101".U && io.funct7 === "b0000000".U) {io.operation := "b0111".U} //SRLI
      .elsewhen (io.funct3 === "b101".U && io.funct7 === "b0100000".U) {io.operation := "b0100".U} //SRAI
      .otherwise { io.operation := "b0000".U } //ANDI
    }
    .otherwise {io.operation := DontCare}
  }.otherwise {
    when (io.aluop === 2.U){
      when (io.funct3 === "b000".U && io.funct7 === "b0000000".U) { io.operation := "b0010".U } //ADD
      .elsewhen (io.funct3 === "b000".U && io.funct7 === "b0100000".U) { io.operation := "b0011".U } //SUB
      .elsewhen (io.funct3 === "b001".U) { io.operation := "b1001".U } //SLL
      .elsewhen (io.funct3 === "b010".U) { io.operation := "b1000".U } //SLT
      .elsewhen (io.funct3 === "b011".U) { io.operation := "b0101".U } //SLTU
      .elsewhen (io.funct3 === "b100".U) { io.operation := "b0110".U } //XOR
      .elsewhen (io.funct3 === "b101".U && io.funct7 === "b0000000".U) { io.operation := "b0111".U } //SRL
      .elsewhen (io.funct3 === "b101".U && io.funct7 === "b0100000".U) { io.operation := "b0100".U } //SRA
      .elsewhen (io.funct3 === "b110".U) { io.operation := "b0001".U } //OR
      .otherwise { io.operation := "b0000".U } //AND
    } .elsewhen (io.aluop === 1.U){
      when (io.funct3 === "b000".U) { io.operation := "b1101".U} //BEQ
      .elsewhen (io.funct3 === "b001".U) { io.operation := "b1110".U} //BNE
      .elsewhen (io.funct3 === "b100".U) { io.operation := "b1000".U} //BLT
      .elsewhen (io.funct3 === "b101".U) { io.operation := "b1011".U} //BGE
      .elsewhen (io.funct3 === "b110".U) { io.operation := "b0101".U} //BLTU
      .elsewhen (io.funct3 === "b111".U) { io.operation := "b1100".U} //BGEU
      .otherwise {io.operation := "b1100".U} //BGEU
    }
    .otherwise {io.operation := "b0010".U}
  }
}