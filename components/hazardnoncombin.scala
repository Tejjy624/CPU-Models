// This file contains the hazard detection unit

package dinocpu.components

import chisel3._

/**
 * The hazard detection unit
 *
 * Input:  rs1, the first source register number
 * Input:  rs2, the second source register number
 * Input:  idex_memread, true if the instruction in the ID/EX register is going to read from memory
 * Input:  idex_rd, the register number of the destination register for the instruction in the ID/EX register
 * Input:  exmem_taken, if true, then we are using the nextpc in the EX/MEM register, *not* pc+4.
 * Input:  imem_ready, if true, then the Instruction Memory is ready for another instruction 
 * Input:  imem_good, if true, then an instruction was successfully retrieved and can unstall CPU
 * Input:  dmem_good, if true, then can unstall CPU for data memory
 *
 * Output: pcfromtaken, if true, use the pc from MEM
 * Output: pcstall, if true, stall the pipeline
 * Output: if_id_stall, if true, stall the if_id register. 
 * Output: if_id_flush, if true, flush the if_id register. 
 * Output: id_ex_stall, if true, stall the id_ex register. 
 * Output: id_ex_flush, if true, flush the id_ex register. 
 * Output: ex_mem_stall, if true, stall the ex_mem register. 
 * Output: ex_mem_flush, if true, flush the ex_mem register. 
 * Output: mem_wb_stall, if true, stall the mem_wb register. 
 * Output: mem_wb_flush, if true, flush the mem_wb register. 
 * Output: imem_valid, if true, imem can process new instruction
 *
 * For more information, see Section 4.7 and beginning of 4.8 of Patterson and Hennessy
 * This follows the "Data hazards and stalls" section and the "Assume branch not taken" section
 */
class HazardUnitNonCombin extends Module {
  val io = IO(new Bundle {
    val rs1          = Input(UInt(5.W))
    val rs2          = Input(UInt(5.W))
    val idex_memread = Input(Bool())
    val idex_rd      = Input(UInt(5.W))
    val exmem_taken  = Input(Bool())
    val imem_ready   = Input(Bool())
    val imem_good    = Input(Bool())
    val dmem_good    = Input(Bool())

    val pcfromtaken  = Output(Bool())
    val pcstall      = Output(Bool())
    val if_id_stall  = Output(Bool())
    val if_id_flush  = Output(Bool())
    val id_ex_stall  = Output(Bool())
    val id_ex_flush  = Output(Bool())
    val ex_mem_stall = Output(Bool())
    val ex_mem_flush = Output(Bool())
    val mem_wb_stall = Output(Bool())
    val mem_wb_flush = Output(Bool())
    val imem_valid   = Output(Bool())
  })

  // default
  io.pcfromtaken  := false.B
  io.pcstall      := false.B
  io.if_id_stall  := false.B
  io.if_id_flush  := false.B
  io.id_ex_stall  := false.B
  io.id_ex_flush  := false.B
  io.ex_mem_stall := false.B
  io.ex_mem_flush := false.B
  io.mem_wb_stall := false.B
  io.mem_wb_flush := false.B
  io.imem_valid   := true.B

  // Load to use hazard.
  when (io.idex_memread && (io.idex_rd === io.rs1 || io.idex_rd === io.rs2) && !io.exmem_taken) {
    io.pcfromtaken := false.B
    io.pcstall     := true.B
    io.if_id_stall := true.B
    io.id_ex_flush := true.B

    io.if_id_flush  := false.B
    io.id_ex_stall  := false.B
    io.ex_mem_flush := false.B
    io.ex_mem_stall := false.B
    io.mem_wb_flush := false.B
    io.mem_wb_stall := false.B
  }
  // Jumps and Branches
  when (io.exmem_taken) {
    io.pcfromtaken  := true.B // use the PC from mem stage
    io.pcstall      := false.B // use the PC from mem stage
    io.if_id_flush  := true.B
    io.id_ex_flush  := true.B
    io.ex_mem_flush := true.B

    io.if_id_stall  := false.B
    io.id_ex_stall  := false.B
    io.ex_mem_stall := false.B
    io.mem_wb_flush := false.B
    io.mem_wb_stall := false.B
  }
  // Instruction is invalid if imem not ready
  when (!io.imem_good) { //(io.imem_ready === false.B) && 
    io.pcstall := true.B
    io.if_id_stall := true.B
    io.id_ex_flush := true.B

    io.if_id_flush  := false.B
    io.id_ex_stall  := false.B
    io.ex_mem_flush := false.B
    io.ex_mem_stall := false.B
    io.mem_wb_flush := false.B
    io.mem_wb_stall := false.B
  }

  when (!io.imem_ready) {
    io.imem_valid := false.B
  }
  // Stall all stages when dmem is busy
  when (!io.dmem_good) { //|| (!io.imem_ready)) {
    io.pcstall := true.B
    io.if_id_stall := true.B
    io.id_ex_stall := true.B
    io.ex_mem_stall := true.B
    io.mem_wb_stall := true.B

    io.if_id_flush  := false.B
    io.id_ex_flush  := false.B
    io.ex_mem_flush := false.B
    io.mem_wb_flush := false.B
    //io.imem_valid := false.B
  }
  assert(!(io.if_id_stall & io.if_id_flush))
  assert(!(io.id_ex_stall & io.id_ex_flush))
  assert(!(io.ex_mem_stall & io.ex_mem_flush))
  assert(!(io.mem_wb_stall & io.mem_wb_flush))
  assert(!(io.pcstall & io.pcfromtaken))
}