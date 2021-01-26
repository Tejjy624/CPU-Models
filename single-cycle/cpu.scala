// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc         = RegInit(0.U)
  val control    = Module(new Control())
  val registers  = Module(new RegisterFile())
  val aluControl = Module(new ALUControl())
  val alu        = Module(new ALU())
  val immGen     = Module(new ImmediateGenerator())
  val pcPlusFour = Module(new Adder())
  val branchAdd  = Module(new Adder())
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  // To make the FIRRTL compiler happy. Remove this as you connect up the I/O's
  //control.io    := DontCare
  //registers.io  := DontCare
  //aluControl.io := DontCare
  //alu.io        := DontCare
  //immGen.io     := DontCare
  //pcPlusFour.io := DontCare
  //branchAdd.io  := DontCare
  //io.dmem       := DontCare

  //Inst Memory
  io.imem.address := pc
  io.imem.valid := true.B

  //PCPlusFour Adder
  pcPlusFour.io.inputx := pc
  pcPlusFour.io.inputy := 4.U

  val instruction = io.imem.instruction

  //Control Unit
  control.io.opcode := instruction(6,0)

  //Immediate Gen
  immGen.io.instruction := instruction(31,0)

  //Register File
  registers.io.readreg1 := instruction(19,15)
  registers.io.readreg2 := instruction(24,20)
  registers.io.writereg := instruction(11,7)
  registers.io.wen      := (registers.io.writereg =/= 0.U)
  

  //ALU Control
  aluControl.io.aluop  := control.io.aluop
  aluControl.io.itype  := control.io.itype
  aluControl.io.funct7 := instruction(31,25)
  aluControl.io.funct3 := instruction(14,12)

  //Branch Adder
  branchAdd.io.inputx := pc
  branchAdd.io.inputy := immGen.io.sextImm

  //ALU
  alu.io.operation := aluControl.io.operation

  //ALU Inputx Mux
  val inputx = Mux(control.io.pcadd, pc, registers.io.readdata1)
  alu.io.inputx := inputx
  //alu.io.inputx := registers.io.readdata1
  
  //ALU Inputy Mux
  val inputy = Mux(control.io.alusrc, immGen.io.sextImm, registers.io.readdata2)
  alu.io.inputy := inputy

  //Result Mux
  val execute = MuxCase(alu.io.result,
    Array((control.io.resultselect === 1.U) -> immGen.io.sextImm,
      (control.io.resultselect === 2.U) -> pcPlusFour.io.result))
  
  //Branch Mux
  //val bmux = Mux(alu.io.result(0), branchAdd.io.result, pcPlusFour.io.result)
  when (control.io.branch === true.B) {
    when (alu.io.result(0) === false.B) {
      when (control.io.jump === true.B) {pc := alu.io.result}
      .otherwise {pc := pcPlusFour.io.result}
    } .otherwise {pc := branchAdd.io.result}
  } .otherwise {pc := pcPlusFour.io.result}
  //{pc := bmux
  //}.otherwise {pc := pcPlusFour.io.result}
    
  //Jump Mux
  //when (control.io.jump === true.B){
  //  val jmux = alu.io.result//Mux(control.io.jump, alu.io.result, bmux)
  //} .otherwise {val jmux = bmux}
  //pc := jmux

  //when (control.io.jump === false.B) {pc := pcPlusFour.io.result}
  //.otherwise {pc := jmux}
  //pc := jmux


  //Data Memory
  io.dmem.address := alu.io.result
  io.dmem.memread := control.io.memread
  io.dmem.memwrite := control.io.memwrite
  io.dmem.valid := control.io.validinst
  io.dmem.maskmode := instruction(13,12)
  io.dmem.sext := ~instruction(14)
  io.dmem.writedata := registers.io.readdata2
 
  //Data Mux
  val writedmem = Mux(control.io.toreg === 0.U, execute, io.dmem.readdata)
  registers.io.writedata := writedmem

  // Debug statements here
  printf(p"DBG: CYCLE=$cycleCount\n")
  printf(p"DBG: pc: $pc\n")
  printf(p"DBG: alu: ${alu.io}\n")
}

/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "branchCtrl",
      "pcPlusFour",
      "branchAdd"
    )
  }
}
