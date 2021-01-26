// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu.pipelined

import chisel3._
import chisel3.util._
import dinocpu._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.6 of Patterson and Hennessy
 * This follows figure 4.49
 */
class PipelinedCPU(implicit val conf: CPUConfig) extends BaseCPU {

  // Everything in the register between IF and ID stages
  class IFIDBundle extends Bundle {
    val instruction = UInt(32.W)
    val pc          = UInt(32.W)
    val pcplusfour  = UInt(32.W)
  }

  // Control signals used in EX stage
  class EXControl extends Bundle {
    val itype        = Bool()
    val aluop        = UInt(2.W)
    val resultselect = UInt(2.W)
    val alusrc       = Bool()
    val pcadd        = Bool()
    val branch       = Bool() 
    val jump         = Bool() 
    val pcfromalu    = Bool()
  }

  // Control signals used in MEM stage
  class MControl extends Bundle {
    val memread = Bool()
    val memwrite = Bool()
    val maskmode = UInt(2.W)
    val sext = Bool()
    val taken = Bool()
  }

  // Control signals used in WB stage
  class WBControl extends Bundle {
    val regwrite = Bool()
    val toreg = Bool()
  }

  // Data of the the register between ID and EX stages
  class IDEXBundle extends Bundle {
    val pcplusfour = UInt(32.W)
    val pc = UInt(32.W)
    val imm = UInt(32.W)
    val funct3 = UInt(3.W)
    val funct7 = UInt(7.W)
    val readdata1 = UInt(32.W)
    val readdata2 = UInt(32.W)
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
    val writereg = UInt(5.W)
  }

  // Control block of the IDEX register
  class IDEXControl extends Bundle {
    val ex_ctrl  = new EXControl
    val mem_ctrl = new MControl
    val wb_ctrl  = new WBControl
  }

  // Everything in the register between EX and MEM stages
  class EXMEMBundle extends Bundle {
    val result = UInt(32.W)
    val imm = UInt(32.W)
    val writereg = UInt(5.W)
    val next_pc = UInt(32.W)//Not needed for part 1(According to campuswire)
    val writedata = UInt(32.W)
  }

  // Control block of the EXMEM register
  class EXMEMControl extends Bundle {
    val mem_ctrl  = new MControl
    val wb_ctrl   = new WBControl
  }

  // Everything in the register between MEM and WB stages
  class MEMWBBundle extends Bundle {
    val readdata = UInt(32.W)
    val result = UInt(32.W)
    val writereg = UInt(5.W)

  }

  // Control block of the MEMWB register
  class MEMWBControl extends Bundle {
    val wb_ctrl = new WBControl
  }

  // All of the structures required
  val pc         = RegInit(0.U)
  val control    = Module(new Control())
  val registers  = Module(new RegisterFile())
  val aluControl = Module(new ALUControl())
  val alu        = Module(new ALU())
  val immGen     = Module(new ImmediateGenerator())
  val pcPlusFour = Module(new Adder())
  val branchAdd  = Module(new Adder())
  val forwarding = Module(new ForwardingUnit())  //pipelined only
  val hazard     = Module(new HazardUnit())      //pipelined only
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  // The four pipeline registers
  val if_id       = Module(new StageReg(new IFIDBundle))

  val id_ex       = Module(new StageReg(new IDEXBundle))
  val id_ex_ctrl  = Module(new StageReg(new IDEXControl))

  val ex_mem      = Module(new StageReg(new EXMEMBundle))
  val ex_mem_ctrl = Module(new StageReg(new EXMEMControl))

  val mem_wb      = Module(new StageReg(new MEMWBBundle))
  // To make the interface of the mem_wb_ctrl register consistent with the other control
  // registers, we create an anonymous Bundle
  val mem_wb_ctrl = Module(new StageReg(new MEMWBControl))

    // Remove these as you hook up each one
    //registers.io  := DontCare
    //aluControl.io := DontCare
    //alu.io        := DontCare
    //immGen.io     := DontCare
    //pcPlusFour.io := DontCare
    //branchAdd.io  := DontCare
    //io.dmem       := DontCare
    //forwarding.io := DontCare
    //hazard.io     := DontCare

    //id_ex.io       := DontCare
    //id_ex_ctrl.io  := DontCare
    //ex_mem.io      := DontCare
    //ex_mem_ctrl.io := DontCare
    //mem_wb.io      := DontCare
    //mem_wb_ctrl.io := DontCare

  // Forward declaration of wires that connect different stages

  // From memory back to fetch. Since we don't decide whether to take a branch or not until the memory stage.
  val next_pc      = Wire(UInt())
  //next_pc         := DontCare     // Remove when connected

  val write_data = Wire(UInt())
  //write_data    := DontCare       // Remove when connected

  /////////////////////////////////////////////////////////////////////////////
  // FETCH STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Note: This comes from the memory stage!
  // Only update the pc if pcstall is false
  pc :=  Mux(hazard.io.pcstall, pc,
          Mux(hazard.io.pcfromtaken, next_pc, pcPlusFour.io.result))
  //pcPlusFour.io.result//next_pc//ex_mem.io.data.next_pc//

  // Send the PC to the instruction memory port to get the instruction
  io.imem.address := pc
  io.imem.valid   := !hazard.io.if_id_stall

  // Get the PC + 4
  pcPlusFour.io.inputx := pc
  pcPlusFour.io.inputy := 4.U

  // Fill the IF/ID register if we are not bubbling IF/ID
  // otherwise, leave the IF/ID register *unchanged*
  if_id.io.in.instruction := io.imem.instruction
  if_id.io.in.pc          := pc
  if_id.io.in.pcplusfour  := pcPlusFour.io.result
  if_id.io.valid := !hazard.io.if_id_stall
  if_id.io.flush := hazard.io.if_id_flush

  /////////////////////////////////////////////////////////////////////////////
  // ID STAGE
  /////////////////////////////////////////////////////////////////////////////

  val rs1 = if_id.io.data.instruction(19,15)
  val rs2 = if_id.io.data.instruction(24,20)

  // Send input from this stage to hazard detection unit
  hazard.io.rs1 := rs1
  hazard.io.rs2 := rs2

  // Send opcode to control
  control.io.opcode := if_id.io.data.instruction(6,0)

  // Send register numbers to the register file
  registers.io.readreg1 := rs1
  registers.io.readreg2 := rs2

  // Send the instruction to the immediate generator
  immGen.io.instruction := if_id.io.data.instruction

  // FIll the id_ex register
  id_ex.io.in.pcplusfour  := if_id.io.data.pcplusfour
  id_ex.io.in.pc          := if_id.io.data.pc
  id_ex.io.in.imm         := immGen.io.sextImm
  id_ex.io.in.funct3      := if_id.io.data.instruction(14,12)
  id_ex.io.in.funct7      := if_id.io.data.instruction(31,25)
  id_ex.io.in.readdata1   := registers.io.readdata1
  id_ex.io.in.readdata2   := registers.io.readdata2
  id_ex.io.in.writereg    := if_id.io.data.instruction(11,7)
  id_ex.io.in.rs1         := rs1
  id_ex.io.in.rs2         := rs2
  id_ex.io.valid := true.B
  id_ex.io.flush := hazard.io.id_ex_flush

  // Set the execution control signals
  id_ex_ctrl.io.in.ex_ctrl.alusrc       := control.io.alusrc
  id_ex_ctrl.io.in.ex_ctrl.pcadd        := control.io.pcadd
  id_ex_ctrl.io.in.ex_ctrl.aluop        := control.io.aluop
  id_ex_ctrl.io.in.ex_ctrl.itype        := control.io.itype
  id_ex_ctrl.io.in.ex_ctrl.resultselect := control.io.resultselect
  id_ex_ctrl.io.in.ex_ctrl.branch       := control.io.branch
  id_ex_ctrl.io.in.ex_ctrl.jump         := control.io.jump
  id_ex_ctrl.io.in.ex_ctrl.pcfromalu    := control.io.pcfromalu
  id_ex_ctrl.io.valid := true.B
  

  // Set the memory control signals
  id_ex_ctrl.io.in.mem_ctrl.memread   := control.io.memread
  id_ex_ctrl.io.in.mem_ctrl.memwrite  := control.io.memwrite
  id_ex_ctrl.io.in.mem_ctrl.maskmode  := if_id.io.data.instruction(13,12)
  id_ex_ctrl.io.in.mem_ctrl.sext      := ~if_id.io.data.instruction(14)
  id_ex_ctrl.io.in.mem_ctrl.taken    := false.B

  // Set the writeback control signals
  id_ex_ctrl.io.in.wb_ctrl.regwrite   := control.io.regwrite
  id_ex_ctrl.io.in.wb_ctrl.toreg      := control.io.toreg

  id_ex_ctrl.io.flush := hazard.io.id_ex_flush

  /////////////////////////////////////////////////////////////////////////////
  // EX STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Set the inputs to the hazard detection unit from this stage (SKIP FOR PART I)
  hazard.io.idex_memread := id_ex_ctrl.io.data.mem_ctrl.memread
  hazard.io.idex_rd := id_ex.io.data.writereg

  // Set the input to the forwarding unit from this stage (SKIP FOR PART I)
  forwarding.io.rs1 := id_ex.io.data.rs1
  forwarding.io.rs2 := id_ex.io.data.rs2

  // Connect the ALU control wires (line 50 of single-cycle/cpu.scala)
  aluControl.io.aluop  := id_ex_ctrl.io.data.ex_ctrl.aluop
  aluControl.io.itype  := id_ex_ctrl.io.data.ex_ctrl.itype
  aluControl.io.funct7 := id_ex.io.data.funct7
  aluControl.io.funct3 := id_ex.io.data.funct3

  // Insert the forward inputx mux here (SKIP FOR PART I)
  val forwardxmux = Wire(UInt(32.W))
  forwardxmux := MuxCase(0.U, Array(
    (forwarding.io.forwardA === 0.U) -> id_ex.io.data.readdata1,
    (forwarding.io.forwardA === 1.U) -> ex_mem.io.data.result,
    (forwarding.io.forwardA === 2.U) -> write_data,
    (forwarding.io.forwardA === 3.U) -> ex_mem.io.data.imm))
  
  // Insert the forward inputy mux here (SKIP FOR PART I)
  val forwardymux = Wire(UInt(32.W))
  forwardymux := MuxCase(0.U, Array(
    (forwarding.io.forwardB === 0.U) -> id_ex.io.data.readdata2,
    (forwarding.io.forwardB === 1.U) -> ex_mem.io.data.result,
    (forwarding.io.forwardB === 2.U) -> write_data,
    (forwarding.io.forwardB === 3.U) -> ex_mem.io.data.imm))

  // Input x mux
  alu.io.inputx := Mux(id_ex_ctrl.io.data.ex_ctrl.pcadd, id_ex.io.data.pc, forwardxmux)

  // Input y mux (line 63 of single-cycle/cpu.scala)
  alu.io.inputy := Mux(id_ex_ctrl.io.data.ex_ctrl.alusrc, id_ex.io.data.imm, forwardymux)

  // Set the ALU operation
  alu.io.operation := aluControl.io.operation
  
  // Connect the branchAdd unit
  branchAdd.io.inputx := id_ex.io.data.pc
  branchAdd.io.inputy := id_ex.io.data.imm

  // Set the EX/MEM register values
  ex_mem_ctrl.io.in.mem_ctrl := id_ex_ctrl.io.data.mem_ctrl
  ex_mem_ctrl.io.in.wb_ctrl := id_ex_ctrl.io.data.wb_ctrl
  ex_mem.io.in.imm := id_ex.io.data.imm
  ex_mem.io.in.writereg := id_ex.io.data.writereg
  ex_mem.io.in.writedata := forwardymux//id_ex.io.data.readdata2//alu.io.inputy//
  ex_mem.io.valid := true.B
  ex_mem.io.flush := false.B
  ex_mem_ctrl.io.valid := true.B
  
  
  // Calculate whether which PC we should use and set the taken flag (line 96 in single-cycle/cpu.scala)
  // Branch Add Mux
  ex_mem.io.in.next_pc := Mux(id_ex_ctrl.io.data.ex_ctrl.pcfromalu, alu.io.result, branchAdd.io.result)//id_ex.io.data.pc//
  
  when ((id_ex_ctrl.io.data.ex_ctrl.jump) || (id_ex_ctrl.io.data.ex_ctrl.branch && alu.io.result(0))) {
    ex_mem_ctrl.io.in.mem_ctrl.taken := true.B
  } .otherwise {
    ex_mem_ctrl.io.in.mem_ctrl.taken := false.B
  }
  // Determine which result to use 
  // ALU Result Mux
  val m_result = MuxCase(0.U, Array(
      (id_ex_ctrl.io.data.ex_ctrl.resultselect === 0.U) -> alu.io.result,
      (id_ex_ctrl.io.data.ex_ctrl.resultselect === 1.U) -> id_ex.io.data.imm,
      (id_ex_ctrl.io.data.ex_ctrl.resultselect === 2.U) -> id_ex.io.data.pcplusfour))
  ex_mem.io.in.result := m_result
  ex_mem_ctrl.io.flush := hazard.io.ex_mem_flush

  /////////////////////////////////////////////////////////////////////////////
  // MEM STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Set data memory IO (line 75 of single-cycle/cpu.scala)
  io.dmem.address   := ex_mem.io.data.result
  io.dmem.writedata := ex_mem.io.data.writedata
  io.dmem.memread   := ex_mem_ctrl.io.data.mem_ctrl.memread
  io.dmem.memwrite  := ex_mem_ctrl.io.data.mem_ctrl.memwrite
  io.dmem.maskmode  := ex_mem_ctrl.io.data.mem_ctrl.maskmode
  io.dmem.sext      := ex_mem_ctrl.io.data.mem_ctrl.sext
  when(io.dmem.memread || io.dmem.memwrite) {
    io.dmem.valid := true.B
  } .otherwise {
    io.dmem.valid := false.B
  }

  // Send next_pc back to the fetch stage
  next_pc := ex_mem.io.data.next_pc

  // Send input signals to the hazard detection unit (SKIP FOR PART I)
  hazard.io.exmem_taken := ex_mem_ctrl.io.data.mem_ctrl.taken

  // Send input signals to the forwarding unit (SKIP FOR PART I)
  forwarding.io.exmemrd := ex_mem.io.data.writereg
  forwarding.io.exmemrw := ex_mem_ctrl.io.data.wb_ctrl.regwrite

  // Wire the MEM/WB register
  mem_wb_ctrl.io.in.wb_ctrl := ex_mem_ctrl.io.data.wb_ctrl
  mem_wb.io.in.writereg := ex_mem.io.data.writereg
  mem_wb.io.in.result := ex_mem.io.data.result
  mem_wb.io.in.readdata := io.dmem.readdata
  mem_wb.io.valid := true.B
  mem_wb.io.flush := false.B
  mem_wb_ctrl.io.valid := true.B
  mem_wb_ctrl.io.flush := false.B

  /////////////////////////////////////////////////////////////////////////////
  // WB STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Set the writeback data mux (line 88 single-cycle/cpu.scala)
  write_data := MuxCase(0.U, Array(
                        (mem_wb_ctrl.io.data.wb_ctrl.toreg === 0.U) -> mem_wb.io.data.result,
                        (mem_wb_ctrl.io.data.wb_ctrl.toreg === 1.U) -> mem_wb.io.data.readdata))
  
  // Write the data to the register file
  registers.io.wen := (mem_wb_ctrl.io.data.wb_ctrl.regwrite) && (registers.io.writereg =/= 0.U)
  registers.io.writereg := mem_wb.io.data.writereg
  registers.io.writedata := write_data

  // Set the input signals for the forwarding unit (SKIP FOR PART I)
  forwarding.io.memwbrd := mem_wb.io.data.writereg
  forwarding.io.memwbrw := mem_wb_ctrl.io.data.wb_ctrl.regwrite
}

/*
 * Object to make it easier to print information about the CPU
 */
object PipelinedCPUInfo {
  def getModules(): List[String] = {
    List(
      "imem",
      "dmem",
      "control",
      "branchCtrl",
      "registers",
      "aluControl",
      "alu",
      "immGen",
      "pcPlusFour",
      "branchAdd",
      "forwarding",
      "hazard",
    )
  }
  def getPipelineRegs(): List[String] = {
    List(
      "if_id",
      "id_ex",
      "id_ex_ctrl",
      "ex_mem",
      "ex_mem_ctrl",
      "mem_wb",
      "mem_wb_ctrl"
    )
  }
}
