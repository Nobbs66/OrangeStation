'ALU Operations
Declare Sub iop_log_ADD()
Declare Sub iop_log_ADDI()
Declare Sub iop_log_ADDIU()
Declare Sub iop_log_ADDU()
Declare Sub iop_log_AND()
Declare Sub iop_log_ANDI()
Declare Sub iop_log_DIV()
Declare Sub iop_log_DIVU()
Declare Sub iop_log_MULT()
Declare Sub iop_log_MULTU()
Declare Sub iop_log_NOR()
Declare Sub iop_log_OR()
Declare Sub iop_log_ORI()
Declare Sub iop_log_SLL()
Declare Sub iop_log_SLLV()
Declare Sub iop_log_SRA()
Declare Sub iop_log_SRAV()
Declare Sub iop_log_SRL()
Declare Sub iop_log_SRLV()
Declare Sub iop_log_SUB()
Declare Sub iop_log_SUBU()
Declare Sub iop_log_XOR()
Declare Sub iop_log_XORI()
'Read Modify Write Operations
Declare Sub iop_log_LB()
Declare Sub iop_log_LBU()
Declare Sub iop_log_LH()
Declare Sub iop_log_LHU()
Declare Sub iop_log_LUI()
Declare Sub iop_log_LW()
Declare Sub iop_log_LWL()
Declare Sub iop_log_LWR()
Declare Sub iop_log_MFHI()
Declare Sub iop_log_MFLO()
Declare Sub iop_log_MTLO()
Declare Sub iop_log_MTHI()
Declare Sub iop_log_SB()
Declare Sub iop_log_SH()
Declare Sub iop_log_SW()
Declare Sub iop_log_SWL()
Declare Sub iop_log_SWR()
'Misc. Operations
Declare Sub iop_log_SLT()
Declare Sub iop_log_SLTI()
Declare Sub iop_log_SLTIU()
Declare Sub iop_log_SLTU()
Declare Sub iop_log_SYSCALL()
'Logic Flow Operations
Declare Sub iop_log_BEQ()
Declare Sub iop_log_BGEZ()
Declare Sub iop_log_BGEZAL()
Declare Sub iop_log_BGTZ()
Declare Sub iop_log_BLEZ()
Declare Sub iop_log_BLTZ()
Declare Sub iop_log_BLTZAL()
Declare Sub iop_log_BNE()
Declare Sub iop_log_BREAK()
Declare Sub iop_log_JMP()
Declare Sub iop_log_JAL()
Declare Sub iop_log_JALR()
Declare Sub iop_log_JR()
Declare Sub iop_log_NULL()
Declare Sub iop_log_COP0()
Declare Sub iop_log_COP2()
Declare Sub iop_log_LWC2()
Declare Sub iop_log_SWC2()
Declare Sub iop_log_REGIMM()
Declare Sub iop_log_SPECIAL()
Dim Shared tbl_SPECIAL(0 To 63) As Sub() => _
{																														  _
@iop_log_SLL 	, @CPU_NULL 	, @iop_log_SRL , @iop_log_SRA , @iop_log_SLLV   , @iop_log_NULL , @iop_log_SRLV, @iop_log_SRAV,_
@iop_log_JR  	, @iop_log_JALR 	, @iop_log_NULL, @iop_log_NULL, @iop_log_SYSCALL, @iop_log_BREAK, @iop_log_NULL, @iop_log_NULL,_
@iop_log_MFHI	, @iop_log_MTHI 	, @iop_log_MFLO, @iop_log_MTLO, @iop_log_NULL   , @iop_log_NULL , @iop_log_NULL, @iop_log_NULL,_
@iop_log_MULT	, @iop_log_MULTU	, @iop_log_DIV , @iop_log_DIVU, @iop_log_NULL   , @iop_log_NULL , @iop_log_NULL, @iop_log_NULL,_
@iop_log_ADD 	, @iop_log_ADDU 	, @iop_log_SUB , @iop_log_SUBU, @iop_log_AND    , @iop_log_OR   , @iop_log_XOR , @iop_log_NOR ,_
@iop_log_NULL	, @iop_log_NULL 	, @iop_log_SLT , @iop_log_SLTU, @iop_log_NULL   , @iop_log_NULL , @iop_log_NULL, @iop_log_NULL,_
@iop_log_NULL	, @iop_log_NULL 	, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL   , @iop_log_NULL , @iop_log_NULL, @iop_log_NULL,_
@iop_log_NULL	, @iop_log_NULL 	, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL   , @iop_log_NULL , @iop_log_NULL, @iop_log_NULL _
}
Dim Shared tbl_REGIMM(0 To 31) As Sub () => _
{																														 _
@iop_log_BLTZ  , @iop_log_BGEZ  , @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL,_
@iop_log_NULL  , @iop_log_NULL  , @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL,_
@iop_log_BLTZAL, @iop_log_BGEZAL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL,_
@iop_log_NULL  , @iop_log_NULL  , @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL}
Dim Shared tbl_NORMAL(0 To 63) As Sub () => _
{																															_
@iop_log_SPECIAL, @iop_log_REGIMM, @iop_log_JMP , @iop_log_JAL  , @iop_log_BEQ , @iop_log_BNE , @iop_log_BLEZ, @iop_log_BGTZ,_
@iop_log_ADDI   , @iop_log_ADDIU , @iop_log_SLTI, @iop_log_SLTIU, @iop_log_ANDI, @iop_log_ORI , @iop_log_XORI, @iop_log_LUI ,_
@iop_log_COP0   , @iop_log_NULL  , @iop_log_COP2, @iop_log_NULL , @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL,_
@iop_log_NULL   , @iop_log_NULL  , @iop_log_NULL, @iop_log_NULL , @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL,_
@iop_log_LB     , @iop_log_LH    , @iop_log_LWL , @iop_log_LW   , @iop_log_LBU , @iop_log_LHU , @iop_log_LWR , @iop_log_NULL,_
@iop_log_SB     , @iop_log_SH    , @iop_log_SWL , @iop_log_SW   , @iop_log_NULL, @iop_log_NULL, @iop_log_SWR , @iop_log_NULL,_
@iop_log_NULL   , @iop_log_NULL  , @iop_log_LWC2, @iop_log_NULL , @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL,_
@iop_log_NULL   , @iop_log_NULL  , @iop_log_SWC2, @iop_log_NULL , @iop_log_NULL, @iop_log_NULL, @iop_log_NULL, @iop_log_NULL}
Dim Shared As String * 4 iop_log_regs(0 To 31) = _
{"zero", "at", "v0", "v1", _
"a0", "a1", "a2", "a3", _
"t0", "t1", "t2", "t3", _
"t4", "t5", "t6", "t7", _
"s0", "s1", "s2", "s3", _
"s4", "s5", "s6", "s7", _
"t8", "t9", "k0", "k1", _
"gp", "sp", "fp", "ra"} 



Sub iop_log_ADD()
	Print #11, hex(cpu.pc) & ": " & "ADDU " & iop_log_regs(rd)& "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_ADDI()
	Print #11, hex(cpu.pc) & ": " & "ADDIU " & iop_log_regs(rt) & "," & iop_log_regs(rs) & "," & CShort(imm)
End Sub
Sub iop_log_ADDIU()
	Print #11, hex(cpu.pc) & ": " & "ADDIU " & iop_log_regs(rt) & "," & iop_log_regs(rs) & "," & CShort(imm)
End Sub
Sub iop_log_ADDU()
	Print #11, hex(cpu.pc) & ": " & "ADDU " & iop_log_regs(rd)& "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_AND()
	Print #11, hex(cpu.pc) & ": " & "AND " & iop_log_regs(rd)& "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_ANDI()
	Print #11, hex(cpu.pc) & ": " & "ANDI " & iop_log_regs(rt) & "," & iop_log_regs(rs) & "," & CShort(imm)
End Sub
Sub iop_log_DIV()
	Print #11, hex(cpu.pc) & ": " & "DIV " & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_DIVU()
	Print #11, hex(cpu.pc) & ": " & "DIV " & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_MULT()
	Print #11, hex(cpu.pc) & ": " & "DIV " & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_MULTU()
	Print #11, hex(cpu.pc) & ": " & "DIV " & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_NOR()
	Print #11, hex(cpu.pc) & ": " & "NOR " & iop_log_regs(rd) & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_OR()
	Print #11, hex(cpu.pc) & ": " & "OR " & iop_log_regs(rd) & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_ORI()
	Print #11, hex(cpu.pc) & ": " & "ORI " & iop_log_regs(rt) & "," & iop_log_regs(rs) & "," & CShort(imm)
End Sub
Sub iop_log_SLL()
	If cpu.opcode = 0 Then 
		Print #11, Hex(cpu.pc) & ": " & "NOP"
	Else 
		Print #11, hex(cpu.pc) & ": " & "SLL " & iop_log_regs(rd) & "," & iop_log_regs(rt) & "," & sa 
	EndIf
End Sub
Sub iop_log_SLLV()
	Print #11, hex(cpu.pc) & ": " & "SLLV " & iop_log_regs(rd) & "," & iop_log_regs(rt) & "," & iop_log_regs(rs)
End Sub
Sub iop_log_SRA()
	Print #11, hex(cpu.pc) & ": " & "SRA " & iop_log_regs(rd) & "," & iop_log_regs(rt) & "," & sa 
End Sub
Sub iop_log_SRAV()
	Print #11, hex(cpu.pc) & ": " & "SRAV " & "," & iop_log_regs(rd) & "," & iop_log_regs(rt) & "," & iop_log_regs(rs)
End Sub
Sub iop_log_SRL()
	Print #11, hex(cpu.pc) & ": " & "SRL " & iop_log_regs(rd) & "," & iop_log_regs(rt) & "," & sa 
End Sub
Sub iop_log_SRLV()
	Print #11, hex(cpu.pc) & ": " & "SRLV " & iop_log_regs(rd) & "," & iop_log_regs(rt) & "," & iop_log_regs(rs)
End Sub
Sub iop_log_SUB()
	Print #11, hex(cpu.pc) & ": " & "SUB " & iop_log_regs(rd) & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_SUBU()
	Print #11, hex(cpu.pc) & ": " & "SUBU " & iop_log_regs(rd) & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_XOR()
	Print #11, hex(cpu.pc) & ": " & "XOR " & iop_log_regs(rd) & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_XORI()
	Print #11, hex(cpu.pc) & ": " & "XORI " & iop_log_regs(rt) & "," & iop_log_regs(rs) & "," & CShort(imm)
End Sub
'Read Modify Write Operations
Sub iop_log_LB()
	Print #11, hex(cpu.pc) & ": " & "LB " & "," & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub iop_log_LBU()
	Print #11, hex(cpu.pc) & ": " & "LBU " & "," & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub iop_log_LH()
	Print #11, hex(cpu.pc) & ": " & "LH " & "," & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub iop_log_LHU()
	Print #11, hex(cpu.pc) & ": " & "LHU " & "," & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub iop_log_LUI()
	Print #11, hex(cpu.pc) & ": " & "LUI " & "," & iop_log_regs(rt) & "," & Hex(imm Shl 16)
End Sub
Sub iop_log_LW()
	Print #11, hex(cpu.pc) & ": " & "LW " & iop_log_regs(rt) & "," & Hex(imm) & "," & iop_log_regs(rs)
End Sub
Sub iop_log_LWL()
	Print #11, hex(cpu.pc) & ": " & "LWL " & "," & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub iop_log_LWR()
	Print #11, hex(cpu.pc) & ": " & "LWR " & "," & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub iop_log_MFHI()
	Print #11, hex(cpu.pc) & ": " & "MFHI " & iop_log_regs(rd)
End Sub
Sub iop_log_MFLO()
	Print #11, hex(cpu.pc) & ": " & "MFLO " & iop_log_regs(rd)
End Sub
Sub iop_log_MTLO()
	Print #11, hex(cpu.pc) & ": " & "MTLO " & iop_log_regs(rs)
End Sub
Sub iop_log_MTHI()
	Print #11, hex(cpu.pc) & ": " & "MTHI " & iop_log_regs(rs)
End Sub
Sub iop_log_SB()
	Print #11, hex(cpu.pc) & ": " & "SB " & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub iop_log_SH()
	Print #11, hex(cpu.pc) & ": " & "SH " & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub iop_log_SW()
	Print #11, hex(cpu.pc) & ": " & "SW " & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub iop_log_SWL()
	Print #11, hex(cpu.pc) & ": " & "SWL " & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub iop_log_SWR()
	Print #11, hex(cpu.pc) & ": " & "SWR " & iop_log_regs(rt) & "," & Hex(CShort(imm))
End Sub
'Misc. Operations
Sub iop_log_SLT()
	Print #11, hex(cpu.pc) & ": " & "SLT " & iop_log_regs(rd) & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_SLTI()
	Print #11, hex(cpu.pc) & ": " & "SLTIU " & iop_log_regs(rt) & "," & iop_log_regs(rs) & "," & CShort(imm)
End Sub
Sub iop_log_SLTIU()
	Print #11, hex(cpu.pc) & ": " & "SLTIU " & iop_log_regs(rt) & "," & iop_log_regs(rs) & "," & CShort(imm)
End Sub
Sub iop_log_SLTU()
	Print #11, hex(cpu.pc) & ": " & "SLT " & iop_log_regs(rd) & "," & iop_log_regs(rs) & "," & iop_log_regs(rt)
End Sub
Sub iop_log_SYSCALL()
	Print #11, Hex(cpu.pc) & ": " & "SYSCALL: "
	Print "Syscall Reached"
	sleep
End Sub
'Logic Flow Operations
Sub iop_log_BEQ()
	Print #11, hex(cpu.pc) & ": " & "BEQ " & iop_log_regs(rs) & "," & iop_log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub iop_log_BGEZ()
	Print #11, hex(cpu.pc) & ": " & "BGEZ " & iop_log_regs(rs) & "," & iop_log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub iop_log_BGEZAL()
	Print #11, hex(cpu.pc) & ": " & "BGEZAL " & iop_log_regs(rs) & "," & iop_log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub iop_log_BGTZ()
	Print #11, hex(cpu.pc) & ": " & "BGTZ " & iop_log_regs(rs) & "," & iop_log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub iop_log_BLEZ()
	Print #11, hex(cpu.pc) & ": " & "BLEZ " & iop_log_regs(rs) & "," & iop_log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub iop_log_BLTZ()
	Print #11, hex(cpu.pc) & ": " & "BLTZ " & iop_log_regs(rs) & "," & iop_log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub iop_log_BLTZAL()
	Print #11, hex(cpu.pc) & ": " & "BLTZAL " & iop_log_regs(rs) & "," & iop_log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub iop_log_BNE()
	Print #11, hex(cpu.pc) & ": " & "BNE " & iop_log_regs(rs) & "," & iop_log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub iop_log_BREAK()
	Print #11, Hex(cpu.pc) & ": " & "[Critical] Breakpoint exception!"
End Sub
Sub iop_log_JMP()
	Print #11, hex(cpu.pc) & ": " & "J " & " " & Hex(jTarget)
End Sub
Sub iop_log_JAL()
	Print #11, hex(cpu.pc) & ": " & "JAL " & " " & Hex(jTarget)
End Sub
Sub iop_log_JALR()
	Print #11, hex(cpu.pc) & ": " & "JALR " & " " & iop_log_regs(rs)
End Sub
Sub iop_log_JR()
	Print #11, hex(cpu.pc) & ": " & "JR " & " " & iop_log_regs(rs)
End Sub
Sub iop_log_NULL()
	Print #11, Hex(cpu.pc) & ": " & "[Critical] Invalid Opcode: 0x" & Hex(cpu.opcode)
End Sub
Sub iop_log_COP0()
	
End Sub
Sub iop_log_COP2()
	
End Sub
Sub iop_log_LWC2()
	
End Sub
Sub iop_log_SWC2()
	
End Sub
Sub iop_log_REGIMM()
	tbl_REGIMM(rt)()
End Sub
Sub iop_log_SPECIAL()
	tbl_SPECIAL(cpu.opcode And &h3F)()
End Sub
Sub logOpcodes()
	Dim As UByte  l_rs (iop.opcode Shr 21) And &h1F
	Dim As UByte  l_rt (iop.opcode Shr 16) And &h1F
	Dim As UByte  l_rd (iop.opcode Shr 11) And &h1F
	Dim As UByte  l_sa ((iop.opcode Shr 6) And &h1F)
	Dim As ushort l_imm (iop.opcode And &hFFFF)
	Dim As Ulong jtarget ((iop.opcode And &h3FFFFFF) Shl 2)
	tbl_NORMAL(cpu.opcode Shr 26)()
End Sub
