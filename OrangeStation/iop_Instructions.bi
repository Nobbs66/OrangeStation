'#Define rs (iop.opcode Shr 21) And &h1F
'#Define rt (iop.opcode Shr 16) And &h1F
'#Define rd (iop.opcode Shr 11) And &h1F
'#Define sa ((iop.opcode Shr 6) And &h1F)
'#Define imm (iop.opcode And &hFFFF)
'#Define jtarget ((iop.opcode And &h3FFFFFF) Shl 2)
'#Define _logging
'ALU Operations
Declare Sub IOP_ADD()
Declare Sub IOP_ADDI()
Declare Sub IOP_ADDIU()
Declare Sub IOP_ADDU()
Declare Sub IOP_AND()
Declare Sub IOP_ANDI()
Declare Sub IOP_DIV()
Declare Sub IOP_DIVU()
Declare Sub IOP_MULT()
Declare Sub IOP_MULTU()
Declare Sub IOP_NOR()
Declare Sub IOP_OR()
Declare Sub IOP_ORI()
Declare Sub IOP_SLL()
Declare Sub IOP_SLLV()
Declare Sub IOP_SRA()
Declare Sub IOP_SRAV()
Declare Sub IOP_SRL()
Declare Sub IOP_SRLV()
Declare Sub IOP_SUB()
Declare Sub IOP_SUBU()
Declare Sub IOP_XOR()
Declare Sub IOP_XORI()
'Read Modify Write Operations
Declare Sub IOP_LB()
Declare Sub IOP_LBU()
Declare Sub IOP_LH()
Declare Sub IOP_LHU()
Declare Sub IOP_LI()
Declare Sub IOP_LUI()
Declare Sub IOP_LW()
Declare Sub IOP_LWL()
Declare Sub IOP_LWR()
Declare Sub IOP_MFHI()
Declare Sub IOP_MFLO()
Declare Sub IOP_MTLO()
Declare Sub IOP_MTHI()
Declare Sub IOP_SB()
Declare Sub IOP_SH()
Declare Sub IOP_SW()
Declare Sub IOP_SWL()
Declare Sub IOP_SWR()
'Misc. Operations
Declare Sub IOP_SLT()
Declare Sub IOP_SLTI()
Declare Sub IOP_SLTIU()
Declare Sub IOP_SLTU()
Declare Sub IOP_SYSCALL()
'Logic Flow Operations
Declare Sub IOP_BEQ()
Declare Sub IOP_BGEZ()
Declare Sub IOP_BGEZAL()
Declare Sub IOP_BGTZ()
Declare Sub IOP_BLEZ()
Declare Sub IOP_BLTZ()
Declare Sub IOP_BLTZAL()
Declare Sub IOP_BNE()
Declare Sub IOP_BREAK()
Declare Sub IOP_JMP()
Declare Sub IOP_JAL()
Declare Sub IOP_JALR()
Declare Sub IOP_JR()
'Coprocessor Operations
Declare Sub IOP_CFC1() 'CFCz
Declare Sub IOP_CFC2() 'CFCz
Declare Sub IOP_CFC3() 'CFCz
Declare Sub IOP_COP0() 'COPz
Declare Sub IOP_COP2() 'COPz
Declare Sub IOP_CTC2() 'CTCz
Declare Sub IOP_LWC2()
Declare Sub IOP_MFC0() 'MFCz
Declare Sub IOP_MFC2() 'MFCz
Declare Sub IOP_MTC0() 'MTCz
Declare Sub IOP_MTC2() 'MTCz
Declare Sub IOP_SWC2()
Declare Sub IOP_NULL()
Declare Sub IOP_decodeOp()
'#Include "logging.bi"
Dim Shared IOP_SPECIAL(0 To 63) As Sub() => _
{	@IOP_SLL , @IOP_NULL , @IOP_SRL , @IOP_SRA , @IOP_SLLV   , @IOP_NULL , @IOP_SRLV, @IOP_SRAV,_
	@IOP_JR  , @IOP_JALR , @IOP_NULL, @IOP_NULL, @IOP_SYSCALL, @IOP_BREAK, @IOP_NULL, @IOP_NULL,_
	@IOP_MFHI, @IOP_MTHI , @IOP_MFLO, @IOP_MTLO, @IOP_NULL   , @IOP_NULL , @IOP_NULL, @IOP_NULL,_
	@IOP_MULT, @IOP_MULTU, @IOP_DIV , @IOP_DIVU, @IOP_NULL   , @IOP_NULL , @IOP_NULL, @IOP_NULL,_
	@IOP_ADD , @IOP_ADDU , @IOP_SUB , @IOP_SUBU, @IOP_AND    , @IOP_OR   , @IOP_XOR , @IOP_NOR ,_
	@IOP_NULL, @IOP_NULL , @IOP_SLT , @IOP_SLTU, @IOP_NULL   , @IOP_NULL , @IOP_NULL, @IOP_NULL,_
	@IOP_NULL, @IOP_NULL , @IOP_NULL, @IOP_NULL, @IOP_NULL   , @IOP_NULL , @IOP_NULL, @IOP_NULL,_
	@IOP_NULL, @IOP_NULL , @IOP_NULL, @IOP_NULL, @IOP_NULL   , @IOP_NULL , @IOP_NULL, @IOP_NULL} 
Dim Shared IOP_REGIMM(0 To 31) As Sub () => _
{	@IOP_BLTZ  , @IOP_BGEZ  , @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL,_
	@IOP_NULL  , @IOP_NULL  , @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL,_
	@IOP_BLTZAL, @IOP_BGEZAL, @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL,_
	@IOP_NULL  , @IOP_NULL  , @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL}
Dim Shared IOP_BASIC(0 To 63) As Sub () => _
{	@IOP_NULL  	, @IOP_NULL	 , @IOP_JMP , @IOP_JAL  , @IOP_BEQ , @IOP_BNE , @IOP_BLEZ, @IOP_BGTZ,_
	@IOP_ADDI   , @IOP_ADDIU , @IOP_SLTI, @IOP_SLTIU, @IOP_ANDI, @IOP_ORI , @IOP_XORI, @IOP_LUI ,_
	@IOP_COP0   , @IOP_NULL  , @IOP_COP2, @IOP_NULL , @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL,_
	@IOP_NULL   , @IOP_NULL  , @IOP_NULL, @IOP_NULL , @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL,_
	@IOP_LB     , @IOP_LH    , @IOP_LWL , @IOP_LW   , @IOP_LBU , @IOP_LHU , @IOP_LWR , @IOP_NULL,_
	@IOP_SB     , @IOP_SH    , @IOP_SWL , @IOP_SW   , @IOP_NULL, @IOP_NULL, @IOP_SWR , @IOP_NULL,_
	@IOP_NULL   , @IOP_NULL  , @IOP_LWC2, @IOP_NULL , @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL,_ 
	@IOP_NULL   , @IOP_NULL  , @IOP_SWC2, @IOP_NULL , @IOP_NULL, @IOP_NULL, @IOP_NULL, @IOP_NULL}
'ALU Operations
Sub IOP_ADD()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = CInt(iop.reg(op_rs)) + CInt(iop.reg(op_rt))
End Sub
Sub IOP_ADDI()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	iop.reg(op_rt) = CInt(iop.reg(op_rs)) + op_imm
End Sub
Sub IOP_ADDIU()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)	
	iop.reg(op_rt) = CInt(iop.reg(op_rs)) + op_imm
End Sub
Sub IOP_ADDU()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = CInt(iop.reg(op_rs)) + CInt(iop.reg(op_rt))
End Sub
Sub IOP_AND()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = iop.reg(op_rs) And iop.reg(op_rt)
End Sub
Sub IOP_ANDI()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	iop.reg(op_rt) = iop.reg(op_rs) And imm
End Sub
Sub IOP_DIV()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	If iop.reg(op_rt) <> 0 Then 
		iop.LO = CInt(iop.reg(op_rs)) / CInt(iop.reg(op_rt)) 
		iop.HI = CInt(iop.reg(op_rs)) Mod CInt(iop.reg(op_rt))
	EndIf
End Sub
Sub IOP_DIVU()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	If iop.reg(op_rt) <> 0 Then 
		iop.LO = iop.reg(op_rs) / iop.reg(op_rt)
		iop.HI = iop.reg(op_rs) Mod iop.reg(op_rt)
	EndIf
End Sub
Sub IOP_MULT()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim result As ULongInt = CInt(iop.reg(op_rs)) * CInt(iop.reg(op_rt))
	iop.LO = result And &hFFFFFFFF
	iop.HI = (result Shr 32) And &hFFFFFFFF
End Sub
Sub IOP_MULTU()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim result As ULongInt = iop.reg(op_rs)  * iop.reg(op_rt)
	iop.LO = result And &hFFFFFFFF
	iop.HI = (result Shr 32) And &hFFFFFFFF
End Sub
Sub IOP_NOR()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = Not(iop.reg(op_rs) Or iop.reg(op_rt))
End Sub
Sub IOP_OR()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = iop.reg(op_rs) Or iop.reg(op_rt)
End Sub
Sub IOP_ORI()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	iop.reg(op_rt) = iop.reg(op_rs) Or op_imm
End Sub
Sub IOP_SLL()
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	Dim op_sa As UByte = ((iop.opcode Shr 6) And &h1F)
	iop.reg(op_rd) = iop.reg(op_rt) Shl op_sa
End Sub
Sub IOP_SLLV()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = iop.reg(op_rt) Shl (iop.reg(op_rs) And &h1F)
End Sub
Sub IOP_SRA()
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	Dim op_sa As UByte = ((iop.opcode Shr 6) And &h1F)
	iop.reg(op_rt) = CInt(iop.reg(op_rt) Shr op_sa)
End Sub
Sub IOP_SRAV()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rt) = CInt(iop.reg(op_rt) Shr (iop.reg(op_rs) And &h1F))
End Sub
Sub IOP_SRL()
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	Dim op_sa As UByte = ((iop.opcode Shr 6) And &h1F)
	iop.reg(op_rd) = iop.reg(op_rt) Shr op_sa
End Sub
Sub IOP_SRLV()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = iop.reg(op_rt) Shr (iop.reg(op_rs) And &h1F)
End Sub
Sub IOP_SUB()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = CInt(iop.reg(op_rs)) - CInt(iop.reg(op_rt))
End Sub
Sub IOP_SUBU()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = CInt(iop.reg(op_rs)) - CInt(iop.reg(op_rt))
End Sub
Sub IOP_XOR()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = iop.reg(op_rs) Xor iop.reg(op_rt)
End Sub
Sub IOP_XORI()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	iop.reg(op_rt) = iop.reg(op_rs) Xor op_imm
End Sub
'Read Modify Write Operations
Sub IOP_LB()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim addr As ULong = CShort(op_imm) + iop.reg(op_rs) 
	iop.reg(op_rt) = CByte(iop_read8(addr))
End Sub	
Sub IOP_LBU()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim addr As ULong = CShort(op_imm) + iop.reg(op_rs)
	iop.reg(op_rt) = iop_read8(addr)
End Sub 	
Sub IOP_LH()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim addr As ULong = CShort(op_imm) + iop.reg(op_rs)
	iop.reg(op_rt) = CShort(iop_read16(addr))
End Sub
Sub IOP_LHU()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim addr As ULong = CShort(op_imm) + iop.reg(op_rs)
	iop.reg(op_rt) = iop_read16(addr)
End Sub
Sub IOP_LUI()
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	iop.reg(op_rt) = op_imm Shl 16
End Sub
Sub IOP_LW()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim addr As ULong = op_imm + iop.reg(op_rs)
	iop.reg(op_rt) = iop_read32(addr)
End Sub
Sub IOP_LWL()
	
End Sub
Sub IOP_LWR()
	
End Sub
Sub IOP_MFHI()
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = iop.HI
End Sub
Sub IOP_MFLO()
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rd) = iop.LO
End Sub
Sub IOP_MTLO()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	iop.LO = iop.reg(op_rs)
End Sub
Sub IOP_MTHI()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	iop.HI = iop.reg(op_rs)
End Sub
Sub IOP_SB()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim isc As UByte = (iop_cop0reg.SR Shr 16) And 1
	Dim addr As ULong = CShort(op_imm) + iop.reg(op_rs)
	If isc = 0 Then iop_write8((iop.reg(op_rt) And &hFF),addr)
End Sub
Sub IOP_SH()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim isc As UByte = (iop_cop0reg.SR Shr 16) And 1
	Dim addr As ULong = op_imm + iop.reg(op_rs)
	If isc = 0 Then iop_write16((iop.reg(op_rt) And &hFFFF),addr)
End Sub
Sub IOP_SW()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim isc As UByte = (iop_cop0reg.SR Shr 16) And 1
	Dim addr As ULong = CShort(op_imm) + iop.reg(op_rs)
	If isc = 0 Then iop_write32(iop.reg(op_rt),addr)
End Sub
Sub IOP_SWL()
	
End Sub
Sub IOP_SWR()
	
End Sub
'Misc. Operations
Sub IOP_SLT()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	If CInt(iop.reg(op_rs)) < CInt(iop.reg(op_rt)) Then iop.reg(op_rd) = 1 Else iop.reg(op_rd) = 0
End Sub
Sub IOP_SLTI()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	If CInt(iop.reg(op_rs)) < CShort(op_imm) Then iop.reg(op_rt) = 1 Else iop.reg(op_rt) = 0
End Sub
Sub IOP_SLTIU()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	If iop.reg(op_rs) < op_imm Then iop.reg(op_rt) = 1 Else iop.reg(op_rt) = 0
End Sub
Sub IOP_SLTU()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	If iop.reg(op_rs) < iop.reg(op_rt) Then iop.reg(op_rd) = 1 Else iop.reg(op_rd) = 0	
End Sub
Sub IOP_SYSCALL()
	Cls
	For i As Integer = 0 To 31
		Print "0x" & Hex(iop.reg(i))
	Next
End Sub
'Logic Flow Operations
Sub IOP_BEQ()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim op_target As Long = imm Shl 2
	If iop.reg(op_rs) = iop.reg(op_rt) Then 
		iop.branchPC = iop.PC + op_target + 4
		iop.branchPending = 1
	EndIf
End Sub
Sub IOP_BGEZ()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim op_target As Long = imm Shl 2
	If CInt(iop.reg(op_rs)) >= 0 Then 
		iop.branchPC = iop.PC + op_target + 4
		iop.branchPending = 1
	EndIf
End Sub
Sub IOP_BGEZAL()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	iop.reg(31) = iop.PC + 8
	Dim op_target As Long = op_imm Shl 2
	If CInt(iop.reg(op_rs)) >= 0 Then 
		iop.branchPC = iop.PC + op_target + 4
		iop.branchPending = 1
	EndIf
End Sub
Sub IOP_BGTZ()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim op_target As Long = op_imm Shl 2
	If CInt(iop.reg(op_rs)) > 0 Then 
		iop.branchPC = iop.PC + op_target + 4
		iop.branchPending = 1
	EndIf
End Sub
Sub IOP_BLEZ()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim op_target As Long = op_imm Shl 2
	If CInt(iop.reg(op_rs)) <= 0 Then 
		iop.branchPC = iop.PC + op_target + 4
		iop.branchPending = 1
	EndIf
End Sub
Sub IOP_BLTZ()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim op_target As Long = op_imm Shl 2
	If CInt(iop.reg(op_rs)) < 0 Then 
		iop.branchPC = iop.PC + op_target + 4
		iop.branchPending = 1
	EndIf
End Sub
Sub IOP_BLTZAL()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	iop.reg(31) = iop.PC + 8
	Dim op_target As Long = op_imm Shl 2
	If CInt(iop.reg(op_rs)) < 0 Then 
		iop.branchPC = iop.PC + op_target + 4
		iop.branchPending = 1
	EndIf
End Sub
Sub IOP_BNE()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_imm As Short = CShort(iop.opcode And &hFFFF)
	Dim op_target As Long = op_imm Shl 2
	If iop.reg(op_rs) <> iop.reg(op_rt) Then 
		iop.branchPC = iop.PC + op_target + 4
		Print #99, "BNE: " & Hex(op_imm)
		iop.branchPending = 1
	EndIf
End Sub
Sub IOP_BREAK()
	Print "[Critical] Breakpoint Exception 0x" & Hex(iop.PC)
End Sub
Sub IOP_JMP()
	Dim op_target As ULong = ((iop.opcode And &h3FFFFFF) Shl 2)
	iop.branchPC = iop.PC And &hF0000000
	iop.branchPC Or= op_target
	iop.branchPending = 1
End Sub
Sub IOP_JAL()
	iop.reg(31) = iop.PC + 8
	Dim op_target As ULong = ((iop.opcode And &h3FFFFFF) Shl 2)
	iop.branchPC = iop.PC And &hF0000000
	iop.branchPC Or= op_target
	iop.branchPending = 1
End Sub
Sub IOP_JALR()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	iop.reg(31) = iop.PC + 8
	iop.branchPC = iop.reg(op_rs)
	iop.branchPending = 1
End Sub
Sub IOP_JR()
	Dim op_rs As UByte = (iop.opcode Shr 21) And &h1F
	iop.branchPC = iop.reg(op_rs)
	iop.branchPending = 1
End Sub
'Coprocessor Operations
Sub IOP_CFC1() 'CFCz

End Sub
Sub IOP_CFC2() 'CFCz
	
End Sub
Sub IOP_CFC3() 'CFCz
	
End Sub
Sub IOP_COP0() 'COPz
	
End Sub
Sub IOP_COP2() 'COPz
	
End Sub
Sub IOP_CTC2() 'CTCz
	
End Sub
Sub IOP_LWC2()
	
End Sub
Sub IOP_MFC0() 'MFCz
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop.reg(op_rt) = iop_cop0reg.reg(op_rd)
End Sub
Sub IOP_MFC2() 'MFCz
	Print "reading from gte reg"
End Sub
Sub IOP_MTC0() 'MTCz
	Dim op_rt As UByte = (iop.opcode Shr 16) And &h1F
	Dim op_rd As UByte = (iop.opcode Shr 11) And &h1F
	iop_cop0reg.reg(op_rd) = iop.reg(op_rt)
End Sub
Sub IOP_MTC2() 'MTCz
	Print "writing to gte reg"
End Sub
Sub IOP_SWC2()
	
End Sub
Sub IOP_NULL()

End Sub
Sub IOP_DecodeOp()
		Select Case (iop.opcode Shr 21)
			Case &h242 'CFZ0
				IOP_CFC2
			Case &h246 'CTC2
				IOP_CTC2
			Case &h204 'MTC0
				IOP_MTC0
			Case &h244 'MTC2
				IOP_MTC2
			Case &h200 'MFC0
				IOP_MFC0
			Case &h240 'MFC2
				IOP_MFC2
		Case Else
	End Select
	Select Case (iop.opcode Shr 26)
		Case &h0 'Special
			IOP_SPECIAL(iop.opcode And &h3F)()
		Case &h1 'REGIMM
			IOP_REGIMM((iop.opcode Shr 16) And &h1F)()
		Case Else
			IOP_BASIC((iop.opcode Shr 26)And &h3F)()
	End Select
	#Ifdef _logging
		logOpcodes()
	#endif 
		
End Sub