Dim Shared logging As UByte = 1
#Define rs ((cpu.Opcode Shr 21) And &h1F)
#Define rt ((cpu.Opcode Shr 16) And &h1F)
#Define rd ((cpu.Opcode Shr 11) And &h1F)
#Define sa ((cpu.Opcode Shr 6) And &h1F)
#Define imm (cpu.opcode And &hFFFF)
#Define Target ((cpu.opcode And &h3FFFFFF) Shl 2)
Declare Sub ee_ADD()
Declare Sub ee_ADDI()
Declare Sub ee_ADDIU()
Declare Sub ee_ADDU()
Declare Sub ee_AND()
Declare Sub ee_ANDI()
Declare Sub ee_BEQ()
Declare Sub ee_BEQL()
Declare Sub ee_BGEZ()
Declare Sub ee_BGEZAL()
Declare Sub ee_BGEZALL()
Declare Sub ee_BGEZL()
Declare Sub ee_BGTZ()
Declare Sub ee_BGTZL()
Declare Sub ee_BLEZ()
Declare Sub ee_BLEZL()
Declare Sub ee_BLTZ()
Declare Sub ee_BLTZAL()
Declare Sub ee_BLTZALL()
Declare Sub ee_BLTZL()
Declare Sub ee_BNE()
Declare Sub ee_BNEL()
Declare Sub ee_BREAK()
Declare Sub ee_DADD()
Declare Sub ee_DADDI()
Declare Sub ee_DADDIU()
Declare Sub ee_DADDU()
Declare Sub ee_DIV()
Declare Sub ee_DIVU()
Declare Sub ee_DSLL()
Declare Sub ee_DSLL32()
Declare Sub ee_DSLLV()
Declare Sub ee_DSRA()
Declare Sub ee_DSRA32()
Declare Sub ee_DSRAV()
Declare Sub ee_DSRL()
Declare Sub ee_DSRL32()
Declare Sub ee_DSRLV()
Declare Sub ee_DSUB()
Declare Sub ee_DSUBU()
Declare Sub ee_J()
Declare Sub ee_JAL()
Declare Sub ee_JALR()
Declare Sub ee_JR()
Declare Sub ee_LB()
Declare Sub ee_LBU()
Declare Sub ee_LD()
Declare Sub ee_LDL()
Declare Sub ee_LDR()
Declare Sub ee_LH()
Declare Sub ee_LHU()
Declare Sub ee_LUI()
Declare Sub ee_LW()
Declare Sub ee_LWL()
Declare Sub ee_LWR()
Declare Sub ee_LWU()
Declare Sub ee_MFHI()
Declare Sub ee_MFLO()
Declare Sub ee_MOVN()
Declare Sub ee_MOVZ()
Declare Sub ee_MTHI()
Declare Sub ee_MTLO()
Declare Sub ee_MULT()
Declare Sub ee_MULTU()
Declare Sub ee_NOR()
Declare Sub ee_OR()
Declare Sub ee_ORI()
Declare Sub ee_PREF()
Declare Sub ee_SB()
Declare Sub ee_SD()
Declare Sub ee_SDL()
Declare Sub ee_SDR()
Declare Sub ee_SH()
Declare Sub ee_SLL()
Declare Sub ee_SLLV()
Declare Sub ee_SLT()
Declare Sub ee_SLTI()
Declare Sub ee_SLTIU()
Declare Sub ee_SLTU()
Declare Sub ee_SRA()
Declare Sub ee_SRAV()
Declare Sub ee_SRL()
Declare Sub ee_SRLV()
Declare Sub ee_SUB()
Declare Sub ee_SUBU()
Declare Sub ee_SW()
Declare Sub ee_SWL()
Declare Sub ee_SWR()
Declare Sub ee_SYNC()
Declare Sub ee_SYSCALL()
Declare Sub ee_TEQ()
Declare Sub ee_TEQI()
Declare Sub ee_TGE()
Declare Sub ee_TGEI()
Declare Sub ee_TGEIU()
Declare Sub ee_TGEU()
Declare Sub ee_TLT()
Declare Sub ee_TLTI()
Declare Sub ee_TLTIU()
Declare Sub ee_TLTU()
Declare Sub ee_TNE()
Declare Sub ee_TNEI()
Declare Sub ee_XOR()
Declare Sub ee_XORI()
Declare Sub ee_DEFAULT()

Declare Sub mmi_DIV1()
Declare Sub mmi_DIVU1()
Declare Sub mmi_LQ()
Declare Sub mmi_MADD()
Declare Sub mmi_MADD1()
Declare Sub mmi_MADDU()
Declare Sub mmi_MADDU1()
Declare Sub mmi_MFHI1()
Declare Sub mmi_MFLO1()
Declare Sub mmi_MFSA()
Declare Sub mmi_MTHI1()
Declare Sub mmi_MTLO1()
Declare Sub mmi_MTSA()
Declare Sub mmi_MTSAB()
Declare Sub mmi_MTSAH()
Declare Sub mmi_MULT()
Declare Sub mmi_MULT1()
Declare Sub mmi_MULTU()
Declare Sub mmi_MULTU1()
Declare Sub mmi_PABSH()
Declare Sub mmi_PABSW()
Declare Sub mmi_PADDB()
Declare Sub mmi_PADDH()
Declare Sub mmi_PADDSB()
Declare Sub mmi_PADDSH()
Declare Sub mmi_PADDSW()
Declare Sub mmi_PADDUB()
Declare Sub mmi_PADDUH()
Declare Sub mmi_PADDUW()
Declare Sub mmi_PADDW()
Declare Sub mmi_PADDSBH()
Declare Sub mmi_PAND()
Declare Sub mmi_PCEQB()
Declare Sub mmi_PCEQH()
Declare Sub mmi_PCEQW()
Declare Sub mmi_PCGTB()
Declare Sub mmi_PCGTH()
Declare Sub mmi_PCGTW()
Declare Sub mmi_PCPYH()
Declare Sub mmi_PCPYLD()
Declare Sub mmi_PCPYUD()
Declare Sub mmi_PDIVBW()
Declare Sub mmi_PDIVUW()
Declare Sub mmi_PDIVW()
Declare Sub mmi_PEXCH()
Declare Sub mmi_PEXCW()
Declare Sub mmi_PEXEH()
Declare Sub mmi_PEXEW()
Declare Sub mmi_PEXT5()
Declare Sub mmi_PEXTLB()
Declare Sub mmi_PEXTLH()
Declare Sub mmi_PEXTLW()
Declare Sub mmi_PEXTUB()
Declare Sub mmi_PEXTUH()
Declare Sub mmi_PEXTUW()
Declare Sub mmi_PHMADH()
Declare Sub mmi_PHMSBH()
Declare Sub mmi_PINTEH()
Declare Sub mmi_PINTH()
Declare Sub mmi_PLZCW()
Declare Sub mmi_PMADDH()
Declare Sub mmi_PMADDUW()
Declare Sub mmi_PMADDW()
Declare Sub mmi_PMAXH()
Declare Sub mmi_PMAXW()
Declare Sub mmi_PMFHI()
Declare Sub mmi_PMFHLLH()
Declare Sub mmi_PMFHLLW()
Declare Sub mmi_PMFHLSH()
Declare Sub mmi_PMFHLSLW()
Declare Sub mmi_PMFHLUW()
Declare Sub mmi_PMFLO()
Declare Sub mmi_PMINH()
Declare Sub mmi_PMINW()
Declare Sub mmi_PMSUBH()
Declare Sub mmi_PMSUBW()
Declare Sub mmi_PMTHI()
Declare Sub mmi_PMTHLLW()
Declare Sub mmi_PMTLO()
Declare Sub mmi_PMULTH()
Declare Sub mmi_PMULTUW()
Declare Sub mmi_PMULTW()
Declare Sub mmi_PNOR()
Declare Sub mmi_POR()
Declare Sub mmi_PPAC5()
Declare Sub mmi_PPACB()
Declare Sub mmi_PPACH()
Declare Sub mmi_PPACW()
Declare Sub mmi_PREVH()
Declare Sub mmi_PROT3W()
Declare Sub mmi_PSLLH()
Declare Sub mmi_PSLLVW()
Declare Sub mmi_PSLLW()
Declare Sub mmi_PSRAH()
Declare Sub mmi_PSRAVW()
Declare Sub mmi_PSRAW()
Declare Sub mmi_PSRLH()
Declare Sub mmi_PSRLVW()
Declare Sub mmi_PSRLW()
Declare Sub mmi_PSUBB()
Declare Sub mmi_PSUBH()
Declare Sub mmi_PSUBSB()
Declare Sub mmi_PSUBSH()
Declare Sub mmi_PSUBSW()
Declare Sub mmi_PSUBUB()
Declare Sub mmi_PSUBUH()
Declare Sub mmi_PSUBUW()
Declare Sub mmi_PSUBW()
Declare Sub mmi_PXOR()
Declare Sub mmi_QFSRV()
Declare Sub mmi_SQ()
Declare Sub mmi_DEFAULT()

Declare Sub cop0_BC0F()
Declare Sub cop0_BC0FL()
Declare Sub cop0_BC0T()
Declare Sub cop0_BC0TL()
Declare Sub cop0_BFH()
Declare Sub cop0_BHINBT()
Declare Sub cop0_BXLBT()
Declare Sub cop0_DHIN()
Declare Sub cop0_DHWBIN()
Declare Sub cop0_DHWOIN()
Declare Sub cop0_DXIN()
Declare Sub cop0_DXLDT()
Declare Sub cop0_DXLTG()
Declare Sub cop0_DXSDT()
Declare Sub cop0_DXSTG()
Declare Sub cop0_DXWBIN()
Declare Sub cop0_IFL()
Declare Sub cop0_IHIN()
Declare Sub cop0_IXIN()
Declare Sub cop0_IXLDT()
Declare Sub cop0_IXLTG()
Declare Sub cop0_IXSDT()
Declare Sub cop0_IXSTG()
Declare Sub cop0_DI()
Declare Sub cop0_EI()
Declare Sub cop0_ERET()
Declare Sub cop0_MFBPC()
Declare Sub cop0_MFC0()
Declare Sub cop0_MFDAB()
Declare Sub cop0_MFDABM()
Declare Sub cop0_MFDVB()
Declare Sub cop0_MFDVBM()
Declare Sub cop0_MFIAB()
Declare Sub cop0_MFIABM()
Declare Sub cop0_MFPC()
Declare Sub cop0_MFPS()
Declare Sub cop0_MTBPC()
Declare Sub cop0_MTC0()
Declare Sub cop0_MTDAB()
Declare Sub cop0_MTDABM()
Declare Sub cop0_MTDVB()
Declare Sub cop0_MTDVBM()
Declare Sub cop0_MTIAB()
Declare Sub cop0_MTIABM()
Declare Sub cop0_MTPC()
Declare Sub cop0_MTPS()
Declare Sub cop0_TLBP()
Declare Sub cop0_TLBR()
Declare Sub cop0_TLBWI()
Declare Sub cop0_TLBWR()
Declare Sub cop0_DEFAULT()

Declare Sub cop1_ABSS()
Declare Sub cop1_ADDS()
Declare Sub cop1_ADDAS()
Declare Sub cop1_BC1F()
Declare Sub cop1_BC1FL()
Declare Sub cop1_BC1T()
Declare Sub cop1_BCTTL()
Declare Sub cop1_CEQS()
Declare Sub cop1_CFS()
Declare Sub cop1_CLES()
Declare Sub cop1_CLTS()
Declare Sub cop1_CFC1()
Declare Sub cop1_CTC1()
Declare Sub cop1_CVTSW()
Declare Sub cop1_CVTWS()
Declare Sub cop1_DIVS()
Declare Sub cop1_LWC1()
Declare Sub cop1_MADDS()
Declare Sub cop1_MADDAS()
Declare Sub cop1_MAXS()
Declare Sub cop1_MFC1()
Declare Sub cop1_MINS()
Declare Sub cop1_MOVS()
Declare Sub cop1_MSUBS()
Declare Sub cop1_MSUBAS()
Declare Sub cop1_MTC1()
Declare Sub cop1_MULS()
Declare Sub cop1_MULAS()
Declare Sub cop1_NEGS()
Declare Sub cop1_RSQRTS()
Declare Sub cop1_SQRTS()
Declare Sub cop1_SUBS()
Declare Sub cop1_SUBAS()
Declare Sub cop1_SWC1()
Declare Sub cop1_DEFAULT

Declare Sub DecodeOp()
Declare Sub SPECIAL()
Declare Sub REGIMM()
Declare Sub COP0()
Declare Sub BC0()
Declare Sub TLB()
Declare Sub COP1()
Declare Sub COP2()
Declare Sub MMI()
Declare Sub MMI0()
Declare Sub MMI1()
Declare Sub MMI2()
Declare Sub MMI3()

#Include "logging.bi"

Dim Shared tbl_NORMAL(0 To 63) As Sub() => _
{	@SPECIAL,	 @REGIMM,		@ee_J, 	 	 @ee_JAL, 	  @ee_BEQ,  	@ee_BNE, 	 @ee_BLEZ, 		@ee_BGTZ, 	 _ '000
	@ee_ADDI, 	 @ee_ADDIU, 	@ee_SLTI, 	 @ee_SLTIU,   @ee_ANDI, 	@ee_ORI, 	 @ee_XORI, 		@ee_LUI,  	 _ '001
	@COP0, 	 	 @COP1, 	 		@COP2, 		 @ee_DEFAULT, @ee_BEQL, 	@ee_BNEL, 	 @ee_BLEZL, 	@ee_BGTZL, 	 _ '010
	@ee_DADDI,	 @ee_DADDIU,	@ee_LDL, 	 @ee_LDR, 	  @MMI, 			@ee_DEFAULT, @mmi_LQ, 		@mmi_SQ, 	 _	'011
	@ee_LB, 		 @ee_LH, 		@ee_LWL, 	 @ee_LW, 	  @ee_LBU, 		@ee_LHU, 	 @ee_LWR, 		@ee_LWU, 	 _ '100
	@ee_SB, 		 @ee_SH, 		@ee_SWL, 	 @ee_SW, 	  @ee_SDL, 		@ee_SDR, 	 @ee_SWR, 		@ee_DEFAULT, _ '101
	@ee_DEFAULT, @cop1_LWC1, 	@ee_DEFAULT, @ee_PREF, 	  @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT , @ee_LD, 		 _ '110
	@ee_DEFAULT, @cop1_SWC1, 	@ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, 	@ee_SD 		 _ '111
}



Dim Shared tbl_SPECIAL(0 To 63) As Sub() => _
{	@ee_SLL, 	@ee_DEFAULT, @ee_SRL,  @ee_SRA,  @ee_SLLV, 	 @ee_DEFAULT, 	@ee_SRLV, 		@ee_SRAV, 		_
	@ee_JR, 		@ee_JALR, 	 @ee_MOVZ, @ee_MOVN, @ee_SYSCALL, @ee_BREAK, 	@ee_DEFAULT, 	@ee_SYNC, 		_
	@ee_MFHI, 	@ee_MTHI, 	 @ee_MFLO, @ee_MTLO, @ee_DSLLV, 	 @ee_DEFAULT, 	@ee_DSRLV, 		@ee_DSRAV, 		_
	@ee_MULT, 	@ee_MULTU, 	 @ee_DIV,  @ee_DIVU, @ee_DEFAULT, @ee_DEFAULT, 	@ee_DEFAULT, 	@ee_DEFAULT, 	_
	@ee_ADD, 	@ee_ADDU, 	 @ee_SUB,  @ee_SUBU, @ee_AND, 	 @ee_OR, 		@ee_XOR, 		@ee_NOR, 		_
	@mmi_MFSA, 	@mmi_MTSA, 	 @ee_SLT,  @ee_SLTU, @ee_DADD, 	 @ee_DADDU, 	@ee_DSUB, 		@ee_DSUBU, 		_
	@ee_TGE, 	@ee_TGEU, 	 @ee_TLT,  @ee_TLTU, @ee_TEQ, 	 @ee_DEFAULT, 	@ee_TNE, 		@ee_DEFAULT, 	_
	@ee_DSLL, 	@ee_DEFAULT, @ee_DSRL, @ee_DSRA, @ee_DSLL32,  @ee_DEFAULT, 	@ee_DSRL32, 	@ee_DSRA32 		_
}

Dim Shared tbl_REGIMM(0 To 31) As Sub() => _
{	@ee_BLTZ, 	@ee_BGEZ, 	@ee_BLTZL,	 @ee_BGEZL,	  @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@ee_TGEI, 	@ee_TGEIU, 	@ee_TLTI,	 @ee_TLTIU,	  @ee_TEQI, 	@ee_DEFAULT, @ee_TNEI,	  @ee_DEFAULT, _
	@ee_BLTZAL, @ee_BGEZAL, @ee_BLTZALL, @ee_BGEZALL, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@mmi_MTSAB, @mmi_MTSAH, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT  _
}

Dim Shared tbl_MMI(0 To 63) As Sub() => _ 
{	@mmi_MADD, @mmi_MADDU, @ee_DEFAULT, @ee_DEFAULT, @mmi_PLZCW, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@ee_DEFAULT, @ee_DEFAULT, @mmi_MFLO1, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@ee_DEFAULT, @ee_DEFAULT, @mmi_DIV1, @mmi_DIVU1, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@MMI2		  , @MMI3		, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT _
}
/'
Dim Shared MMI0(0 To 31) As Sub() => _

Dim Shared MMI1(0 To 31) As Sub() => _
'/	
Dim Shared tbl_MMI2(0 To 31) As Sub() => _
{	@mmi_PMADDW,	@ee_DEFAULT,	@mmi_PSLLVW,	@mmi_PSRLVW,	_
	@mmi_PMSUBW,	@ee_DEFAULT,	@ee_DEFAULT,	@ee_DEFAULT,	_
	@mmi_PMFHI ,	@mmi_PMFLO ,	@mmi_PINTH ,	@ee_DEFAULT,	_
	@mmi_PMULTW,	@mmi_PDIVW ,	@mmi_PCPYLD,	@ee_DEFAULT,	_
	@mmi_PMADDH,	@mmi_PHMADH,	@mmi_PAND  ,	@mmi_PXOR  ,	_
	@mmi_PMSUBH,	@mmi_PHMSBH,	@ee_DEFAULT,	@ee_DEFAULT,	_
	@ee_DEFAULT,	@ee_DEFAULT,	@mmi_PEXEH ,	@mmi_PREVH ,	_
	@mmi_PMULTH,	@mmi_PDIVBW,	@mmi_PEXEW ,	@mmi_PROT3W		_
}
Dim Shared tbl_MMI3(0 To 31) As Sub() => _
{	@mmi_PMADDUW,	@ee_DEFAULT,	@ee_DEFAULT,	@ee_DEFAULT,	_
	@ee_DEFAULT, 	@ee_DEFAULT, 	@ee_DEFAULT,	@ee_DEFAULT,	_
	@mmi_PMTHI,		@mmi_PMTLO,		@mmi_PINTEH,	@ee_DEFAULT,	_
	@mmi_PMULTUW,	@mmi_PDIVUW,	@mmi_PCPYUD,	@ee_DEFAULT,	_
	@ee_DEFAULT, 	@ee_DEFAULT, 	@mmi_POR	  ,	@mmi_PNOR  ,	_
	@mmi_PMTHI,		@mmi_PMTLO,		@mmi_PINTEH,	@ee_DEFAULT,	_
	@ee_DEFAULT, 	@ee_DEFAULT, 	@mmi_PEXCH ,	@mmi_PCPYH ,	_
	@mmi_PMTHI,		@mmi_PMTLO,		@mmi_PEXCW ,	@ee_DEFAULT		_
}
	 

Dim Shared tbl_COP0(0 To 31) As Sub() => _
{ 	@cop0_MFC0,  @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @cop0_MTC0,  @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@BC0, 		 @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@TLB, 		 @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, _
	@ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT, @ee_DEFAULT  _
}
/'
Dim Shared BC0(0 To 31) As Sub() => _

Dim Shared TLB(0 To 63) As Sub() => _

Dim Shared COP1(0 To 31) As Sub() => _

Dim Shared BC1(0 To 31) As Sub() => _

Dim Shared FPUS(0 To 63) As Sub() => _

Dim Shared FPUW(0 To 63) As Sub() => _

'/
Sub BC0()
	
End Sub
Sub TLB()
	
End Sub
Sub ee_ADD()
	Print"TODO: ADD"
End Sub
Sub ee_ADDI()
	Print"TODO: ADDI"
End Sub
Sub ee_ADDIU()
	'Print"ADDIU"
	cpu.reg(rt).r0 = cpu.reg(rs).r0 + CShort(imm)
End Sub
Sub ee_ADDU()
	'Print"ADDU"
	cpu.reg(rd).r0 = cint(CInt(cpu.reg(rt).int0) + CInt(cpu.reg(rs).int0))
End Sub
Sub ee_AND()
	'Print"AND"
	cpu.reg(rd).r0 = cpu.reg(rs).r0 and cpu.reg(rt).r0
End Sub
Sub ee_ANDI()
	'Print"ANDI"
	cpu.reg(rt).r0 = cpu.reg(rs).r0 And imm
End Sub
Sub ee_BEQ()
	'print "BEQ" 
	If cpu.reg(rs).r0 = cpu.reg(rt).r0 Then
		Dim As Integer temp = CShort(imm)
		temp Shl= 2
		cpu.branchPC = cpu.PC + temp + 4
		cpu.branchPending = 1
	EndIf
End Sub
Sub ee_BEQL()
	'print "BEQL" 
	If cpu.reg(rs).r0 = cpu.reg(rt).r0 Then
		Dim As Integer temp = CShort(imm)
		temp Shl= 2
		cpu.branchPC = cpu.PC + temp + 4
		cpu.branchPending = 1
	Else
		cpu.PC += 4
	EndIf
End Sub
Sub ee_BGEZ()
	If cpu.reg(rs).s0 >= 0 Then 
		cpu.branchPC = CShort(imm Shl 2)  + cpu.PC + 4
		cpu.branchPending = 1
	EndIf	
End Sub
Sub ee_BGEZAL()
	Print"TODO: BGEZAL"
End Sub
Sub ee_BGEZALL()
	Print"TODO: BGEZALL"
End Sub
Sub ee_BGEZL()
	Print"TODO: BGEZL"
End Sub
Sub ee_BGTZ()
	Print"TODO: BGTZ"
End Sub
Sub ee_BGTZL()
	Print"TODO: BGTZL"
End Sub
Sub ee_BLEZ()
	If cpu.reg(rs).s0 <= 0 Then 
		cpu.branchPC = CShort(imm Shl 2)  + cpu.PC + 4
		cpu.branchPending = 1
	EndIf
End Sub
Sub ee_BLEZL()
	Print"TODO: BLEZL"
End Sub
Sub ee_BLTZ()
	'print "BLTZ"
	If cpu.reg(rs).s0 < 0 Then 
		cpu.branchPC = CShort(imm Shl 2) + cpu.PC + 4
		cpu.branchPending = 1
	EndIf
End Sub
Sub ee_BLTZAL()
	Print "TODO: BLTZAL"
End Sub
Sub ee_BLTZALL()
	Print "TODO: BLTZALL"
End Sub
Sub ee_BLTZL()
	Print "TODO: BLTZL"
End Sub
Sub ee_BNE()
	'print "BNE"
	If cpu.reg(rs).r0 <> cpu.reg(rt).r0 Then
		Dim As Integer temp = CShort(imm)
		temp Shl= 2
		cpu.branchPC = cpu.PC + temp + 4
		cpu.branchPending = 1
	EndIf
End Sub
Sub ee_BNEL()
	'print "BNEL"
	If cpu.reg(rs).r0 <> cpu.reg(rt).r0 Then
		Dim As Integer temp = CShort(imm)
		temp Shl= 2
		cpu.branchPC = cpu.PC + temp + 4
		cpu.branchPending = 1
	Else
		cpu.PC += 4
	EndIf
End Sub
Sub ee_BREAK()
End Sub
Sub ee_DADD()
	Print "TODO: DADD"
End Sub
Sub ee_DADDI()
	Print "TODO: DADDI"
End Sub
Sub ee_DADDIU()
	Print "TODO: DADDIU"
End Sub
Sub ee_DADDU()
	'print "DADDU"
	cpu.reg(rd).r0 = cpu.reg(rt).s0 + cpu.reg(rs).s0
End Sub
Sub ee_DIV()
	'print "DIV"
	If cpu.reg(rs).s0 = &h80000000 AndAlso cpu.reg(rt).s0 = &hFFFFFFFF Then 
		cpu.LO.r0 = &h80000000
		cpu.HI.r0 = 0
	Else 
		cpu.LO.r0 = cpu.reg(rs).int0 \ cpu.reg(rt).int0
		cpu.HI.r0 = cpu.reg(rs).int0 Mod cpu.reg(rt).int0
	EndIf
End Sub
Sub ee_DIVU()
	If Not(cpu.reg(rt).uint0) Then 
		cpu.LO.r0 = &hFFFFFFFF
		cpu.HI.r0 = cpu.reg(rs).uint0
	Else
		cpu.LO.r0 = cpu.reg(rs).uint0 \ cpu.reg(rt).uint0
		cpu.HI.r0 = cpu.reg(rs).uint0 Mod cpu.reg(rt).uint0
	EndIf 
End Sub
Sub ee_DSLL()
	'print "DSLL"
	cpu.reg(rd).r0 Shl= sa
End Sub
Sub ee_DSLL32()
	cpu.reg(rd).r0 = cpu.reg(rt).r0 Shl (sa + 32)
End Sub
Sub ee_DSLLV()
	Print"TODO: DSLLV"
End Sub
Sub ee_DSRA()
	Print"TODO: DSRA"
End Sub
Sub ee_DSRA32()
	cpu.reg(rd).r0 = cpu.reg(rt).s0 Shr (sa + 32)
End Sub
Sub ee_DSRAV()
	Print"TODO: DSRAV"
End Sub
Sub ee_DSRL()
	Print"TODO: DSRL"
End Sub
Sub ee_DSRL32()
	cpu.reg(rd).r0 = cpu.reg(rt).r0 Shr (sa + 32)
End Sub
Sub ee_DSRLV()
	Print"TODO: DSRLV"
End Sub
Sub ee_DSUB()
	Print"TODO: DSUB"
End Sub
Sub ee_DSUBU()
	Print"TODO: DSUBU"
End Sub
Sub ee_J()
	'print "JUMP"
	cpu.branchPending = 1
	cpu.branchPC = cpu.PC And &hF0000000
	cpu.branchPC Or= Target
	If cpu.branchPC = cpu.PC Then 
		Print "Infinite loop at 0x:" & Hex(cpu.PC)
		Sleep	
	EndIf

End Sub
Sub ee_JAL()
	'print "JAL"
	cpu.reg(31).r0 = cpu.pc + 8
	cpu.branchPending = 1
	cpu.branchPC = cpu.PC And &hF0000000
	cpu.branchPC Or= Target
End Sub
Sub ee_JALR()
	'print "JALR"
	cpu.reg(31).r0 = cpu.pc + 8
	cpu.branchPending = 1
	cpu.branchPC = cpu.reg(rs).r0
End Sub
Sub ee_JR()
	'print "JR"
	cpu.branchPending = 1
	cpu.branchPC = cpu.reg(rs).r0
End Sub
Sub ee_LB()
	Dim addr As UInteger 
	addr = cpu.reg(rs).r0 + CShort(imm)
	cpu.reg(rt).r0 = CByte(read8(addr))
End Sub
Sub ee_LBU()
	'print "LBU"
	Dim addr As UInteger 
	addr = cpu.reg(rs).r0 + CShort(imm)
	cpu.reg(rt).r0 = read8(addr)
End Sub
Sub ee_LD()
	'print "LD"
	Dim addr As UInteger 
	addr = cpu.reg(rs).r0 + CShort(imm)
	cpu.reg(rt).r0 = read64(addr)
End Sub
Sub ee_LDL()
	Print"TODO: LDL"
End Sub
Sub ee_LDR()
	Print"TODO: LDR"
End Sub
Sub ee_LH()
	'print "LH"
	Dim addr As UInteger 
	addr = cpu.reg(rs).r0 + CShort(imm)
	cpu.reg(rt).r0 = cShort(read16(addr))
End Sub
Sub ee_LHU()
	'print "LHU"
	Dim addr As UInteger 
	addr = cpu.reg(rs).r0 + CShort(imm)
	cpu.reg(rt).r0 = read16(addr)
End Sub
Sub ee_LUI()
	'print "LUI"
	Dim temp As LongInt
	temp = imm
	temp Shl= 16
	cpu.Reg(rt).r0 = temp
End Sub
Sub ee_LW()
	'print "LW" 
	Dim addr As UInteger 
	addr = cpu.reg(rs).r0 + CShort(imm)
	cpu.reg(rt).r0 = read32(addr)
End Sub
Sub ee_LWL()
	Print"TODO: LWL"
End Sub
Sub ee_LWR()
	Print"TODO: LWR"
End Sub
Sub ee_LWU()
	Print"TODO: LWU"
End Sub
Sub ee_MFHI()
	Print"TODO: MFHI"
End Sub
Sub ee_MFLO()
	'print "MFLO"
	cpu.reg(rd).r0 = cpu.LO.r0
End Sub
Sub ee_MOVN()
	'print "MOVN"
	If cpu.reg(rt).r0 <> 0 Then cpu.reg(rd).r0 = cpu.reg(rs).r0
End Sub
Sub ee_MOVZ()
	'Print "MovZ"
	If cpu.reg(rt).r0 = 0 Then cpu.reg(rd).r0 = cpu.reg(rs).r0
End Sub
Sub ee_MTHI()
	Print "TODO: MTHI"
End Sub
Sub ee_MTLO()
	Print "TODO: MTLO"
End Sub
Sub ee_MULT()
	'print "MULT"
	Dim As Integer mRS = CInt(cpu.reg(rs).int0)
	Dim As Integer mRT = CInt(cpu.reg(rt).int0)
	Dim As LongInt result = mRS * mRT
	cpu.LO.r0 = result And &hFFFFFFFF
	cpu.HI.r0 = (result Shr 32) And &hFFFFFFFF 
End Sub
Sub ee_MULTU()
	Print"TODO: MULTU"
End Sub
Sub ee_NOR()
	Print"TODO: NOR"
End Sub
Sub ee_OR()
	'print "OR"
	cpu.reg(rd).r0 = cpu.reg(rs).r0 Or cpu.reg(rt).r0
End Sub
Sub ee_ORI()
	'print "ORI"
	cpu.reg(rt).r0 = cpu.reg(rs).r0 Or imm
End Sub
Sub ee_PREF()
	'print "TODO: PREF"
End Sub
Sub ee_SB()
	'print "SB"
	Dim addr As ULong
	Dim value As UByte
	addr = CShort(imm) + cpu.reg(rs).r0 
	value = cpu.reg(rt).r0 And &hFF
	write8(addr, value)
End Sub
Sub ee_SD()
	'print "SD"
	Dim addr As ULong
	addr = CShort(imm) + cpu.reg(rs).r0
	write64(addr,cpu.reg(rt).r0)
End Sub
Sub ee_SDL()
	Print"TODO: SDL"
End Sub
Sub ee_SDR()
	Print"TODO: SDR"
End Sub
Sub ee_SH()
	'print "SH"
	Dim addr As ULong
	Dim value As UShort
	addr = CShort(imm) + cpu.reg(rs).r0
	value = cpu.reg(rt).r0 And &hFFFF
	write16(addr,value)
End Sub
Sub ee_SLL()
	'print "SLL"
	cpu.reg(rd).r0 = cpu.reg(rt).r0 Shl sa
End Sub
Sub ee_SLLV()
	Print"TODO: SLLV"
End Sub
Sub ee_SLT()
	'print "SLT"
	If	cpu.reg(rs).s0 < cpu.reg(rt).s0 Then cpu.reg(rd).r0 = 1 Else cpu.reg(rd).r0 = 0
End Sub
Sub ee_SLTI()
	'print "SLTI"
	If cpu.reg(rs).s0 < CShort(imm) Then cpu.reg(rt).r0 = 1 Else cpu.reg(rt).r0 = 0
End Sub
Sub ee_SLTIU()
	'print "SLTIU"
	If cpu.reg(rs).r0 < imm Then cpu.reg(rt).r0 = 1 Else cpu.reg(rt).r0 = 0
End Sub
Sub ee_SLTU() 
	'print "SLTU"
	If cpu.reg(rs).r0 < cpu.reg(rt).r0 Then cpu.reg(rd).r0 = 1 Else cpu.reg(rd).r0 = 0
End Sub
Sub ee_SRA()
	'print "SRA"
	cpu.reg(rd).r0 = cpu.reg(rt).r0 shr sa
End Sub
Sub ee_SRAV()
	Print"TODO: SRAV"
End Sub
Sub ee_SRL()
	'print "SRL"
	cpu.reg(rd).s0 = CInt(cpu.reg(rt).s0 Shr sa)
End Sub
Sub ee_SRLV()
	Print"TODO: SRVL"
End Sub
Sub ee_SUB()
	Print"TODO: SUB"
End Sub
Sub ee_SUBU()
	'print "SUBU"
	
	'Print "RS : " & Hex(cpu.reg(rs).r0)
	'Print "RT : " & Hex(cpu.reg(rt).r0)
	cpu.reg(rd).s0 = cpu.reg(rs).s0 - cpu.reg(rt).s0
	'Print "RD : " & Hex(cpu.reg(rd).r0)
	'sleep
End Sub
Sub ee_SW()
	'print "SW"
	Dim addr As ULong
	addr = CShort(imm) + cpu.reg(rs).r0
	write32(addr,cpu.reg(rt).r0)
End Sub
Sub ee_SWL()
	Print"TODO: SWL"
End Sub
Sub ee_SWR()
	Print"TODO: SWR"
End Sub
Sub ee_SYNC()
	'print "SYNC"
End Sub
Sub ee_SYSCALL()
	Print"SYSCALLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL"
End Sub
Sub ee_TEQ()
	Print"TODO: TEQ"
End Sub
Sub ee_TEQI()
	Print"TODO: TEQI"
End Sub
Sub ee_TGE()
	Print"TODO: TGE"
End Sub
Sub ee_TGEI()
	Print"TODO: TGEI"
End Sub
Sub ee_TGEIU()
	Print"TODO: TGEIU"
End Sub
Sub ee_TGEU()
	Print"TODO: TGEU"
End Sub
Sub ee_TLT()
	Print"TODO: TLT"
End Sub
Sub ee_TLTI()
	Print"TODO: TLTI"
End Sub
Sub ee_TLTIU()
	Print"TODO: TLTIU"
End Sub
Sub ee_TLTU()
	Print"TODO: TLTU"
End Sub
Sub ee_TNE()
	Print"TODO: TNE"
End Sub
Sub ee_TNEI()
	Print"TODO: TNEI"
End Sub
Sub ee_XOR()
	cpu.reg(rd).r0 = cpu.reg(rs).r0 Xor cpu.reg(rt).r0
End Sub
Sub ee_XORI()
	cpu.reg(rt).r0 = cpu.reg(rs).r0 xOr imm
End Sub
Sub ee_DEFAULT()
	'print "INVALID OPCODE"
	sleep
End Sub

Sub mmi_DIV1()
	If ((cpu.reg(rs).s0 = &h80000000) AndAlso (cpu.reg(rt).s0 = &hFFFFFFFF)) Then 
		cpu.LO.r0 = &h80000000
		cpu.HI.r0 = 0
	Else 
		cpu.LO.r1 = CInt(cpu.reg(rs).int0 / cpu.reg(rt).int0)
		cpu.HI.r1 = CInt(cpu.reg(rs).int0 Mod cpu.reg(rt).int0)
	EndIf
End Sub
Sub mmi_DIVU1()
	cls
	Print "DIVU1"
End Sub
Sub mmi_LQ()

End Sub
Sub mmi_MADD()

End Sub
Sub mmi_MADD1()

End Sub
Sub mmi_MADDU()

End Sub
Sub mmi_MADDU1()

End Sub
Sub mmi_MFHI1()

End Sub
Sub mmi_MFLO1()
	cpu.reg(rd).r0 = cpu.LO.r1
End Sub
Sub mmi_MFSA()

End Sub
Sub mmi_MTHI1()

End Sub
Sub mmi_MTLO1()

End Sub
Sub mmi_MTSA()

End Sub
Sub mmi_MTSAB()

End Sub
Sub mmi_MTSAH()

End Sub
Sub mmi_MULT()

End Sub
Sub mmi_MULT1()

End Sub
Sub mmi_MULTU()

End Sub
Sub mmi_MULTU1()

End Sub
Sub mmi_PABSH()

End Sub
Sub mmi_PABSW()

End Sub
Sub mmi_PADDB()

End Sub
Sub mmi_PADDH()

End Sub
Sub mmi_PADDSB()

End Sub
Sub mmi_PADDSH()

End Sub
Sub mmi_PADDSW()

End Sub
Sub mmi_PADDUB()

End Sub
Sub mmi_PADDUH()

End Sub
Sub mmi_PADDUW()

End Sub
Sub mmi_PADDW()

End Sub
Sub mmi_PADDSBH()

End Sub
Sub mmi_PAND()

End Sub
Sub mmi_PCEQB()

End Sub
Sub mmi_PCEQH()

End Sub
Sub mmi_PCEQW()

End Sub
Sub mmi_PCGTB()

End Sub
Sub mmi_PCGTH()

End Sub
Sub mmi_PCGTW()

End Sub
Sub mmi_PCPYH()

End Sub
Sub mmi_PCPYLD()

End Sub
Sub mmi_PCPYUD()

End Sub
Sub mmi_PDIVBW()

End Sub
Sub mmi_PDIVUW()

End Sub
Sub mmi_PDIVW()

End Sub
Sub mmi_PEXCH()

End Sub
Sub mmi_PEXCW()

End Sub
Sub mmi_PEXEH()

End Sub
Sub mmi_PEXEW()

End Sub
Sub mmi_PEXT5()

End Sub
Sub mmi_PEXTLB()

End Sub
Sub mmi_PEXTLH()

End Sub
Sub mmi_PEXTLW()

End Sub
Sub mmi_PEXTUB()

End Sub
Sub mmi_PEXTUH()

End Sub
Sub mmi_PEXTUW()

End Sub
Sub mmi_PHMADH()

End Sub
Sub mmi_PHMSBH()

End Sub
Sub mmi_PINTEH()

End Sub
Sub mmi_PINTH()

End Sub
Sub mmi_PLZCW()

End Sub
Sub mmi_PMADDH()

End Sub
Sub mmi_PMADDUW()

End Sub
Sub mmi_PMADDW()

End Sub
Sub mmi_PMAXH()

End Sub
Sub mmi_PMAXW()

End Sub
Sub mmi_PMFHI()

End Sub
Sub mmi_PMFHLLH()

End Sub
Sub mmi_PMFHLLW()

End Sub
Sub mmi_PMFHLSH()

End Sub
Sub mmi_PMFHLSLW()

End Sub
Sub mmi_PMFHLUW()

End Sub
Sub mmi_PMFLO()

End Sub
Sub mmi_PMINH()

End Sub
Sub mmi_PMINW()

End Sub
Sub mmi_PMSUBH()

End Sub
Sub mmi_PMSUBW()

End Sub
Sub mmi_PMTHI()

End Sub
Sub mmi_PMTHLLW()

End Sub
Sub mmi_PMTLO()

End Sub
Sub mmi_PMULTH()

End Sub
Sub mmi_PMULTUW()

End Sub
Sub mmi_PMULTW()

End Sub
Sub mmi_PNOR()

End Sub
Sub mmi_POR()

End Sub
Sub mmi_PPAC5()

End Sub
Sub mmi_PPACB()

End Sub
Sub mmi_PPACH()

End Sub
Sub mmi_PPACW()

End Sub
Sub mmi_PREVH()

End Sub
Sub mmi_PROT3W()

End Sub
Sub mmi_PSLLH()

End Sub
Sub mmi_PSLLVW()

End Sub
Sub mmi_PSLLW()

End Sub
Sub mmi_PSRAH()

End Sub
Sub mmi_PSRAVW()

End Sub
Sub mmi_PSRAW()

End Sub
Sub mmi_PSRLH()

End Sub
Sub mmi_PSRLVW()

End Sub
Sub mmi_PSRLW()

End Sub
Sub mmi_PSUBB()

End Sub
Sub mmi_PSUBH()

End Sub
Sub mmi_PSUBSB()

End Sub
Sub mmi_PSUBSH()

End Sub
Sub mmi_PSUBSW()

End Sub
Sub mmi_PSUBUB()

End Sub
Sub mmi_PSUBUH()

End Sub
Sub mmi_PSUBUW()

End Sub
Sub mmi_PSUBW()

End Sub
Sub mmi_PXOR()

End Sub
Sub mmi_QFSRV()

End Sub
Sub mmi_SQ()

End Sub
Sub mmi_DEFAULT()

End Sub

Sub cop0_BC0F()

End Sub
Sub cop0_BC0FL()

End Sub
Sub cop0_BC0T()

End Sub
Sub cop0_BC0TL()

End Sub
Sub cop0_BFH()

End Sub
Sub cop0_BHINBT()

End Sub
Sub cop0_BXLBT()

End Sub
Sub cop0_DHIN()

End Sub
Sub cop0_DHWBIN()

End Sub
Sub cop0_DHWOIN()

End Sub
Sub cop0_DXIN()

End Sub
Sub cop0_DXLDT()

End Sub
Sub cop0_DXLTG()

End Sub
Sub cop0_DXSDT()

End Sub
Sub cop0_DXSTG()

End Sub
Sub cop0_DXWBIN()

End Sub
Sub cop0_IFL()

End Sub
Sub cop0_IHIN()

End Sub
Sub cop0_IXIN()

End Sub
Sub cop0_IXLDT()

End Sub
Sub cop0_IXLTG()

End Sub
Sub cop0_IXSDT()

End Sub
Sub cop0_IXSTG()

End Sub
Sub cop0_DI()

End Sub
Sub cop0_EI()

End Sub
Sub cop0_ERET()

End Sub
Sub cop0_MFBPC()

End Sub
Sub cop0_MFC0()
	'print "MFC0"
	cpu.reg(rt).r0 = readCop0Reg(rd)
End Sub
Sub cop0_MFDAB()

End Sub
Sub cop0_MFDABM()

End Sub
Sub cop0_MFDVB()

End Sub
Sub cop0_MFDVBM()

End Sub
Sub cop0_MFIAB()

End Sub
Sub cop0_MFIABM()

End Sub
Sub cop0_MFPC()

End Sub
Sub cop0_MFPS()

End Sub
Sub cop0_MTBPC()

End Sub
Sub cop0_MTC0()
	'print "Move to CoProcessor 0 " & rd
	write_Cop0Reg(rd, cpu.reg(rt).int0)
End Sub
Sub cop0_MTDAB()

End Sub
Sub cop0_MTDABM()

End Sub
Sub cop0_MTDVB()

End Sub
Sub cop0_MTDVBM()

End Sub
Sub cop0_MTIAB()

End Sub
Sub cop0_MTIABM()

End Sub
Sub cop0_MTPC()

End Sub
Sub cop0_MTPS()

End Sub
Sub cop0_TLBP()

End Sub
Sub cop0_TLBR()

End Sub
Sub cop0_TLBWI()

End Sub
Sub cop0_TLBWR()

End Sub
Sub cop0_DEFAULT()

End Sub
Sub cop1_ABSS()

End Sub
Sub cop1_ADDS()

End Sub
Sub cop1_ADDAS()

End Sub
Sub cop1_BC1F()

End Sub
Sub cop1_BC1FL()

End Sub
Sub cop1_BC1T()

End Sub
Sub cop1_BCTTL()

End Sub
Sub cop1_CEQS()

End Sub
Sub cop1_CFS()

End Sub
Sub cop1_CLES()

End Sub
Sub cop1_CLTS()

End Sub
Sub cop1_CFC1()

End Sub
Sub cop1_CTC1()

End Sub
Sub cop1_CVTSW()

End Sub
Sub cop1_CVTWS()

End Sub
Sub cop1_DIVS()

End Sub
Sub cop1_LWC1()

End Sub
Sub cop1_MADDS()

End Sub
Sub cop1_MADDAS()

End Sub
Sub cop1_MAXS()

End Sub
Sub cop1_MFC1()

End Sub
Sub cop1_MINS()

End Sub
Sub cop1_MOVS()

End Sub
Sub cop1_MSUBS()

End Sub
Sub cop1_MSUBAS()

End Sub
Sub cop1_MTC1()

End Sub
Sub cop1_MULS()

End Sub
Sub cop1_MULAS()

End Sub
Sub cop1_NEGS()

End Sub
Sub cop1_RSQRTS()

End Sub
Sub cop1_SQRTS()

End Sub
Sub cop1_SUBS()

End Sub
Sub cop1_SUBAS()

End Sub
Sub cop1_SWC1()

End Sub
Sub cop1_DEFAULT()

End Sub
Sub SPECIAL()
	tbl_SPECIAL(cpu.opcode And &h3F)()
End Sub
Sub REGIMM()
	tbl_REGIMM(rt)()
End Sub
Sub COP0()
	tbl_COP0((cpu.opcode Shr 21) And &h1F)()
End Sub
Sub COP1()

End Sub
Sub COP2()

End Sub
Sub MMI()
	Print "MMI OP: " & (cpu.opcode And &h3F)
	tbl_MMI(cpu.opcode And &h3F)()
End Sub
Sub MMII0()

End Sub
Sub MMI1()

End Sub
Sub MMI2()
	Print "MMI2: " & Hex(rs)
	tbl_MMI2(rs)()
End Sub
Sub MMI3()
	Print "MMI3 Op: " & Hex(rs)
	tbl_MMI3(rs)()
End Sub
Sub DecodeOp()
	If logging = 1 Then 
		pain()
	EndIf
	tbl_NORMAL(cpu.opcode Shr 26)()
End sub

