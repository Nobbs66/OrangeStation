Open "trace.txt" For Output As #11
Declare Sub log_ADD()
Declare Sub log_ADDI()
Declare Sub log_ADDIU()
Declare Sub log_ADDU()
Declare Sub log_AND()
Declare Sub log_ANDI()
Declare Sub log_BEQ()
Declare Sub log_BEQL()
Declare Sub log_BGEZ()
Declare Sub log_BGEZAL()
Declare Sub log_BGEZALL()
Declare Sub log_BGEZL()
Declare Sub log_BGTZ()
Declare Sub log_BGTZL()
Declare Sub log_BLEZ()
Declare Sub log_BLEZL()
Declare Sub log_BLTZ()
Declare Sub log_BLTZAL()
Declare Sub log_BLTZALL()
Declare Sub log_BLTZL()
Declare Sub log_BNE()
Declare Sub log_BNEL()
Declare Sub log_BREAK()
Declare Sub log_DADD()
Declare Sub log_DADDI()
Declare Sub log_DADDIU()
Declare Sub log_DADDU()
Declare Sub log_DIV()
Declare Sub log_DIVU()
Declare Sub log_DSLL()
Declare Sub log_DSLL32()
Declare Sub log_DSLLV()
Declare Sub log_DSRA()
Declare Sub log_DSRA32()
Declare Sub log_DSRAV()
Declare Sub log_DSRL()
Declare Sub log_DSRL32()
Declare Sub log_DSRLV()
Declare Sub log_DSUB()
Declare Sub log_DSUBU()
Declare Sub log_J()
Declare Sub log_JAL()
Declare Sub log_JALR()
Declare Sub log_JR()
Declare Sub log_LB()
Declare Sub log_LBU()
Declare Sub log_LD()
Declare Sub log_LDL()
Declare Sub log_LDR()
Declare Sub log_LH()
Declare Sub log_LHU()
Declare Sub log_LUI()
Declare Sub log_LW()
Declare Sub log_LWL()
Declare Sub log_LWR()
Declare Sub log_LWU()
Declare Sub log_MFHI()
Declare Sub log_MFLO()
Declare Sub log_MOVN()
Declare Sub log_MOVZ()
Declare Sub log_MTHI()
Declare Sub log_MTLO()
Declare Sub log_MULT()
Declare Sub log_MULTU()
Declare Sub log_NOR()
Declare Sub log_OR()
Declare Sub log_ORI()
Declare Sub log_PREF()
Declare Sub log_SB()
Declare Sub log_SD()
Declare Sub log_SDL()
Declare Sub log_SDR()
Declare Sub log_SH()
Declare Sub log_SLL()
Declare Sub log_SLLV()
Declare Sub log_SLT()
Declare Sub log_SLTI()
Declare Sub log_SLTIU()
Declare Sub log_SLTU()
Declare Sub log_SRA()
Declare Sub log_SRAV()
Declare Sub log_SRL()
Declare Sub log_SRLV()
Declare Sub log_SUB()
Declare Sub log_SUBU()
Declare Sub log_SW()
Declare Sub log_SWL()
Declare Sub log_SWR()
Declare Sub log_SYNC()
Declare Sub log_SYSCALL()
Declare Sub log_TEQ()
Declare Sub log_TEQI()
Declare Sub log_TGE()
Declare Sub log_TGEI()
Declare Sub log_TGEIU()
Declare Sub log_TGEU()
Declare Sub log_TLT()
Declare Sub log_TLTI()
Declare Sub log_TLTIU()
Declare Sub log_TLTU()
Declare Sub log_TNE()
Declare Sub log_TNEI()
Declare Sub log_XOR()
Declare Sub log_XORI()
Declare Sub log_DEFAULT()

Declare Sub mmilog_DIV1()
Declare Sub mmilog_DIVU1()
Declare Sub mmilog_LQ()
Declare Sub mmilog_MADD()
Declare Sub mmilog_MADD1()
Declare Sub mmilog_MADDU()
Declare Sub mmilog_MADDU1()
Declare Sub mmilog_MFHI1()
Declare Sub mmilog_MFLO1()
Declare Sub mmilog_MFSA()
Declare Sub mmilog_MTHI1()
Declare Sub mmilog_MTLO1()
Declare Sub mmilog_MTSA()
Declare Sub mmilog_MTSAB()
Declare Sub mmilog_MTSAH()
Declare Sub mmilog_MULT()
Declare Sub mmilog_MULT1()
Declare Sub mmilog_MULTU()
Declare Sub mmilog_MULTU1()
Declare Sub mmilog_PABSH()
Declare Sub mmilog_PABSW()
Declare Sub mmilog_PADDB()
Declare Sub mmilog_PADDH()
Declare Sub mmilog_PADDSB()
Declare Sub mmilog_PADDSH()
Declare Sub mmilog_PADDSW()
Declare Sub mmilog_PADDUB()
Declare Sub mmilog_PADDUH()
Declare Sub mmilog_PADDUW()
Declare Sub mmilog_PADDW()
Declare Sub mmilog_PADDSBH()
Declare Sub mmilog_PAND()
Declare Sub mmilog_PCEQB()
Declare Sub mmilog_PCEQH()
Declare Sub mmilog_PCEQW()
Declare Sub mmilog_PCGTB()
Declare Sub mmilog_PCGTH()
Declare Sub mmilog_PCGTW()
Declare Sub mmilog_PCPYH()
Declare Sub mmilog_PCPYLD()
Declare Sub mmilog_PCPYUD()
Declare Sub mmilog_PDIVBW()
Declare Sub mmilog_PDIVUW()
Declare Sub mmilog_PDIVW()
Declare Sub mmilog_PEXCH()
Declare Sub mmilog_PEXCW()
Declare Sub mmilog_PEXEH()
Declare Sub mmilog_PEXEW()
Declare Sub mmilog_PEXT5()
Declare Sub mmilog_PEXTLB()
Declare Sub mmilog_PEXTLH()
Declare Sub mmilog_PEXTLW()
Declare Sub mmilog_PEXTUB()
Declare Sub mmilog_PEXTUH()
Declare Sub mmilog_PEXTUW()
Declare Sub mmilog_PHMADH()
Declare Sub mmilog_PHMSBH()
Declare Sub mmilog_PINTEH()
Declare Sub mmilog_PINTH()
Declare Sub mmilog_PLZCW()
Declare Sub mmilog_PMADDH()
Declare Sub mmilog_PMADDUW()
Declare Sub mmilog_PMADDW()
Declare Sub mmilog_PMAXH()
Declare Sub mmilog_PMAXW()
Declare Sub mmilog_PMFHI()
Declare Sub mmilog_PMFHLLH()
Declare Sub mmilog_PMFHLLW()
Declare Sub mmilog_PMFHLSH()
Declare Sub mmilog_PMFHLSLW()
Declare Sub mmilog_PMFHLUW()
Declare Sub mmilog_PMFLO()
Declare Sub mmilog_PMINH()
Declare Sub mmilog_PMINW()
Declare Sub mmilog_PMSUBH()
Declare Sub mmilog_MPSUBW()
Declare Sub mmilog_PMTHI()
Declare Sub mmilog_PMTHLLW()
Declare Sub mmilog_PMTLO()
Declare Sub mmilog_PMUITH()
Declare Sub mmilog_PMULTUW()
Declare Sub mmilog_PMULTW()
Declare Sub mmilog_PNOR()
Declare Sub mmilog_POR()
Declare Sub mmilog_PPAC5()
Declare Sub mmilog_PPACB()
Declare Sub mmilog_PPACH()
Declare Sub mmilog_PPACW()
Declare Sub mmilog_PREVH()
Declare Sub mmilog_PROT3W()
Declare Sub mmilog_PSLLH()
Declare Sub mmilog_PSLLVW()
Declare Sub mmilog_PSLLW()
Declare Sub mmilog_PSRAH()
Declare Sub mmilog_PSRAVW()
Declare Sub mmilog_PSRAW()
Declare Sub mmilog_PSRLH()
Declare Sub mmilog_PSRLVW()
Declare Sub mmilog_PSRLW()
Declare Sub mmilog_PSUBB()
Declare Sub mmilog_PSUBH()
Declare Sub mmilog_PSUBSB()
Declare Sub mmilog_PSUBSH()
Declare Sub mmilog_PSUBSW()
Declare Sub mmilog_PSUBUB()
Declare Sub mmilog_PSUBUH()
Declare Sub mmilog_PSUBUW()
Declare Sub mmilog_PSUBW()
Declare Sub mmilog_PXOR()
Declare Sub mmilog_QFSRV()
Declare Sub mmilog_SQ()
Declare Sub mmilog_DEFAULT()

Declare Sub cp0log_BC0F()
Declare Sub cp0log_BC0FL()
Declare Sub cp0log_BC0T()
Declare Sub cp0log_BC0TL()
Declare Sub cp0log_BFH()
Declare Sub cp0log_BHINBT()
Declare Sub cp0log_BXLBT()
Declare Sub cp0log_DHIN()
Declare Sub cp0log_DHWBIN()
Declare Sub cp0log_DHWOIN()
Declare Sub cp0log_DXIN()
Declare Sub cp0log_DXLDT()
Declare Sub cp0log_DXLTG()
Declare Sub cp0log_DXSDT()
Declare Sub cp0log_DXSTG()
Declare Sub cp0log_DXWBIN()
Declare Sub cp0log_IFL()
Declare Sub cp0log_IHIN()
Declare Sub cp0log_IXIN()
Declare Sub cp0log_IXLDT()
Declare Sub cp0log_IXLTG()
Declare Sub cp0log_IXSDT()
Declare Sub cp0log_IXSTG()
Declare Sub cp0log_DI()
Declare Sub cp0log_EI()
Declare Sub cp0log_ERET()
Declare Sub cp0log_MFBPC()
Declare Sub cp0log_MFC0()
Declare Sub cp0log_MFDAB()
Declare Sub cp0log_MFDABM()
Declare Sub cp0log_MFDVB()
Declare Sub cp0log_MFDVBM()
Declare Sub cp0log_MFIAB()
Declare Sub cp0log_MFIABM()
Declare Sub cp0log_MFPC()
Declare Sub cp0log_MFPS()
Declare Sub cp0log_MTBPC()
Declare Sub cp0log_MTC0()
Declare Sub cp0log_MTDAB()
Declare Sub cp0log_MTDABM()
Declare Sub cp0log_MTDVB()
Declare Sub cp0log_MTDVBM()
Declare Sub cp0log_MTIAB()
Declare Sub cp0log_MTIABM()
Declare Sub cp0log_MTPC()
Declare Sub cp0log_MTPS()
Declare Sub cp0log_TLBP()
Declare Sub cp0log_TLBR()
Declare Sub cp0log_TLBWI()
Declare Sub cp0log_TLBWR()
Declare Sub cp0log_DEFAULT()

Declare Sub cp1log_ABSS()
Declare Sub cp1log_ADDS()
Declare Sub cp1log_ADDAS()
Declare Sub cp1log_BC1F()
Declare Sub cp1log_BC1FL()
Declare Sub cp1log_BC1T()
Declare Sub cp1log_BCTTL()
Declare Sub cp1log_CEQS()
Declare Sub cp1log_CFS()
Declare Sub cp1log_CLES()
Declare Sub cp1log_CLTS()
Declare Sub cp1log_CFC1()
Declare Sub cp1log_CTC1()
Declare Sub cp1log_CVTSW()
Declare Sub cp1log_CVTWS()
Declare Sub cp1log_DIVS()
Declare Sub cp1log_LWC1()
Declare Sub cp1log_MADDS()
Declare Sub cp1log_MADDAS()
Declare Sub cp1log_MAXS()
Declare Sub cp1log_MFC1()
Declare Sub cp1log_MINS()
Declare Sub cp1log_MOVS()
Declare Sub cp1log_MSUBS()
Declare Sub cp1log_MSUBAS()
Declare Sub cp1log_MTC1()
Declare Sub cp1log_MULS()
Declare Sub cp1log_MULAS()
Declare Sub cp1log_NEGS()
Declare Sub cp1log_RSQRTS()
Declare Sub cp1log_SQRTS()
Declare Sub cp1log_SUBS()
Declare Sub cp1log_SUBAS()
Declare Sub cp1log_SWC1()
Declare Sub cp1log_DEFAULT

Declare Sub log_DecodeOp()
Declare Sub logs_SPECIAL()
Declare Sub logs_REGIMM()
Declare Sub logs_COP0()
Declare Sub logs_BC0()
Declare Sub logs_TLB()
Declare Sub logs_COP1()
Declare Sub logs_COP2()
Declare Sub logs_MMI()
Declare Sub logs_MMI0()
Declare Sub logs_MMI1()
Declare Sub logs_MMI2()
Declare Sub logs_MMI3()
Declare Sub pain()
Dim Shared log_NORMAL(0 To 63) As Sub() => _
{	@logs_SPECIAL,	 @logs_REGIMM,		@log_J, 	 	 @log_JAL, 	  @log_BEQ,  	@log_BNE, 	 @log_BLEZ, 		@log_BGTZ, 	 _ '000
	@log_ADDI, 	 @log_ADDIU, 	@log_SLTI, 	 @log_SLTIU,   @log_ANDI, 	@log_ORI, 	 @log_XORI, 		@log_LUI,  	 _ '001
	@logs_COP0, 	 	@logs_COP1, 	 @logs_COP2,  @log_DEFAULT, @log_BEQL, 	@log_BNEL, 	 @log_BLEZL, 	@log_BGTZL, 	 _ '010
	@log_DADDI,	 @log_DADDIU,	@log_LDL, 	 @log_LDR, 	  @logs_COP2, 		@log_DEFAULT, @mmilog_LQ, 		@mmilog_SQ, 	 _	'011
	@log_LB, 		 @log_LH, 		@log_LWL, 	 @log_LW, 	  @log_LBU, 		@log_LHU, 	 @log_LWR, 		@log_LWU, 	 _ '100
	@log_SB, 		 @log_SH, 		@log_SWL, 	 @log_SW, 	  @log_SDL, 		@log_SDR, 	 @log_SWR, 		@log_DEFAULT, _ '101
	@log_DEFAULT, @cp1log_LWC1, 	@log_DEFAULT, @log_PREF, 	  @log_DEFAULT, @log_DEFAULT, @log_DEFAULT , @log_LD, 		 _ '110
	@log_DEFAULT, @cp1log_SWC1, 	@log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, 	@log_SD 		 _ '111
}

Dim Shared log_SPECIAL(0 To 63) As Sub() => _
{	@log_SLL, 	@log_DEFAULT, @log_SRL,  @log_SRA,  @log_SLLV, 	 @log_DEFAULT, 	@log_SRLV, 		@log_SRAV, 		_
	@log_JR, 		@log_JALR, 	 @log_MOVZ, @log_MOVN, @log_SYSCALL, @log_BREAK, 	@log_DEFAULT, 	@log_SYNC, 		_
	@log_MFHI, 	@log_MTHI, 	 @log_MFLO, @log_MTLO, @log_DSLLV, 	 @log_DEFAULT, 	@log_DSRLV, 		@log_DSRAV, 		_
	@log_MULT, 	@log_MULTU, 	 @log_DIV,  @log_DIVU, @log_DEFAULT, @log_DEFAULT, 	@log_DEFAULT, 	@log_DEFAULT, 	_
	@log_ADD, 	@log_ADDU, 	 @log_SUB,  @log_SUBU, @log_AND, 	 @log_OR, 		@log_XOR, 		@log_NOR, 		_
	@mmilog_MFSA, 	@mmilog_MTSA, 	 @log_SLT,  @log_SLTU, @log_DADD, 	 @log_DADDU, 	@log_DSUB, 		@log_DSUBU, 		_
	@log_TGE, 	@log_TGEU, 	 @log_TLT,  @log_TLTU, @log_TEQ, 	 @log_DEFAULT, 	@log_TNE, 		@log_DEFAULT, 	_
	@log_DSLL, 	@log_DEFAULT, @log_DSRL, @log_DSRA, @log_DSLL32,  @log_DEFAULT, 	@log_DSRL32, 	@log_DSRA32 		_
}

Dim Shared log_REGIMM(0 To 31) As Sub() => _
{	@log_BLTZ, 	@log_BGEZ, 	@log_BLTZL,	 @log_BGEZL,	  @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, _
	@log_TGEI, 	@log_TGEIU, 	@log_TLTI,	 @log_TLTIU,	  @log_TEQI, 	@log_DEFAULT, @log_TNEI,	  @log_DEFAULT, _
	@log_BLTZAL, @log_BGEZAL, @log_BLTZALL, @log_BGEZALL, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, _
	@mmilog_MTSAB, @mmilog_MTSAH, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT  _
}


Dim Shared log_cp0log(0 To 31) As Sub() => _
{ 	@cp0log_MFC0,  @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @cp0log_MTC0,  @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, _
	@logs_BC0, 		 @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, _
	@logs_TLB, 		 @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, _
	@log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT, @log_DEFAULT  _
}
Dim Shared As String * 4 log_regs(0 To 31) = _
{"zero", "at", "v0", "v1", _
"a0", "a1", "a2", "a3", _
"t0", "t1", "t2", "t3", _
"t4", "t5", "t6", "t7", _
"s0", "s1", "s2", "s3", _
"s4", "s5", "s6", "s7", _
"t8", "t9", "k0", "k1", _
"gp", "sp", "fp", "ra"} 

Sub logs_BC0()
	
End Sub
Sub logs_TLB()

End Sub
Sub log_ADD()
	Print #11, hex(cpu.PC) & ": " & "ADD " & log_regs(rd)& "(0x" & Hex(cpu.reg(rd).r0) & ")," & log_regs(rs) & Hex(cpu.reg(rd).r0) & ")," & log_regs(rt) & Hex(cpu.reg(rd).r0) & ")" 
End Sub
Sub log_ADDI()
	Print #11, hex(cpu.pc) & ": " & "ADDI " & log_regs(rt) &  "(0x" & Hex(cpu.reg(rd).r0) & ")," & log_regs(rs) & "," & CShort(imm)
End Sub
Sub log_ADDIU()
	Print #11, hex(cpu.pc) & ": " & "ADDIU " & log_regs(rt) & "," & log_regs(rs) & "," & CShort(imm)
End Sub
Sub log_ADDU()
	Print #11, hex(cpu.pc) & ": " & "ADDU " & log_regs(rd)& "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_AND()
	Print #11, hex(cpu.pc) & ": " & "AND " & log_regs(rd)& "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_ANDI()
	Print #11, hex(cpu.pc) & ": " & "ANDI " & log_regs(rt) & "," & log_regs(rs) & "," & CShort(imm)
End Sub
Sub log_BEQ()
	Print #11, hex(cpu.pc) & ": " & "BEQ " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub log_BEQL()
	Print #11, hex(cpu.pc) & ": " & "BEQL " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub log_BGEZ()
	Print #11, hex(cpu.pc) & ": " & "BGEZ " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub log_BGEZAL()
	Print #11, hex(cpu.pc) & ": " & "BGEZAL " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub log_BGEZALL()
	Print #11, hex(cpu.pc) & ": " & "BGEZALL " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm)Shl 2)
End Sub
Sub log_BGEZL()
	Print #11, hex(cpu.pc) & ": " & "BGEZL " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
End Sub
Sub log_BGTZ()
	Print #11, hex(cpu.pc) & ": " & "BGTZ " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
End Sub
Sub log_BGTZL()
	Print #11, hex(cpu.pc) & ": " & "BGTZL " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
End Sub
Sub log_BLEZ()
	Print #11, hex(cpu.pc) & ": " & "BLEZZ " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
End Sub
Sub log_BLEZL()
	Print #11, hex(cpu.pc) & ": " & "BLEZL " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
End Sub
Sub log_BLTZ()
	Print #11, hex(cpu.pc) & ": " & "BLTZ " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
	Print #11, Hex(cpu.reg(rs).s0) & " : " & Hex(cpu.reg(rt).s0)
End Sub
Sub log_BLTZAL()
	Print #11, hex(cpu.pc) & ": " & "BLTZAL " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
End Sub
Sub log_BLTZALL()
	Print #11, hex(cpu.pc) & ": " & "BLTZALL " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
End Sub
Sub log_BLTZL()
	Print #11, hex(cpu.pc) & ": " & "BLTZL " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
End Sub
Sub log_BNE()
	Print #11, hex(cpu.pc) & ": " & "BNE " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
End Sub
Sub log_BNEL()
	Print #11, hex(cpu.pc) & ": " & "BNEL " & log_regs(rs) & "," & log_regs(rt) & "," & (CShort(imm) Shl 2)
End Sub
Sub log_BREAK()
	Print #11, hex(cpu.pc) & ": " & "BREAK"
	Print "[Critical] Breakpoint Exception 0x:" & Hex(cpu.PC)
	For i As Integer = 0 To 31
		Print log_regs(i) & ": " & Hex(cpu.reg(i).uint3) & "_" & Hex(cpu.reg(i).uint2) & "_" & Hex(cpu.reg(i).uint1) & "_" & Hex(cpu.reg(i).uint0)
	Next
	Print ""
End Sub
Sub log_DADD()
	Print #11, hex(cpu.pc) & ": " & "DADD " & log_regs(rd) & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_DADDI()
	Print #11, hex(cpu.pc) & ": " & "DADDI" & log_regs(rt) & "," & log_regs(rs) & "," & (CShort(imm)Shl 2)
End Sub
Sub log_DADDIU()
	Print #11, hex(cpu.pc) & ": " & "DADDIU " & log_regs(rt) & "," & log_regs(rs) & "," & (CShort(imm)Shl 2)
End Sub
Sub log_DADDU()
	Print #11, hex(cpu.pc) & ": " & "DADDU " & log_regs(rd) & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_DIV()
	Print #11, hex(cpu.pc) & ": " & "DIV " & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_DIVU()
	Print #11, hex(cpu.pc) & ": " & "DIVU " & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_DSLL()
	Print #11, hex(cpu.pc) & ": " & "DSLL " & "," & log_regs(rd) & "," & log_regs(rt) & "," & sa
End Sub
Sub log_DSLL32()
	Print #11, hex(cpu.pc) & ": " & "DSLL32 " & "," & log_regs(rd) & "," & log_regs(rt) & "," & sa
End Sub
Sub log_DSLLV()
	Print #11, hex(cpu.pc) & ": " & "DSLLV " & "," & log_regs(rd) & "," & log_regs(rt) & "," & log_regs(rs)
End Sub
Sub log_DSRA()
	Print #11, hex(cpu.pc) & ": " & "DSRA " & "," & log_regs(rd) & "," & log_regs(rt) & "," & sa
End Sub
Sub log_DSRA32()
	Print #11, hex(cpu.pc) & ": " & "DSRA32 " & "," & log_regs(rd) & "," & log_regs(rt) & "," & sa 
End Sub
Sub log_DSRAV()
	Print #11, hex(cpu.pc) & ": " & "DSRAV " & "," & log_regs(rd) & "," & log_regs(rt) & "," & log_regs(rs)
End Sub
Sub log_DSRL()
	Print #11, hex(cpu.pc) & ": " & "DSRL " & "," & log_regs(rd) & "," & log_regs(rt) & "," & sa 
End Sub
Sub log_DSRL32()
	Print #11, hex(cpu.pc) & ": " & "DSRL32 " & "," & log_regs(rd) & "," & log_regs(rt) & "," & sa 
End Sub
Sub log_DSRLV()
	Print #11, hex(cpu.pc) & ": " & "DSRLV " & "," & log_regs(rd) & "," & log_regs(rt) & "," & log_regs(rs)
End Sub
Sub log_DSUB()
	Print #11, hex(cpu.pc) & ": " & "DSUB " & log_regs(rd) & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_DSUBU()
	Print #11, hex(cpu.pc) & ": " & "DSUBU " & log_regs(rd) & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_J()
	Print #11, hex(cpu.pc) & ": " & "J " & " " & Hex(Target)
End Sub
Sub log_JAL()
	Print #11, hex(cpu.pc) & ": " & "JAL " & " " & Hex(Target)
End Sub
Sub log_JALR()
	Print #11, hex(cpu.pc) & ": " & "JALR " & " " & log_regs(rs)
End Sub
Sub log_JR()
	Print #11, hex(cpu.pc) & ": " & "JR " & " " & log_regs(rs)
End Sub
Sub log_LB()
	Print #11, hex(cpu.pc) & ": " & "LB " & log_regs(rt) & "," & Hex(imm) & "," & log_regs(rs)
End Sub
Sub log_LBU()
	Print #11, hex(cpu.pc) & ": " & "LBUI " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_LD()
	Print #11, hex(cpu.pc) & ": " & "LD " & log_regs(rt) & "," & Hex(imm) & "," & log_regs(rs)
End Sub
Sub log_LDL()
	Print #11, hex(cpu.pc) & ": " & "LDL " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_LDR()
	Print #11, hex(cpu.pc) & ": " & "LDR " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_LH()
	Print #11, hex(cpu.pc) & ": " & "LH " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_LHU()
	Print #11, hex(cpu.pc) & ": " & "LHU " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_LUI()
	Print #11, hex(cpu.pc) & ": " & "LUI " & "," & log_regs(rt) & "," & Hex(imm Shl 16)
End Sub
Sub log_LW()
	Print #11, hex(cpu.pc) & ": " & "LW " & log_regs(rt) & "," & Hex(imm) & "," & log_regs(rs)
End Sub
Sub log_LWL()
	Print #11, hex(cpu.pc) & ": " & "LWL " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_LWR()
	Print #11, hex(cpu.pc) & ": " & "LWR " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_LWU()
	Print #11, hex(cpu.pc) & ": " & "LWU " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_MFHI()
	Print #11, hex(cpu.pc) & ": " & "MFHI " & log_regs(rd)
End Sub
Sub log_MFLO()
	Print #11, hex(cpu.pc) & ": " & "MFLO " & log_regs(rd)
End Sub
Sub log_MOVN()
	Print #11, hex(cpu.pc) & ": " & "MOVN " & log_regs(rd) & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_MOVZ()
	Print #11, hex(cpu.pc) & ": " & "MOVS " & log_regs(rd) & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_MTHI()
	Print #11, hex(cpu.pc) & ": " & "MTHI " & log_regs(rs)
End Sub
Sub log_MTLO()
	Print #11, hex(cpu.pc) & ": " & "MTLO " & log_regs(rs)
End Sub
Sub log_MULT()
	Print #11, hex(cpu.pc) & ": " & "MULT " & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_MULTU()
	Print #11, hex(cpu.pc) & ": " & "MULTU " & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_NOR()
	Print #11, hex(cpu.pc) & ": " & "NOR " & log_regs(rd) & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_OR()
	Print #11, hex(cpu.pc) & ": " & "OR " & log_regs(rd) & "," & log_regs(rs) & "," & log_regs(rt)
End Sub	
Sub log_ORI()
	Print #11, hex(cpu.pc) & ": " & "ORI " & log_regs(rt) & "," & log_regs(rs) & "," & CShort(imm)
End Sub
Sub log_PREF()
	Print #11, hex(cpu.pc) & ": " & "PREF "
End Sub
Sub log_SB()
	Print #11, hex(cpu.pc) & ": " & "SB " & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_SD()
	Print #11, hex(cpu.pc) & ": " & "SD " & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_SDL()
	Print #11, hex(cpu.pc) & ": " & "SDL " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_SDR()
	Print #11, hex(cpu.pc) & ": " & "SDR " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_SH()
	Print #11, hex(cpu.pc) & ": " & "SH " & "," & log_regs(rt) & "," & Hex(CShort(imm))
End Sub
Sub log_SLL()
	If cpu.opcode = 0 Then 
		Print #11, Hex(cpu.pc) & ": " & "NOP"
	Else 
		Print #11, hex(cpu.pc) & ": " & "SLL " & "," & log_regs(rd) & "," & log_regs(rt) & "," & sa 
	EndIf
End Sub
Sub log_SLLV()
	Print #11, hex(cpu.pc) & ": " & "SLLV " & "," & log_regs(rd) & "," & log_regs(rt) & "," & log_regs(rs)
End Sub
Sub log_SLT()
	Print #11, hex(cpu.pc) & ": " & "SLT " & "," & log_regs(rd) & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_SLTI()
	Print #11, hex(cpu.pc) & ": " & "SLTI " & log_regs(rt) & "," & log_regs(rs) & "," & CShort(imm)
End Sub
Sub log_SLTIU()
	Print #11, hex(cpu.pc) & ": " & "SLTIU " & log_regs(rt) & "," & log_regs(rs) & "," & CShort(imm)
End Sub
Sub log_SLTU() 
	Print #11, hex(cpu.pc) & ": " & "SLTU " & "," & log_regs(rd) & "," & log_regs(rs) & "," & log_regs(rt)
End Sub
Sub log_SRA()
	Print #11, hex(cpu.pc) & ": " & "SRA " & "," & log_regs(rd) & "," & log_regs(rt) & "," & sa 
End Sub
Sub log_SRAV()
	Print #11, hex(cpu.pc) & ": " & "SRAV " & "," & log_regs(rd) & "," & log_regs(rt) & "," & log_regs(rs)
End Sub
Sub log_SRL()
	Print #11, hex(cpu.pc) & ": " & "SRL " & log_regs(rd) & "," & log_regs(rt) & "," & sa 
End Sub
Sub log_SRLV()
	Print #11, hex(cpu.pc) & ": " & "SRLV " & log_regs(rd) & "," & log_regs(rt) & "," & log_regs(rs)
End Sub
Sub log_SUB()
	Print #11, hex(cpu.pc) & ": " & "SUB "
End Sub
Sub log_SUBU()
	Print #11, hex(cpu.pc) & ": " & "SUBU " & log_regs(rd) & ": " & Hex(cpu.reg(rd).r0) & ", " & log_regs(rs) & ": " & Hex(cpu.reg(rs).r0) & ", " & log_regs(rt) & ": " & Hex(cpu.reg(rt).r0)
	Print #11, "Opcode: " & Hex(cpu.opcode)
End Sub
Sub log_SW()
	Print #11, hex(cpu.pc) & ": " & "SW "
End Sub
Sub log_SWL()
	Print #11, hex(cpu.pc) & ": " & "SWL "
End Sub
Sub log_SWR()
	Print #11, hex(cpu.pc) & ": " & "SWR "
End Sub
Sub log_SYNC()
	Print #11, hex(cpu.pc) & ": " & "SYNC "
End Sub
Sub log_SYSCALL()
	Print #11, hex(cpu.pc) & ": " & "SYSCALL "
End Sub
Sub log_TEQ()
	Print #11, hex(cpu.pc) & ": " & "TEQ "
End Sub
Sub log_TEQI()
	Print #11, hex(cpu.pc) & ": " & "TEQI "
End Sub
Sub log_TGE()
	Print #11, hex(cpu.pc) & ": " & "TGE "
End Sub
Sub log_TGEI()
	Print #11, hex(cpu.pc) & ": " & "TGEI "
End Sub
Sub log_TGEIU()
	Print #11, hex(cpu.pc) & ": " & "TGEIU "
End Sub
Sub log_TGEU()
	Print #11, hex(cpu.pc) & ": " & "TGEU "
End Sub
Sub log_TLT()
	Print #11, hex(cpu.pc) & ": " & "TLT "
End Sub
Sub log_TLTI()
	Print #11, hex(cpu.pc) & ": " & "TLTI "
End Sub
Sub log_TLTIU()
	Print #11, hex(cpu.pc) & ": " & "TLTIU "
End Sub
Sub log_TLTU()
	Print #11, hex(cpu.pc) & ": " & "TLTU "
End Sub
Sub log_TNE()
	Print #11, hex(cpu.pc) & ": " & "TNE "
End Sub
Sub log_TNEI()
	Print #11, hex(cpu.pc) & ": " & "TNEI "
End Sub
Sub log_XOR()
		Print #11, hex(cpu.pc) & ": " & "XOR "
End Sub
Sub log_XORI()
	Print #11, hex(cpu.pc) & ": " & "XORI "
End Sub
Sub log_DEFAULT()
	Print #11, "FAILED OP" 
End Sub

Sub mmilog_DIV1()

End Sub
Sub mmilog_DIVU1()

End Sub
Sub mmilog_LQ()

End Sub
Sub mmilog_MADD()

End Sub
Sub mmilog_MADD1()

End Sub
Sub mmilog_MADDU()

End Sub
Sub mmilog_MADDU1()

End Sub
Sub mmilog_MFHI1()

End Sub
Sub mmilog_MFLO1()
	Print 
End Sub
Sub mmilog_MFSA()

End Sub
Sub mmilog_MTHI1()

End Sub
Sub mmilog_MTLO1()

End Sub
Sub mmilog_MTSA()

End Sub
Sub mmilog_MTSAB()

End Sub
Sub mmilog_MTSAH()

End Sub
Sub mmilog_MULT()

End Sub
Sub mmilog_MULT1()

End Sub
Sub mmilog_MULTU()

End Sub
Sub mmilog_MULTU1()

End Sub
Sub mmilog_PABSH()

End Sub
Sub mmilog_PABSW()

End Sub
Sub mmilog_PADDB()

End Sub
Sub mmilog_PADDH()

End Sub
Sub mmilog_PADDSB()

End Sub
Sub mmilog_PADDSH()

End Sub
Sub mmilog_PADDSW()

End Sub
Sub mmilog_PADDUB()

End Sub
Sub mmilog_PADDUH()

End Sub
Sub mmilog_PADDUW()

End Sub
Sub mmilog_PADDW()

End Sub
Sub mmilog_PADDSBH()

End Sub
Sub mmilog_PAND()

End Sub
Sub mmilog_PCEQB()

End Sub
Sub mmilog_PCEQH()

End Sub
Sub mmilog_PCEQW()

End Sub
Sub mmilog_PCGTB()

End Sub
Sub mmilog_PCGTH()

End Sub
Sub mmilog_PCGTW()

End Sub
Sub mmilog_PCPYH()

End Sub
Sub mmilog_PCPYLD()

End Sub
Sub mmilog_PCPYUD()

End Sub
Sub mmilog_PDIVBW()

End Sub
Sub mmilog_PDIVUW()

End Sub
Sub mmilog_PDIVW()

End Sub
Sub mmilog_PEXCH()

End Sub
Sub mmilog_PEXCW()

End Sub
Sub mmilog_PEXEH()

End Sub
Sub mmilog_PEXEW()

End Sub
Sub mmilog_PEXT5()

End Sub
Sub mmilog_PEXTLB()

End Sub
Sub mmilog_PEXTLH()

End Sub
Sub mmilog_PEXTLW()

End Sub
Sub mmilog_PEXTUB()

End Sub
Sub mmilog_PEXTUH()

End Sub
Sub mmilog_PEXTUW()

End Sub
Sub mmilog_PHMADH()

End Sub
Sub mmilog_PHMSBH()

End Sub
Sub mmilog_PINTEH()

End Sub
Sub mmilog_PINTH()

End Sub
Sub mmilog_PLZCW()

End Sub
Sub mmilog_PMADDH()

End Sub
Sub mmilog_PMADDUW()

End Sub
Sub mmilog_PMADDW()

End Sub
Sub mmilog_PMAXH()

End Sub
Sub mmilog_PMAXW()

End Sub
Sub mmilog_PMFHI()

End Sub
Sub mmilog_PMFHLLH()

End Sub
Sub mmilog_PMFHLLW()

End Sub
Sub mmilog_PMFHLSH()

End Sub
Sub mmilog_PMFHLSLW()

End Sub
Sub mmilog_PMFHLUW()

End Sub
Sub mmilog_PMFLO()

End Sub
Sub mmilog_PMINH()

End Sub
Sub mmilog_PMINW()

End Sub
Sub mmilog_PMSUBH()

End Sub
Sub mmilog_MPSUBW()

End Sub
Sub mmilog_PMTHI()

End Sub
Sub mmilog_PMTHLLW()

End Sub
Sub mmilog_PMTLO()

End Sub
Sub mmilog_PMUITH()

End Sub
Sub mmilog_PMULTUW()

End Sub
Sub mmilog_PMULTW()

End Sub
Sub mmilog_PNOR()

End Sub
Sub mmilog_POR()

End Sub
Sub mmilog_PPAC5()

End Sub
Sub mmilog_PPACB()

End Sub
Sub mmilog_PPACH()

End Sub
Sub mmilog_PPACW()

End Sub
Sub mmilog_PREVH()

End Sub
Sub mmilog_PROT3W()

End Sub
Sub mmilog_PSLLH()

End Sub
Sub mmilog_PSLLVW()

End Sub
Sub mmilog_PSLLW()

End Sub
Sub mmilog_PSRAH()

End Sub
Sub mmilog_PSRAVW()

End Sub
Sub mmilog_PSRAW()

End Sub
Sub mmilog_PSRLH()

End Sub
Sub mmilog_PSRLVW()

End Sub
Sub mmilog_PSRLW()

End Sub
Sub mmilog_PSUBB()

End Sub
Sub mmilog_PSUBH()

End Sub
Sub mmilog_PSUBSB()

End Sub
Sub mmilog_PSUBSH()

End Sub
Sub mmilog_PSUBSW()

End Sub
Sub mmilog_PSUBUB()

End Sub
Sub mmilog_PSUBUH()

End Sub
Sub mmilog_PSUBUW()

End Sub
Sub mmilog_PSUBW()

End Sub
Sub mmilog_PXOR()

End Sub
Sub mmilog_QFSRV()

End Sub
Sub mmilog_SQ()

End Sub
Sub mmilog_DEFAULT()

End Sub

Sub cp0log_BC0F()

End Sub
Sub cp0log_BC0FL()

End Sub
Sub cp0log_BC0T()

End Sub
Sub cp0log_BC0TL()

End Sub
Sub cp0log_BFH()

End Sub
Sub cp0log_BHINBT()

End Sub
Sub cp0log_BXLBT()

End Sub
Sub cp0log_DHIN()

End Sub
Sub cp0log_DHWBIN()

End Sub
Sub cp0log_DHWOIN()

End Sub
Sub cp0log_DXIN()

End Sub
Sub cp0log_DXLDT()

End Sub
Sub cp0log_DXLTG()

End Sub
Sub cp0log_DXSDT()

End Sub
Sub cp0log_DXSTG()

End Sub
Sub cp0log_DXWBIN()

End Sub
Sub cp0log_IFL()

End Sub
Sub cp0log_IHIN()

End Sub
Sub cp0log_IXIN()

End Sub
Sub cp0log_IXLDT()

End Sub
Sub cp0log_IXLTG()

End Sub
Sub cp0log_IXSDT()

End Sub
Sub cp0log_IXSTG()

End Sub
Sub cp0log_DI()

End Sub
Sub cp0log_EI()

End Sub
Sub cp0log_ERET()

End Sub
Sub cp0log_MFBPC()

End Sub
Sub cp0log_MFC0()
	Print #11, hex(cpu.pc) & ": " & "MFC0: " & log_regs(rt) & " : " & rd
End Sub
Sub cp0log_MFDAB()

End Sub
Sub cp0log_MFDABM()

End Sub
Sub cp0log_MFDVB()

End Sub
Sub cp0log_MFDVBM()

End Sub
Sub cp0log_MFIAB()

End Sub
Sub cp0log_MFIABM()

End Sub
Sub cp0log_MFPC()

End Sub
Sub cp0log_MFPS()

End Sub
Sub cp0log_MTBPC()

End Sub
Sub cp0log_MTC0()
	Print #11, hex(cpu.pc) & ": " & "MTC0: " & log_regs(rt) & " : " & rd
End Sub
Sub cp0log_MTDAB()

End Sub
Sub cp0log_MTDABM()

End Sub
Sub cp0log_MTDVB()

End Sub
Sub cp0log_MTDVBM()

End Sub
Sub cp0log_MTIAB()

End Sub
Sub cp0log_MTIABM()

End Sub
Sub cp0log_MTPC()

End Sub
Sub cp0log_MTPS()

End Sub
Sub cp0log_TLBP()

End Sub
Sub cp0log_TLBR()

End Sub
Sub cp0log_TLBWI()

End Sub
Sub cp0log_TLBWR()

End Sub
Sub cp0log_DEFAULT()

End Sub
Sub cp1log_ABSS()

End Sub
Sub cp1log_ADDS()

End Sub
Sub cp1log_ADDAS()

End Sub
Sub cp1log_BC1F()

End Sub
Sub cp1log_BC1FL()

End Sub
Sub cp1log_BC1T()

End Sub
Sub cp1log_BCTTL()

End Sub
Sub cp1log_CEQS()

End Sub
Sub cp1log_CFS()

End Sub
Sub cp1log_CLES()

End Sub
Sub cp1log_CLTS()

End Sub
Sub cp1log_CFC1()

End Sub
Sub cp1log_CTC1()

End Sub
Sub cp1log_CVTSW()

End Sub
Sub cp1log_CVTWS()

End Sub
Sub cp1log_DIVS()

End Sub
Sub cp1log_LWC1()

End Sub
Sub cp1log_MADDS()

End Sub
Sub cp1log_MADDAS()

End Sub
Sub cp1log_MAXS()

End Sub
Sub cp1log_MFC1()

End Sub
Sub cp1log_MINS()

End Sub
Sub cp1log_MOVS()

End Sub
Sub cp1log_MSUBS()

End Sub
Sub cp1log_MSUBAS()

End Sub
Sub cp1log_MTC1()

End Sub
Sub cp1log_MULS()

End Sub
Sub cp1log_MULAS()

End Sub
Sub cp1log_NEGS()

End Sub
Sub cp1log_RSQRTS()

End Sub
Sub cp1log_SQRTS()

End Sub
Sub cp1log_SUBS()

End Sub
Sub cp1log_SUBAS()

End Sub
Sub cp1log_SWC1()

End Sub
Sub cp1log_DEFAULT()

End Sub
Sub logs_SPECIAL()
	log_SPECIAL(cpu.opcode And &h3F)()
End Sub
Sub logs_REGIMM()
	log_REGIMM(rt)()
End Sub
Sub logs_COP0()
	log_cp0log((cpu.opcode Shr 21) And &h1F)()
End Sub
Sub logs_COP1()

End Sub
Sub logs_COP2()

End Sub
Sub logs_MMI()

End Sub
Sub logs_MMI0()

End Sub
Sub logs_MM1()

End Sub
Sub logs_MM2()

End Sub
Sub logs_MMI3()

End Sub
Sub pain()
	log_NORMAL(cpu.opcode Shr 26)()
End Sub
