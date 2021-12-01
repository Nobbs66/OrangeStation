Type iop_cop0Reg
	Union 
		Type
			reg(32) As ULong
		End Type
		Type
			na(0 To 2) As ULong
			BPC As ULong
			nall As ULong
			BDA As ULong
			JUMPDEST As ULong
			DCIC As ULong
			BadVaddr As ULong
			BDAM As ULong
			reserved As ULong
			BPCM As ULong
			SR As ULong
			CAUSE As ULong
			EPC As ULong
			PRID As ULong
			garbage(16 To 31) As ULong
		End Type
	End Union
End Type
Dim Shared iop_cop0Reg As iop_cop0Reg
Declare Sub init_IOP_COP0()
Sub init_IOP_COP0()
	iop_cop0Reg.PRID = &h1F
	iop_cop0Reg.SR = &h400000
End Sub

Sub iop_runCOP0()
	'TODO: Fill in COP0
End Sub
