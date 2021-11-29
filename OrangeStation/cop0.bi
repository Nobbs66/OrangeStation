Type cop0Regs
	Union 
		Type
			reg(0 To 31) As ULong
		End Type
		Type
			index As ULong		'0
			rand As ULong		'1
			entryLo0 As ULong	'2
			entryLo1 As ULong	'3
			context As ULong	'4
			pageMask As ULong	'5
			wired As ULong		'6
			res0 As ULong		'7
			badVaddr As ULong	'8
			count As ULong		'9
			entryHi As ULong	'10
			compare As ULong	'11
			status As ULong	'12
			cause As ULong		'13
			epc As ULong		'14
			prId As ULong		'15
			config As ULong	'16
			res1 As ULong		'17
			res2 As ULong		'18
			res3 As ULong		'19
			res4 As ULong		'20
			res5 As ULong		'21
			res6 As ULong		'22
			badPaddr As ULong	'23
			debug As ULong		'24
			perf As ULong		'25
			res7 As ULong		'26
			res8 As ULong		'27
			tagLo As ULong		'28
			tagHi As ULong		'29
			errorEPC As ULong	'30
			res9 As ULong		'31
		End Type
		INTC_Stat As ULong
	End Union
End Type
Dim Shared cop0Regs As cop0Regs
Declare Sub write_Cop0Reg(reg As UByte, value As ULong)
Declare Function read_Cop0Reg(reg As UByte) As ULong
Sub initCop0()
	cop0Regs.prId = &h00002e20
	cop0Regs.count = 0
End Sub
Sub run_COP0()
	cop0Regs.count += 2
	If cop0Regs.count = cop0Regs.compare Then 
		cop0Regs.cause Or= (1 Shl 15)
	EndIf
End Sub
Function readCop0Reg(reg As UByte) As ULong
	Select Case reg
		Case 0 To &H31
			Return cop0Regs.reg(reg)
		Case Else 
			Print "Invalid COP0 Read: " & reg
	End Select
End Function
Sub write_Cop0Reg(reg As UByte, value As ULong)
	Select Case reg
		Case 11
			cop0Regs.cause And= Not(1 Shl 15)
			cop0Regs.reg(reg) = value
			Print "Cop0 Compare WRite 0x" & Hex(value)
			sleep
		Case Else
			cop0Regs.reg(reg) = value
	End Select 
End Sub
Sub generateExecption()
	
End Sub
