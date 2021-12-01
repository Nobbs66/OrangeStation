Declare Sub fetchOP()
Declare Sub doBranch()


Type cpus
	Dim PC As ULong 
	Dim reg(0 To 31) As uint128
	Dim HI As uint128
	Dim LO As uint128
	'Dim HI As ULongInt
	'Dim LO As ULongInt
	Dim HI1 As ULongInt
	Dim LO1 As ULongInt
	Dim Opcode As ULong
	Dim branchPC As ULong
	Dim branchPending As UByte
End Type
Type timers
	TN_Mode As ULong
	TN_Count As ULong
	TN_Comp As ULong
	TN_Hold As ULong
	Enum mode
		clock = 0
		gateEnable = 2
		gateType = 3
		gateMode = 4
		countClear = 6
		timeEnable = 7
		compInt = 8
		ovfInt = 9
		clearComp = 10
		clearOvf = 11
	End Enum
End Type

Dim Shared cpu As cpus
Dim Shared cpuTimer(0 To 3) As timers
#Include "ee_instructions.bi"

Sub fetchOp()
	 cpu.Opcode = read32(cpu.PC)
End Sub
Sub init_EE()
	initCop0
	cpu.PC = &hBFC00000
	stepping = 0
End Sub
Sub doBranch()
	run_COP0()
	fetchOp()
	DecodeOp()
	cpu.PC = cpu.branchPC
End Sub
Sub dumpRegs()
	Print #11, "V0: 0x" & Hex(cpu.reg(2).r0)
	Print #11, "V1: 0x" & Hex(cpu.reg(3).r0)
	Print #11, "A0: 0x" & Hex(cpu.reg(4).r0)
	Print #11, "A1: 0x" & Hex(cpu.reg(5).r0)
End Sub
Sub run_EE(cycles As UByte)
	Do
		run_COP0()
		cpu.reg(0).r0 = 0 
		cpu.reg(0).r1 = 0
		If cpu.branchPending = 1 Then 
			fetchOp()
			If cpu.PC = &h9FC42560 Then dumpRegs()
			DecodeOp()
			cpu.branchPending = 0 
			cpu.PC = cpu.branchPC
		EndIf
		fetchOp()
		If cpu.PC = &h9FC42560 Then dumpRegs()
		decodeOp()
		
		'Print "EE PC: " & Hex(cpu.pc) & " : " & Hex(cpu.Opcode)
		
		cpu.PC += 4
		tickTimer()
		If MultiKey(SC_SPACE) Then stepping = 1
		If MultiKey(SC_BACKSPACE) Then stepping = 0
		If stepping = 1 Then Print "PC: " & Hex(cpu.pc)
		If stepping = 1 Then Print "Opcode: 0x" & Hex(cpu.opcode)
		'If stepping = 1 Then Sleep
		cycles -= 1
	Loop While(cycles > 0)
	'Sleep(10000)
End Sub
