Declare Sub fetchOP()
Declare Sub doBranch()

Type cpus
	Dim PC As ULong 
	Dim reg(0 To 31) As uint128
	Dim HI As ULongInt
	Dim LO As ULongInt
	Dim HI1 As ULongInt
	Dim LO1 As ULongInt
	Dim Opcode As ULong
	Dim branchPC As ULong
	Dim branchPending As UByte
End Type

Dim Shared cpu As cpus

#Include "ee_instructions.bi"
Sub fetchOp()
	 cpu.Opcode = read32(cpu.PC)
	 'Print Hex(cpu.Opcode)
End Sub
Sub init_EE()
	initCop0
	cpu.PC = &hBFC00000
End Sub
Sub doBranch()
	fetchOp()
	DecodeOp()
	cpu.PC = cpu.branchPC
End Sub
Sub run_EE()
	Do
		cpu.reg(0).r0 = 0 
		cpu.reg(0).r1 = 0
		If cpu.branchPending = 1 Then 
			fetchOp()
			DecodeOp()
			cpu.branchPending = 0 
			cpu.PC = cpu.branchPC
		EndIf
		fetchOp()
		decodeOp()
		cpu.PC += 4
		Sleep
	Loop While Not MultiKey(SC_ESCAPE)
	Sleep(10000)
End Sub
