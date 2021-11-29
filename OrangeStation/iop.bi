Type IOP
	reg(32) As ULong
	PC As ULong
	HI As ULong
	LO As ULong
	cpuClock As ULong
	opcode As ULong
	branchPC As ULong
	branchPending As UByte
End Type
Type iop_Timer
	tCount As ULong
	tMode As ULong
	tTarget As ULong
End Type
Dim Shared IOP As IOP
#Include Once "iop_instructions.bi"
Dim Shared iop_timers(3) As iop_Timer
Declare Sub iop_timerTick()
Declare Sub init_IOP()
Declare Sub runIOP()
Sub init_IOP()
	iop.cpuClock = 44100 * &H300
	iop.PC = &hBFC00000
	init_IOP_COP0()
End Sub
Sub iop_fetchOP()
	iop.opcode = read32(cpu.PC)
End Sub

Sub run_IOP(cycles As UByte)
	Do
		iop.reg(0) = 0
		If cpu.branchPending = 1 Then 
			fetchOP()
			decodeOp()
			iop.branchPending = 0 
			iop.PC = cpu.branchPC
		EndIf
		iop_fetchOP()
		IOP_DecodeOp()
		Print "IOP PC: " & Hex(iop.pc) & " : " & Hex(iop.Opcode)
		iop.PC += 4
		iop_runCOP0()
		iop_timerTick()
		cycles -= 1
	Loop While(cycles > 0)
End Sub


Sub iop_timerTick()

End Sub