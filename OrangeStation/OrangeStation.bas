#Include "fbgfx.bi"
Using fb
#Include "crt.bi"
#Include "file.bi"

#Include "types.bi"
#Include "bus.bi"

#Include "ee.bi"
'#Include "ee_instructions.bi"

ScreenRes(640,480,32)
Declare Sub loadBIOS
Sub loadBIOS()
	Dim regs(0 To 31) As uint128
	If FileExists("bios\bios.bin") Then 
		Open "bios\bios.bin" For Binary As #1
	Else 
		Print "Please provide a valid bios rom"
	Sleep
	EndIf
	Get #1,, bus.bios()
	close #1

End Sub


loadBIOS()

init_EE()
run_EE()
Print "DOing stuff"
Sleep
sleep