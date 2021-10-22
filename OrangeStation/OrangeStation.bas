#Include "fbgfx.bi"
#Include "crt.bi"
#Include "file.bi"

#Include "bus.bi"

#Include "ee.bi"
#Include "ee_instructions.bi"
Using fb
ScreenRes(640,480,32)
Declare Sub loadBIOS
Sub loadBIOS
	If FileExists("bios\bios.bin") Then 
		Open "bios\bios.bin" For Binary As #1
	Else 
		Print "Please provide a valid bios rom"
	Sleep
	EndIf
	Get #1,, bus.bios()
	close #1
End Sub


loadBIOS

Print Hex(bus.bios(&h3b41d1))
sleep