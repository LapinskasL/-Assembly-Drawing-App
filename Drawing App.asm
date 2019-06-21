TITLE Lukas Lapinskas/Drawing app

INCLUDE Irvine32.inc

;-------------------------------------------------
;			CONTROLS
;
;______Key___________________Description__________
;	Arrow keys	_|_	Move the cursor
;	E		_|_	Eraser (black color)
;	F		_|_	Toggle freeroam
;	1-9		_|_	Select from color palette
;	0		_|_	Wacky color
;	C		_|_	Clear drawing
;	[		_|_	Move cursor northwest
;	]		_|_	Move cursor northeast
;	;		_|_	Move cursor southwest
;	'		_|_	Move cursor southeast
;	ESC		_|_	Exit program
;-------------------------------------------------


.data
;---------GLOBALLY USED VARIABLES----------
;holds the current column and row position o f the cursor
currentColumn	BYTE 0
currentRow	BYTE 0

currentColor DWORD 0	;holds currently selected color

;The variable freeroamON is checked to see if it is 1 (ON) or 0 (OFF).
;The reason for two of them is because freeroam is toggled on and off.
;That way, I don't need to know if it's currently ON or OFF to change
;its state. I can change it by just exchanging their values.

freeroamON	BYTE 1
freeroamOFF BYTE 0





.data ;VARIABLES BELOW ARE ONLY USED LOCALLY
;data for setting up color menu
colorArray DWORD 9,10,11,12,13,6,8,14,15
colorNumArray DWORD '1','2','3','4','5','6','7','8','9'
.code
;------------------------------------------------------------
SetUpMenu PROC
; Sets up the whole menu (color palette and freeroam status)
; at the top of the console.
; Receives: none
; Returns: nothing
;------------------------------------------------------------
	push eax
	push ecx
	push esi
	push edi

	xor eax, eax			;set EAX register to 0

	mov esi, OFFSET colorArray	;prepare the two ...
	mov edi, OFFSET colorNumArray	;... arrays for looping
	mov ecx, LENGTHOF colorArray	;loop the length of the arrays
again:							
	push [esi]			;color value
	push [edi]			;color number (1-9)
	call AddColorToPalette		;add color to palette
	add esi, 4			;move to next element
	add edi, 4			;move to next element
loop again

	call SetupFreeroam		;set up freeroam status area

	mov al, currentColumn
	mov al, 4
	mov currentColumn, al		;set currentColumn to 4

	mov al, currentRow
	mov al, 3
	mov currentRow, al		;set currentRow to 2

	pop edi
	pop esi
	pop ecx
	pop eax
	ret
SetUpMenu ENDP



.data ;VARIABLES BELOW ARE ONLY USED LOCALLY
wordON BYTE "ON ", 0	;holds the string "ON " (the space after on is needed so that
			;it can erase the last 'F' in "OFF" when it changes.)
wordOFF BYTE "OFF", 0	;holds the string "OFF"

wordFREEROAM	BYTE "(F) FREEROAM: ", 0	;holds the string "(F) FREEROAM: "
.code
;------------------------------------------------------------
SetupFreeroam PROC
; Writes "(F) FREEROAM: " in the upper right of the console
; window.
; Receives: none
; Returns: nothing
;------------------------------------------------------------
	push edx

	mov dl, 102
	mov dh, 0
	call Gotoxy

	push white
	call SetCharColor

	mov edx, OFFSET wordFREEROAM
	call WriteString

	pop edx
	ret
SetupFreeroam ENDP



;------------------------------------------------------------
UpdateFreeroamStatus PROC
; Prints current status of freeroam in the upper right corner
; of the console window. Either to ON or OFF.
; Receives: none
; Returns: nothing
;------------------------------------------------------------
	push eax
	push edx

	xor eax, eax			;make eax zero

	mov dl, 116			;column 116
	mov dh, 0			;row 0
	call Gotoxy			;move cursor to upper right corner of console window

	mov al, freeroamON
	cmp al, 1
	je changeToON			;if freeroamON variable has value of 1, jump to changeToON label
	push 12				;push color 12 (light green) to be used as a parameter value by SetCharColor
	call SetCharColor		;change text color to green
	mov edx, OFFSET wordOFF
	call WriteString		;write "OFF" in upper right corner

	push currentColor		;go back to the color that was selected initially ...
	call SetSquareColor			;... and apply it.
	jmp done

changeToON:	
	push 10				;push color 10 (light red)
	call SetCharColor		;set character color to red
	mov edx, OFFSET wordON
	call WriteString		;write "ON " in upper right corner

done:
	push 0
	push 0
	call MoveCursor			;return the cursor to where it was before this function ran

	pop edx
	pop eax
	ret
UpdateFreeroamStatus ENDP



;------------------------------------------------------------
AddColorToPalette PROC
; Adds one color to to the palette of 9 colors.
; Receives: [ebp + 8] - a number 0-15 that represents a color
;	    [ebp + 12] - a character
; Returns: nothing
;------------------------------------------------------------
	push ebp
	mov ebp, esp
	push eax


	push [ebp + 12]
	call SetSquareColor		;set the foreground color to the one in [ebp + 8] parameter

	mov al, ' '
	call WriteChar		;write a (space) character in the console
	mov al, [ebp + 8]
	call WriteChar		;write the character at [ebp + 12] in the console

	push 2						
	push 0
	call MoveCursor		;move the cursor 2 columns to the left

	pop eax
	pop ebp
	ret 8			;remove 2 parameters values from the stack and return
AddColorToPalette ENDP



;------------------------------------------------------------
PickRandomColor PROC
; Picks random color out of the palette, with the addition of
; a few other colors. Then, it puts the 
; Receives: none
; Returns: nothing
;------------------------------------------------------------
	push eax
	push ebx
	push ecx

change:
	xor eax, eax		;make EAX zero
	mov ax, 0FFh		;randomly generate colors from 01h to FFh (but exclude any black colors later)
	inc eax				
	call RandomRange	;run function to generate value from said range

	mov ah, 00h			
	mov ecx, 16		;loop 16 times ( 00h, 10, 20, ... E0, F0. These are all black colors)
checkIfBlack:
	cmp al, ah		;compare generated color to a black color
	je change		;if black color is found, jump to generate new color
	add ah, 10h		;increment to test the next black color
loop checkIfBlack

	mov currentColor, eax	;move the new color into currentColor variable
	push currentColor		;set square color ...
	call SetSquareColor			;... to the new color.

	pop ecx
	pop ebx
	pop eax
	ret 		
PickRandomColor ENDP



;------------------------------------------------------------
SetCharColor PROC
; Sets the background color to black and the foreground color
; to [ebp + 8] parameter value.
; Receives: [ebp + 8] - a number 0-15 that represents a color
; Returns: nothing
;------------------------------------------------------------
	push ebp
	mov ebp, esp
	push eax
	push edx

	mov eax, 16
	mov edx, black		;background color
	mul edx
	add eax, [ebp + 8]	;foreground color
	call SetTextColor	;apply color changes

	pop edx
	pop eax
	pop ebp
	ret 4			;remove one parameter from the stack and return
SetCharColor ENDP



;------------------------------------------------------------
SetSquareColor PROC 
; Sets the color of the square used to draw with.
; Receives: [ebp + 8] - a number 0-15 that represents a color
; Returns: nothing
;------------------------------------------------------------
	push ebp
	mov ebp, esp
	push eax
	push edx			;backed up EDX for multiplication (EAX*[ebp+8]=EDX:EAX)

	mov eax, 16
	mul DWORD PTR [ebp + 8]		;set background color (color of the square)
	add eax, black			;set foreground color to black (doens't affect the color of the square
					;because the square is just two (space) characters)
	call SetTextColor		;apply color changes

	mov eax, [ebp + 8]
	mov currentColor, eax		;update currentColor variable with the new change

	pop edx
	pop eax
	pop ebp
	ret 4				;remove one parameter from the stack and return
SetSquareColor ENDP



;------------------------------------------------------------
AlterFreeroam PROC  ;ToggleFreeroam?
; Turns freeroam ON/OFF based on parameter value (1 or 0)
; Receives: [ebp + 8] - digit 1 or 0
; Returns: changes to freeroamOFF and freeroamON variables
;------------------------------------------------------------
	push ebp
	mov ebp, esp
	push eax

	mov eax, [ebp + 8]		;move parameter value into EAX register
	cmp eax, 0
	je turnFreeroamOFF		;if the value is equal to 0, jump to turnFreeroamOFF label

turnFreeroamON:				;else, turn freeroam on
	mov al, 1				
	mov freeroamON, al		;move 1 into freeroamON variable
	xor al, al
	mov freeroamOFF, al		;move 0 into freeroamOFF variable
	jmp done

turnFreeroamOFF:
	xor al, al
	mov freeroamON, al		;move 0 into freeroamON variable
	mov al, 1
	mov freeroamOFF, al		;move 1 into freeroamOFF variable

done:
	pop eax
	pop ebp
	ret 4				;remove one parameter and return
AlterFreeroam ENDP



;------------------------------------------------------------
ColorPicker PROC
; Sets the color based on the key user presses (0-9)
; Receives: [ebp + 8] - hexadecimal value of key pressed by user
; Returns: nothing
;------------------------------------------------------------
	push ebp
	mov ebp, esp
	push edx
	
	mov dx, [ebp + 8]

	cmp dx, 0031h ;if 1 is pressed
	je setBlue
	cmp dx, 0032h ;if 2 is pressed
	je setGreen
	cmp dx, 0033h ;if 3 is pressed
	je setCyan
	cmp dx, 0034h ;if 4 is pressed
	je setRed
	cmp dx, 0035h ; .....
	je setMagenta
	cmp dx, 0036h
	je setBrown
	cmp dx, 0037h
	je setGray
	cmp dx, 0038h
	je setYellow
	cmp dx, 0039h
	je setWhite
	cmp dx, 0030h ;if 0 is pressed
	je randomColor

setBlue:
	push 9
	call SetSquareColor
	jmp done
setGreen:
	push 10
	call SetSquareColor
	jmp done
setCyan:
	push 11
	call SetSquareColor
	jmp done
setRed:
	push 12
	call SetSquareColor
	jmp done
setMagenta:
	push 13
	call SetSquareColor
	jmp done
setBrown:
	push brown
	call SetSquareColor
	jmp done
setGray:
	push gray
	call SetSquareColor
	jmp done
setYellow:
	push yellow
	call SetSquareColor
	jmp done
setWhite:
	push 15
	call SetSquareColor
	jmp done

randomColor:
	call PickRandomColor		;set color to random

	done:
	mov dl, freeroamON
	cmp dl, 0
	jz skip				;if freeroamON has a value of 0 then jump to skip label ...
	push -2
	push 0
	call MoveCursor			;... else, move the cursor back 2 columns
skip:
	mov dl, freeroamON
	cmp dl, 0
	je dontAlter		;don't switch freeroam to OFF if it is already 0 (OFF)
	push 0				
	call AlterFreeroam	;change freeroam status to OFF
	call UpdateFreeroamStatus	;update freeroam status at top right corner of console
	jmp finish
dontAlter:
	push 0
	push 0
	call MoveCursor			;move the cursor back 2 columns

finish:
	pop edx
	pop ebp
	ret 4
ColorPicker ENDP



;------------------------------------------------------------
ToolPicker PROC
; Picks the correct tool out of toolbox based on the key (E or F)
; the user presses. There's only 2 tools though: Eraser and Freeroam.
; The palette is not part of the toolbox.
; If freeroam is ON, it allows the user to move the cursor without
; drawing.
; Receives: [ebp + 8] - the hex value of the key that was pressed
; Returns: nothing
;------------------------------------------------------------
	push ebp
	mov ebp, esp
	push edx

	mov dx, [ebp + 8]	;move hexadecimal value of key pressed into DX register

	cmp dx, 0045h
	je eraser		;if letter E, jump to eraser label
	cmp dx, 0046h
	je freeroam		;if letter F, jump to freeroam label

eraser:	
	push black			
	call SetSquareColor		;set square color to 0 (black)

	mov dl, freeroamON
	cmp dl, 0		;if freeroamON is 0 (OFF) ...
	je dontMove		;... then don't move mouse back 2 columns ???????????????
	push -2
	push 0
	call MoveCursor
dontMove:
	mov dl, freeroamON
	cmp dl, 0
	je dontAlter		;don't switch freeroam to OFF if it is already 0 (OFF)
	push 0				
	call AlterFreeroam	;change freeroam status to OFF
dontAlter:
	jmp done

freeroam:
	;3 lines below below switch freeroam from ON to OFF, or OFF to on, depending on
	;the values present in the two variables.
	mov dl, freeroamON
	xchg freeroamOFF, dl
	mov freeroamON, dl

	mov dl, freeroamON
	cmp dl, 1			;if freeroam is ON
	je doNotMove					
	push -2
	push 0
	call MoveCursor
doNotMove:
	cmp dl, 0
	je skip2			;if freeroam is OFF, jump to skip2
	push 2
	push 0
	call MoveCursor	
skip2:
	push currentColor
	Call SetSquareColor			;set square color to currentColor (so that when freeroam is turned OFF from ON,
					;the color the user had selected is back).

	done:
	call UpdateFreeroamStatus	;Update the OFF/ON word in top right corner of console
	pop edx
	pop ebp
	ret 4
ToolPicker ENDP



;------------------------------------------------------------
MoveCursor PROC
; Moves the cursor based on values pushed for column and row.
; The function adds the values to row and column.
; Column - positive value moves right, negative value moves left
; Row - positive value moves up, negative value moves down
; Receives: [ebp + 8] - amount to move down or up (row)
;	    [ebp + 12] - amount to move left or right (column)
; Returns: nothing
;------------------------------------------------------------
	push ebp
	mov ebp, esp
	push eax
	push edx

	xor eax, eax		;set EAX to 0
	
	mov al, currentColumn	
	add eax, [ebp + 12]	;add number of columns in parameter 
	mov currentColumn, al	;update new cursor column location

	mov al, currentRow
	add eax, [ebp + 8]	;add number of rows in parameter 
	mov currentRow, al	;update new cursor row location

	mov dl, currentColumn
	mov dh, currentRow
	call Gotoxy		;move cursor to the new column and row location

	pop edx
	pop eax
	pop ebp
	ret 8			;remove 2 parameters
MoveCursor ENDP



;------------------------------------------------------------
DrawSquare PROC
; Writes two (space) characters to create a square.
; Receives: none
; Returns: nothing
;------------------------------------------------------------
	push eax

	mov al, ' '	;set AL register to the (space) character
	call WriteChar	;print the character two times
	call WriteChar

	pop eax
	ret
DrawSquare ENDP




;------------------------------------------------------------
; MAIN procedure
;------------------------------------------------------------
main PROC 

start:
	call SetupMenu			;set up the menu	

	push 1				;freeroam is ON initially
	call AlterFreeroam

	Call UpdateFreeroamStatus	;update freeroam's current status

	push 15
	call SetSquareColor			;set initial color to 15 (white)

	call Randomize			;random seed (for random color generation)

LookForKey:
	mov  eax,50			;sleep, to allow OS to time slice
    call Delay				;(otherwise, some key presses are lost)
	call ReadKey			;read key pressed by user and store in DX
	jz LookForKey			

	mov al, currentColumn		;store cursor's current column location
	mov ah, currentRow		;store cursor's current row location

	cmp dx, 0025h			;left arrow key
	je moveLeft
	cmp dx, 0026h			;up arrow key
	je moveUp
	cmp dx, 0027h			;right arrow key
	je moveRight
	cmp dx, 0028h			;down arrow key
	je moveDown

	cmp dx, 0043h			; C key
	je clear

	cmp dx, 00DDh			; [ key
	je moveRightUp
	cmp dx, 00DBh			; ] key
	je moveLeftUp
	cmp dx, 00DEh			; ' key
	je moveRightDown
	cmp dx, 00BAh			; ; key
	je moveLeftDown

	cmp dx, 001Bh			;ESC key
	je exitProgram

	cmp dx, 0039h		
	ja skip				;if key above 0039h, jump to skip label
	cmp dx, 0030h			;if key below 0030h, jump to skip label
	jb skip
	push edx			;push key pressed onto the stack (EDX contains DX)
	call ColorPicker		;set color based on key presses
	jmp draw					

	skip:
	cmp dx, 0045h
	jb LookForKey			;if key below 0045h = (any other key not checked in this function),
					;jump to LookForKey = (don't draw or do anything)
	cmp dx, 0046h
	ja LookForKey			;if key below 0045h = (any other key not checked in this function),
					;jump to LookForKey = (don't draw or do anything)
	push edx			;push key pressed onto the stack (EDX contains DX, and key is stored in DX)
	call ToolPicker			;pick tool based on key pressed = (based on key pushed on stack)
	jmp draw

moveLeft:			
	cmp al, 2					
	jbe nodraw			;if cursor is at column 2 or below, don't move cursor and don't draw
	push -2
	push 0
	call MoveCursor
	jmp draw
moveUp:
	cmp ah, 2
	jbe nodraw			;if cursor is at row 2 or below, don't move cursor and don't draw
	push 0
	push -1
	call MoveCursor
	jmp draw
moveRight:
	cmp al, 116
	jae nodraw			;if cursor is at column 116 or above, don't move cursor and don't draw
	push 2
	push 0
	call MoveCursor
	jmp draw
moveDown:
	cmp ah, 28
	jae nodraw			;if cursor is at row 28 or above, don't move cursor and don't draw
	push 0
	push 1
	call MoveCursor
	jmp draw


moveRightUp:
	cmp al, 116
	jae nodraw			;if cursor is at column 116 or above, don't move cursor and don't draw
	cmp ah, 2
	jbe nodraw			;if cursor is at row 2 or below, don't move cursor and don't draw
	push 2
	push -1
	call MoveCursor
	jmp draw
moveLeftUp:
	cmp al, 2
	jbe nodraw			;if cursor is at column 2 or below, don't move cursor and don't draw
	cmp ah, 2
	jbe nodraw			;if cursor is at row 2 or below, don't move cursor and don't draw
	push -2
	push -1
	call MoveCursor
	jmp draw
moveRightDown:
	cmp al, 116
	jae nodraw			;if cursor is at column 116 or above, don't move cursor and don't draw
	cmp ah, 28
	jae nodraw			;if cursor is at row 28 or above, don't move cursor and don't draw
	push 2
	push 1
	call MoveCursor
	jmp draw
moveLeftDown:
	cmp al, 2
	jbe nodraw			;if cursor is at column 2 or below, don't move cursor and don't draw
	cmp ah, 28
	jae nodraw			;if cursor is at row 28 or above, don't move cursor and don't draw
	push -2
	push 1
	call MoveCursor
	jmp draw

clear:
	push black
	call SetSquareColor			;reset background color to black
	call Clrscr			;clear whole console window
	xor al, al			;set AL to 0
	mov currentColumn, al		;set currentColumn to 0
	mov currentRow, al		;set currentRow to 0
	jmp start			;jmp to start and redraw whole UI again

draw:
	cmp freeroamON, 1
	je noDraw			;if freeroam is on, do not draw
	call DrawSquare			;else, draw a square

noDraw:
	jmp LookForKey			;jump back to LookForKey label

exitProgram:
    exit
main ENDP
END main