;PHASE FINAL
;Group Members
;Abdul Rafay 	 21L-5497
;Zophiel Suleman 21L-1893
[org 0x0100]

jmp start
;-------------------------------------------------------------------
; Data
;-------------------------------------------------------------------
	tickcountR: dw 0
	tickcountG: dw 0
	randomValueR: dw 2560		; random fish locations
	randomValueG: dw 2560
	randomColor1: dw 2			; random coin locations
	randomColor2: dw 4
	fishPosX: dw 18				; initial starting position
	fishPosY: dw 21
	fishPos: dw 2920
	redcoinPos: dw 3124			
	newredCoinPos: dw 3124		; will have same value when randomised as that of coin
	greencoinPos: dw 3050
	newgreenCoinPos: dw 3050	; will have same value when randomised as that of coin
	collisionFlag: dw 0
	coinFlagR: dw 0
	coinFlagG: dw 0
	checkUp: dw 0
	checkDown: dw 0
	checkLeft: dw 0
	checkRight: dw 0
	checkLR: dw 24
	timerflag: dw 0
	terminate: dw 2
	oldkb: dd 0 
	oldtimer: dd 0
	startmsg:db '-----Welcome To Fishy GAME-----'						;31 size
	instructions: db 'Use Arrow Keys to Move the Fish, ESC to Exit'		;44 size
	instructions2: db 'Red Coins = 50	Green Coins = 10'				;31 size				
	entermsg: db 'Press Enter to Start/ESC to Exit'						;32 size
	escmsg: db 'Are you Sure you want to Exit? (Y/N)'					;36 size
	developmsg: db 'Developed by Rafay & Zophiel'						;28 size
	playmsg: db 'play                                '					;36 size
	scoremsg: db 'Score:   '											;9 size
	points: dw 0
	maxlength: dw 80 
	name: db 'Hello' 			; greetings message
	inputmsg: db 'Enter your name: '									;17 size
	buffer: times 81 db 0 		; space for input string

;-------------------------------------------------------------------
; Sound 
;-------------------------------------------------------------------
sound:
		pusha
		mov bx,1500
		soundl:
		in     al,61h            ;  Read current port mode B (8255)
		mov    cl,al             ;  Save current mode
		or     al,3              ;  Switch on speaker and timer
		out    61h,al            ;
		
		mov    al,0b6h           ;  set for channel  2 (8253)
		out    43h,al            ;  command register  8253

		mov    ax,4560           ;  Note of Middle C
		out    42h,al            ;  lower byte of frequency
		
		mov    al,ah             ;
		out    42h,al            ;  higher byte of frequency

		in     al,61h            ;  Read mode of port B (8255)
		mov    al,cl             ;  Previous mode 
		and    al,0FCh           ;
		out    61h,al            ;  Restore mode
		sub bx,1
		cmp bx,0
		jne soundl
		popa
		ret

;-------------------------------------------------------------------
; subroutine to clear the screen 
;-------------------------------------------------------------------
clrscr:	
	push ax
	push cx
	push di
	push es

	mov ax,0xb800
	mov es,ax
	xor di, di
	mov ax, 0x0720
	mov cx, 2000
	
	cld
	rep stosw

	pop es
	pop di 
	pop cx
	pop ax

	ret
;-------------------------------------------------------------------
; delay
;-------------------------------------------------------------------
delay:      
	push cx

	mov cx, 0xFFFF
	loop1:		
	loop loop1

	mov cx, 0xFFFF
	loop2:		
	loop loop2
			
	pop cx
	ret
;-------------------------------------------------------------------
; random number generator ; 2560 to 3360
;-------------------------------------------------------------------
generateValue:
	pusha

	mov bx, 3360
	mov dx, 350

	assignAgain:
	add word[randomValueR], dx
	cmp word[randomValueR], bx
	jb nexttcmp
	mov ax, word[randomValueR]
	sub ax, bx
	mov word[randomValueR], 2560
	add word[randomValueR], ax
	nexttcmp:
	mov ax, word[randomValueG]
	cmp ax, word[randomValueR]	; if its equal then assign sumn else
	je assignAgain

	mov ax, word[randomValueR]	; for the edges
	cmp ax, 2560
	je assignAgain
	cmp ax, 2720
	je assignAgain
	cmp ax, 2880
	je assignAgain
	cmp ax, 3040
	je assignAgain
	cmp ax, 3200
	je assignAgain
	cmp ax, 2718
	je assignAgain
	cmp ax, 2878
	je assignAgain
	cmp ax, 3038
	je assignAgain
	cmp ax, 3198
	je assignAgain
	cmp ax, 3358
	je assignAgain
	
	assignNext:
	mov dx, 470
	add word[randomValueG], dx
	cmp word[randomValueG], bx
	jb nexttcmp2
	mov ax, word[randomValueG]
	sub ax, bx
	mov word[randomValueG], 2560
	add word[randomValueG], ax
	nexttcmp2:
	mov ax, word[randomValueR]
	cmp ax, word[randomValueG]	
	je assignNext

	mov ax, word[randomValueG]	; if its in the edge line
	cmp ax, 2560
	je assignNext
	cmp ax, 2720
	je assignNext
	cmp ax, 2880
	je assignNext
	cmp ax, 3040
	je assignNext
	cmp ax, 3200
	je assignNext
	cmp ax, 2718
	je assignNext
	cmp ax, 2878
	je assignNext
	cmp ax, 3038
	je assignNext
	cmp ax, 3198
	je assignNext
	cmp ax, 3358
	je assignNext

	popa
	ret

resetR:
	pusha

	; clear the old coin
	mov ax, [cs:redcoinPos]
	mov word[cs:newredCoinPos], ax
	call clearRCoin

	; generate random value for new one
	mov ax, 5000
	mov word[redcoinPos], ax
	mov word[newredCoinPos], ax
	mov word[coinFlagR],1
	popa
	ret

resetG:
	pusha

	; clear old coin
	mov ax, [cs:greencoinPos]
	mov word[cs:newgreenCoinPos], ax
	call clearGCoin

	; random value for new one
	mov ax, 6000
	mov word[greencoinPos], ax
	mov word[newgreenCoinPos], ax
	mov word[coinFlagG],1
	popa
	ret
setRed:
	call randR
	ret
setGreen:
	call randG
	ret
randR:
	pusha

	; clear the old coin
	mov ax, [cs:redcoinPos]
	mov word[cs:newredCoinPos], ax
	call clearRCoin

	; generate random value for new one
	mov ax, [randomValueR]
	mov word[redcoinPos], ax
	mov word[newredCoinPos], ax

	popa
	ret

randG:
	pusha

	; clear old coin
	mov ax, [cs:greencoinPos]
	mov word[cs:newgreenCoinPos], ax
	call clearGCoin

	; random value for new one
	mov ax, [randomValueG]
	mov word[greencoinPos], ax
	mov word[newgreenCoinPos], ax

	popa
	ret

generateColor:
	pusha
	mov bx, 15
	mov dx, 3
	assignColor:
	add word[randomColor1], dx
	cmp word[randomColor1], bx
	jb moveOn
	mov ax, word[randomColor1]
	sub ax, bx
	mov word[randomColor1], 0
	add word[randomColor1], ax
	moveOn:
	cmp word[randomColor1], 1	; if its equal then assign sumn else
	je assignColor
	cmp word[randomColor1], 4	; if its equal then assign sumn else
	je assignColor
	popa
	ret
generateColor2:
	pusha 
	mov bx, 15
	mov dx, 3
	assignColor2:
	add word[randomColor2], dx
	cmp word[randomColor2], bx
	jb moveOn2
	mov ax, word[randomColor2]
	sub ax, bx
	mov word[randomColor2], 0
	add word[randomColor2], ax
	moveOn2:
	cmp word[randomColor2], 1	; if its equal then assign sumn else
	je assignColor2
	cmp word[randomColor2], 2	; if its equal then assign sumn else
	je assignColor2
	popa
	ret
;-------------------------------------------------------------------
; dividing screen using this function and background
;-------------------------------------------------------------------
backgroundS:
	push ax
	push cx
	push di
		
	xor di, di			; sky color
	mov ax, 0x0bdb
	mov cx, 560
	
	cld
	rep stosw

	mov di, 1120		; sea color
	mov ax, 0x09db
	mov cx, 640
	
	cld
	rep stosw

	mov di, 2400		; underwater sea color 
	mov ax, 0x01db
	mov cx, 560
	
	cld
	rep stosw

	pop di 
	pop cx
	pop ax

	ret 
;-------------------------------------------------------------------
; print mountains
;-------------------------------------------------------------------
; 960 to 1076 Range
printMountain:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push si
	push di

	mov cx, 0
	mov ax, 0
	mov bx, 0

	mov di, [bp+4]		; load starting point
	mov cl, 21			; height of pyramid
	mov bl, cl			; width of pyramid
	mov ax, 1			; temp

	Pyramid:

	printl1:
	mov word[es:di], 0x0cdb
	add di,2
	loop printl1
	sub bl,4
	mov cl,bl
	mov di,[bp+4]
	mul byte[bp+6]
	sub di,ax
	div byte[bp+6]
	add ax,1
	cmp ax,6
	jne Pyramid
	mov cl,2

	printl2:
	mov word[es:di], 0x0cdb
	add di,2
	loop printl2

	mov cx, 0
	mov ax, 0
	mov bx, 0

	; shadow
	mov ax, 6
	mov cx, 6
	mov di, [bp+4]
	mov bx, 0
	shadowloop:
	mov word[es:di], 0x04db
	sub di, 156
	loop shadowloop 
	sub ax, 1
	mov cx, ax
	mov di, [bp+4]
	add di, bx
	add bx, 2
	cmp ax, 0
	jnz shadowloop

	pop di
	pop si
	pop cx
	pop bx
	pop ax
	pop bp
		
	ret 4
;-------------------------------------------------------------------
; print ship
;------------------------------------------------------------------- 
; 2240 to 2398	
printShip:
	push bp
	mov bp, sp
	push ax
	push cx
	push si
	push di

	; 1st line from bottom
	mov di, [bp+6]		; loads point where itll print
	add di, 4	
	mov cx, [bp+4]
	inc cx
	mov ax, 0x06db		
	cld
	rep stosw
	; 2nd line from bottom	
	mov di, [bp+6]		
	sub di, 160
	mov cx, [bp+4]
	add cx, 5
	cld 
	rep stosw

	; sail
	mov di, [bp+6]
	mov ax, [bp+4]
	add di, ax
	mov bl, 2 ; to calculate midpoint of ship
	div bl
	;	mov ah, 0
	mov bx, ax
	sub di, 320
	;	sub di, 2
	push di
	mov cx, ax
	mov ax, 0x07db
	cld 
	rep stosw
	sub bx, 2
	sailoop:
	pop di
	sub di, 160
	push di
	mov cx, bx
	cld
	rep stosw
	sub bx, 2
	cmp bx, 0
	jne sailoop

	pop di	
	pop di
	pop si
	pop cx
	pop ax
	pop bp
		
	ret 4
;-------------------------------------------------------------------
; clr fish
;-------------------------------------------------------------------
clrFish:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	mov ax, 0xb800
	mov es, ax
	xor ax,ax
	mov bx,[bp+6]
	mov dx,[bp+4]

	mov al, 80				; load al with columns per row
	mul bl		
	cmp dx,80
	jne clrskipthis
	mov dx,0
	clrskipthis:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax

	mov word[es:di], 0x01db
	add dx,1

	mov al, 80				; load al with columns per row
	mul bl		
	cmp dx,80
	jne clrskipthis1
	mov dx,0
	clrskipthis1:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax

	mov word[es:di], 0x01db
	add dx,1

	mov al, 80				; load al with columns per row
	mul  bl		
	cmp dx,80
	jne clrskipthis2
	mov dx,0
	clrskipthis2:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di], 0x01db
	add dx,1

	mov al, 80				; load al with columns per row
	mul  bl		
	cmp dx,80
	jne clrskipthis3
	mov dx,0
	clrskipthis3:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di],0x01db

	mov bx,[bp+6]
	mov dx,[bp+4]
	add bx,1

	mov al, 80				; load al with columns per row
	mul  bl		; 80 x r
	cmp dx,80
	jne clrskipthis4
	mov dx,0
	clrskipthis4:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di], 0x01db
	add dx,1

	mov al, 80				; load al with columns per row
	mul  bl		; 80 x r
	cmp dx,80
	jne clrskipthis5
	mov dx,0
	clrskipthis5:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di], 0x01db
	add dx,1

	mov al, 80				; load al with columns per row
	mul  bl		; 80 x r
	cmp dx,80
	jne clrskipthis6
	mov dx,0
	clrskipthis6:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di], 0x01db
	add dx,1

	mov al, 80				; load al with columns per row
	mul  bl		; 80 x r
	cmp dx,80
	jne clrskipthis7
	mov dx,0
	clrskipthis7:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di], 0x01db	
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
		
	ret 4

;-------------------------------------------------------------------
; print fish
;------------------------------------------------------------------- 
printFish:
	push bp
	mov bp, sp
	push es
	pusha
	push si
	push di
	mov ax, 0xb800
	mov es, ax
	xor ax,ax

	mov bx,[bp+6]			; load fishPosX and Y
	mov dx,[bp+4]

	mov al, 80				; load al with columns per row
	mul bl		
	cmp dx,80
	jb skipthis
	mov dx,0
	skipthis:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax

	mov word[es:di],0x105C
	add dx,1

	mov al, 80				; load al with columns per row
	mul bl		
	cmp dx,80
	jb skipthis1
	mov dx,0
	skipthis1:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax

	mov word[es:di],0x102F
	add dx,1

	mov al, 80				; load al with columns per row
	mul  bl		
	cmp dx,80
	jb skipthis2
	mov dx,0
	skipthis2:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di],0x102D
	add dx,1

	mov al, 80				; load al with columns per row
	mul  bl	
	cmp dx,80
	jb skipthis3
	mov dx,0
	skipthis3:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di],0x105C

	mov bx,[bp+6]
	mov dx,[bp+4]
	add bx,1

	mov al, 80				; load al with columns per row
	mul  bl		
	cmp dx,80
	jb skipthis4
	mov dx,0
	skipthis4:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di],0x102F
	add dx,1

	mov al, 80				; load al with columns per row
	mul  bl		
	cmp dx,80
	jb skipthis5
	mov dx,0
	skipthis5:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di],0x105C
	add dx,1

	mov al, 80				; load al with columns per row
	mul  bl		
	cmp dx,80
	jb skipthis6
	mov dx,0
	skipthis6:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di],0x102D
	add dx,1

	mov al, 80				; load al with columns per row
	mul  bl	
	cmp dx,80
	jb skipthis7
	mov dx,0
	skipthis7:
	add ax, dx			; word number (80xr) + c
	shl ax, 1				; byte no (((80xr) + c)x2)
	mov di,ax
	mov word[es:di],0x102F	
	pop di
	pop si
	popa
	pop es
	pop bp
		
	ret 4
;-------------------------------------------------------------------
; print fish at fishPos
;------------------------------------------------------------------- 
printFishAtPos:
	push es
	pusha
	push di
	mov ax, 0xb800
	mov es, ax
	xor ax, ax

	mov di, [fishPos]
	add di, 2
	mov word[es:di],0x105C
	
	add di, 2
	mov word[es:di],0x102F

	add di, 2
	mov word[es:di],0x102D

	add di, 2
	mov word[es:di],0x105C

	mov di, [fishPos]
	add di, 2
	add di, 160
	mov word[es:di],0x102F

	add di, 2
	mov word[es:di],0x105C

	add di, 2
	mov word[es:di],0x102D

	add di, 2
	mov word[es:di],0x102F	

	pop di
	popa
	pop es
	ret 
;-------------------------------------------------------------------
; clear green coin
;------------------------------------------------------------------- 
clearGCoin:
	push es
	pusha
	push di

	mov ax, 0xb800
	mov es, ax
	xor ax, ax

	mov di, [newgreenCoinPos]
	mov word[es:di], 0x01db
	
	pop di
	popa
	pop es
		
	ret 
;-------------------------------------------------------------------
; clear red coin
;------------------------------------------------------------------- 
clearRCoin:
	push es
	pusha
	push di

	mov ax, 0xb800
	mov es, ax
	xor ax, ax

	mov di, [newredCoinPos]
	mov word[es:di], 0x01db
	
	pop di
	popa
	pop es
		
	ret 
;-------------------------------------------------------------------
; print green coin
;------------------------------------------------------------------- 
printGreenCoin:
	push es
	pusha
	push di

	mov ax, 0xb800
	mov es, ax
	xor ax, ax
	; print 1st coin at the assigned position
	mov di, [greencoinPos]
	mov al, 0xdb
	mov ah, byte[randomColor1]
	mov word[es:di], ax
	
	pop di
	popa
	pop es
		
	ret 
;-------------------------------------------------------------------
; print coin
;------------------------------------------------------------------- 
printRedCoin:
	push es
	pusha
	push di

	mov ax, 0xb800
	mov es, ax
	xor ax, ax
	; print 2nd coin at desired position
	mov di, [redcoinPos]
	mov al, 0xdb
	mov ah, byte[randomColor2]
	mov word[es:di], ax
	
	pop di
	popa
	pop es
		
	ret 

;-------------------------------------------------------------------
; move screen
;------------------------------------------------------------------- 
moveScreen:
	push ax
	mov ax,7		
	push ax
	mov ax,1		;To point to 1st screen
	push ax
	call moveLeft
	mov ax, 15		
	push ax
	mov ax, 8		;To point to 2nd screen
	push ax
	call moveRight
	pop ax

	ret
;-------------------------------------------------------------------
; move Up
;-------------------------------------------------------------------
moveUp:
	push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push si
	push di
	push es
	push ds	
	mov ax, 0xb800
	mov es,ax
	mov ds, ax 	; point ds to video base

	
	mov cx, 480
	mov ax, 80 		; load chars per row in ax
	mul byte [bp+4] ; calculate source position
	mov si, ax 		; load source position in si
	shl si, 1		; convert to bytes
	mov di, si
	add si, 160
	cld
	rep movsw
	mov cx, 80
	mov ax, 80 		; load chars per row in ax
	mul byte [bp+4] ; calculate source position
	mov di, ax 		; load source position in si
	shl di, 1		; convert to bytes
	add di,960
	mov ax, 0x01db
	cld
	rep stosw


	pop ds
	pop es	
	pop di
	pop si
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2
;-------------------------------------------------------------------
; move Down
;-------------------------------------------------------------------
moveDown:
	
	push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push si
	push di
	push es
	push ds	
	mov ax, 0xb800
	mov es,ax
	mov ds, ax 	; point ds to video base
	
	
	mov cx, 480
	mov ax, 80 		; load chars per row in ax
	mul byte [bp+4] ; calculate source position
	mov si, ax 		; load source position in si
	shl si, 1		; convert to bytes
	add si, 1118
	mov di, si
	sub si, 160
	std
	rep movsw
	mov cx, 80
	mov ax, 80 		; load chars per row in ax
	mul byte [bp+4] ; calculate source position
	mov di, ax 		; load source position in si
	shl di, 1		; convert to bytes
	mov ax, 0x01db
	cld
	rep stosw



	pop ds
	pop es	
	pop di
	pop si
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2

;-------------------------------------------------------------------
; move Right
;-------------------------------------------------------------------
moveRight:
	push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push si
	push di
	push es
	push ds
	
	mov ax, 0xb800
	mov es,ax
	mov ds, ax 	; point ds to video base

	movfullRight:
	mov ax, 80 	; load chars per row in ax
	mul byte [bp+4] ; calculate source position
	mov si, ax 	; load source position in si
	shl si, 1 	; convert to byte offset
	mov bx,79
	mov di,si
	add si,158
	mov cx,1
	cld 
	rep movsw 
	;[es:di] = [ds:si]

	mov si, ax 	; load source position in si
	shl si, 1	; convert to byte offset
	add si,156
	mov di, si
	add di,2 
	movlRight:
	mov cx,1
	cld 
	rep movsw 	; scroll Right
	;[es:di] = [ds:si]
	sub si,4
	sub di,4
	sub bx,1
	cmp bx,0
	jne movlRight

	add byte[bp+4],1
	mov ax,word[bp+6]
	cmp word[bp+4],ax
	jne movfullRight

	pop ds
	pop es	
	pop di
	pop si
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 4	
	
 
;-------------------------------------------------------------------
; move Left
;------------------------------------------------------------------- 
moveLeft:
	push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push si
	push di
	push es
	push ds
	
	mov ax, 0xb800
	mov es,ax
	mov ds, ax 	; point ds to video base

	movfullLeft:
	mov ax, 80 	; load chars per row in ax
	mul byte [bp+4] ; calculate source position
	mov si, ax 	; load source position in si
	shl si, 1 	; convert to byte offset
	mov bx,79
	mov di,si
	add di,158
	mov cx,1
	cld
	rep movsw
	;[es:di] = [ds:si]

	mov si, ax 	; load source position in si
	shl si, 1 	; convert to byte offset
	mov di, si
	add si,2 	
	movlLeft:
	mov cx,1
	cld ; set auto increment mode
	rep movsw ; scroll up
	;[es:di] = [ds:si]
	;sub si,4
	;sub di,4

	sub bx,1
	cmp bx,0
	jne movlLeft

	add byte[bp+4],1
	mov ax,word[bp+6]
	cmp [bp+4],ax
	jne movfullLeft

	
	pop ds
	pop es	
	pop di
	pop si
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 4
;--------------------------------------------------------
; Print Using Bios Video Services
;--------------------------------------------------------
PrintMsg:
	push bp
	mov bp,sp
	pusha
	push es
	push di

	mov al,1			;update cursor
	mov bh,0			;page number
	mov bl,[bp+10]		;attribute
	mov dx,[bp+6]		;row and column
	mov cx,[bp+4]		;length of string
	push cs
	pop es
	mov di,bp
	mov ax,0
	mov ah,0x13			;service print string
	mov bp,[bp+8]
	int 0x10
	mov bp,di
	
	pop di
	pop es
	popa
	pop bp
	ret 8
	

;--------------------------------------------------------
; Start Prompt
;--------------------------------------------------------
StartPrompt:
	pusha
	call clrscr 		; call the clrscr subroutine
	;mov dx, name ; greetings message
	;mov ah, 9 ; service 9 � write string
	;int 0x21 ; dos services
	;mov dx, buffer ; user input buffer
	;mov ah, 9 ; service 9 � write string
	;int 0x21 ; dos services
	xor ax,ax
	mov ax,0x0E
	push ax
	mov ax,name		;Prints hello
	push ax
	mov ax,0x0522
	push ax
	mov ax,5
	push ax
	call PrintMsg
	mov ax,0x0E
	push ax
	mov ax,buffer		;Prints Name
	push ax
	mov ax,0x0529
	push ax
	mov ax,25
	push ax
	call PrintMsg
	mov ax,0x0E
	push ax
	mov ax,startmsg		;Prints Welcome
	push ax
	mov ax,0x0618
	push ax
	mov ax,31
	push ax
	call PrintMsg

	mov ax,0x0E
	push ax
	mov ax,instructions	;Prints Game Instruction
	push ax
	mov ax,0x0812
	push ax
	mov ax,44
	push ax
	call PrintMsg

	mov ax,0x0E
	push ax
	mov ax,instructions2	;Prints Coin Instruction
	push ax
	mov ax,0x0916
	push ax
	mov ax,31
	push ax
	call PrintMsg

	mov ax,0x08E
	push ax
	mov ax,entermsg	; Prints Enter Msg
	push ax
	mov ax,0x0B18
	push ax
	mov ax,32
	push ax
	call PrintMsg
	
	mov ax,0x0E
	push ax
	mov ax,developmsg	;Prints Names
	push ax
	mov ax,0x1732
	push ax
	mov ax,28
	push ax
	call PrintMsg

	popa
	ret
;--------------------------------------------------------
; Esc Prompt
;--------------------------------------------------------
EscPrompt:
	push ax
	mov ax,0x07
	push ax
	mov ax,escmsg	;ESC prompt print
	push ax
	mov ax,0x1600
	push ax
	mov ax,36
	push ax
	call PrintMsg
	
	pop ax
	ret

;--------------------------------------------------------
; play Prompt
;--------------------------------------------------------
PlayPrompt:
	push ax
	mov ax,0x07
	push ax
	mov ax,playmsg	;Play prompt print
	push ax
	mov ax,0x1600
	push ax
	mov ax,36
	push ax
	call PrintMsg
	
	pop ax
	ret

;--------------------------------------------------------
; Prints Number
;--------------------------------------------------------
printnum: 
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di

	mov ax, 0xb800
	mov es, ax			; point es to video base
	mov ax, [bp+4]		; load number in ax= 4529
	mov bx, 10			; use base 10 for division
	mov cx, 0			; initialize count of digits

	nextdigit:		
	mov dx, 0			; zero upper half of dividend
	div bx				; divide by 10 AX/BX --> Quotient --> AX, Remainder --> DX ..... 
	add dl, 0x30		; convert digit into ascii value
	push dx				; save ascii value on stack

	inc cx				; increment count of values
	cmp ax, 0			; is the quotient zero
	jnz nextdigit		; if no divide it again

	mov di, 14			; point di to top left column
	nextpos:		
	pop dx				; remove a digit from the stack
	mov dh, 0x07		; use normal attribute
	mov [es:di], dx		; print char on screen
	add di, 2			; move to next screen location
	loop nextpos		; repeat for all digits on stack

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2
;--------------------------------------------------------
; Initialize
;--------------------------------------------------------
Initialize:
	push ax
	push es
	mov ax, 0xb800
	mov es, ax 			; point es to video base

	call clrscr 		; call the clrscr subroutine
	
	call backgroundS

	; PRINTING MOUNTAIN
	mov ax, 156			; Value for dec
	push ax
	mov ax, 990
	push ax
	call printMountain
	mov ax, 156			; Value for dec
	push ax
	mov ax, 1040
	push ax
	call printMountain
	mov ax, 156			; Value for dec
	push ax
	mov ax, 1074
	push ax
	call printMountain
	
	mov word[es:390], 0x0edb
	mov word[es:392], 0x0edb

	; PRINTING SHIP
	mov ax, 2082		; position
	push ax
	mov ax, 16			; size
	push ax
	call printShip
	mov ax, 2300		; position
	push ax
	mov ax, 20			; size
	push ax
	call printShip

	; PRINTING FISH
	mov ax, [fishPosX]
	push ax
	mov ax, [fishPosY]
	push ax
	call printFish

	; PRINTING SCORE MSG
	mov ax, 0x07
	push ax
	mov ax, scoremsg	;Prints Instruction
	push ax
	mov ax, 0x0000
	push ax
	mov ax, 9
	push ax
	call PrintMsg

	pop es
	pop ax
	ret
		
;--------------------------------------------------------
; keyboard interrupt service routine
;--------------------------------------------------------
kbisr:		push ax
			in al, 0x60				; read char from keyboard port
coinCheck1:
			cmp word[coinFlagG], 1
			jne coinCheck2
			cmp al, 0x2a 
			jne coinCheck2
			call setGreen
			mov word[coinFlagG], 0
coinCheck2:
			cmp word[coinFlagR], 1
			jne Rightcmp
			cmp al, 0x36 
			jne Rightcmp
			call setRed
			mov word[coinFlagR], 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Rightcmp:	cmp al, 0x4d			; has the right key been pressed
			jne Leftcmp				; no, try next comparison	

			cmp word[checkLR], 80	; if fish has reached the end then reset
			jne go					
			sub word[fishPos], 158
			mov word[checkLR], 0
			jmp go1
			go:
			add word[checkLR], 1
			go1:
			mov ax, 22		
			push ax
			mov ax, 15				; to print 3rd screen
			push ax
			call moveRight
			add word[fishPos] , 2	; updates fish position
			mov ax, [greencoinPos]	; calculates new position of new position so it can be cleared
			mov word[newgreenCoinPos], ax
			add word[newgreenCoinPos] , 2
			mov ax, [redcoinPos]
			mov word[newredCoinPos], ax
			add word[newredCoinPos] , 2
			call clearRCoin
			call clearGCoin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Leftcmp:	
			cmp al, 0x4b			; has the left key been pressed
			jne Upcmp				; no, try next comparison

			cmp word[checkLR], 0
			jne go2
			add word[fishPos], 158
			mov word[checkLR], 80
			jmp go3
			go2:
			sub word[checkLR], 1
			go3:
			mov ax, 22		
			push ax
			mov ax, 15
			push ax
			call moveLeft
			sub word[fishPos] , 2
			mov ax, [greencoinPos]
			mov word[newgreenCoinPos], ax
			sub word[newgreenCoinPos] , 2
			mov ax, [redcoinPos]
			mov word[newredCoinPos], ax
			sub word[newredCoinPos] , 2
			call clearRCoin
			call clearGCoin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Upcmp:	
			cmp al, 0x48 			; has Up key been pressed
			jne Downcmp 			; no, try next comparison
			
			cmp word[checkUp],3		; if fish is already at the limit 
			je makeSound
			add word[checkUp],1		; up++
			sub word[checkDown],1	
			mov ax, 15
			push ax
			call moveUp	
			sub word[fishPos] , 160
			mov ax, [greencoinPos]
			mov word[newgreenCoinPos], ax
			sub word[newgreenCoinPos] , 160
			mov ax, [redcoinPos]
			mov word[newredCoinPos], ax
			sub word[newredCoinPos] , 160
			call clearRCoin
			call clearGCoin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Downcmp:	
			cmp al, 0x50			 ; has Down key been pressed
			jne checkterminate	 	 ; no, try next comparison
			cmp word[checkDown], 2	 ; if fish is already at the limit 
			je  checkterminate
			add word[checkDown], 1	 ; down++
			sub word[checkUp], 1
			mov ax, 15
			push ax
			call moveDown
			add word[fishPos] , 160
			mov ax, [greencoinPos]
			mov word[newgreenCoinPos], ax
			add word[newgreenCoinPos] , 160
			mov ax, [redcoinPos]
			mov word[newredCoinPos], ax
			add word[newredCoinPos] , 160
			call clearRCoin
			call clearGCoin
			jmp checkterminate

	makeSound:
			call sound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkcancel:	
			cmp al, 0x31	; has N pressed
			jne checkconfirm		; no, try next comparison
			mov word[terminate],0
			jmp exit
checkterminate:	
			cmp al, 0x01	; has ESC pressed
			jne checkEnter	; no, try next comparison
			mov word[terminate],1
			jmp exit
checkconfirm:
			cmp al,0x15		; has Y pressed
			jne nomatch
			mov word[terminate],2
			jmp exit
checkEnter:
			cmp al,0x1c		; has Enter pressed
			jne checkcancel
			mov word[terminate],0
			jmp exit
nomatch:
			pop ax
			jmp far [cs:oldkb] ; call original ISR
exit:		
			mov al, 0x20
			out 0x20, al ; send EOI to PIC
			pop ax
			iret ; return from interrupt
;-------------------------------------------------------------------
; check collision
;------------------------------------------------------------------- 
Collision:
	push es	
	push ds
	pusha
	push di
	; incase of collision, update points based on color, sets/resets collision flag, 
	; updates tickcount and position of that coin
	; red coin=50 & green coin=10 
	mov word[collisionFlag], 0

	; for upper half of fish
	mov dx, 0
	mov cx, 6
	checkUpperPart:
	mov ax, [fishPos]
	add ax, dx

	mov bx, [greencoinPos]
	cmp ax, bx
	jne coincmp2
	
	; if it is in contact with the 1st coin
	mov word[collisionFlag], 1
	call resetG					; assign new coin position
	call printFishAtPos			; also print fish at its current position
	add word[points], 10
	cmp word[randomColor1], 2	; if that coin is green then 10 points
	je points10
	sub word[points], 5			; otherwise 5 points
	points10:
	mov word[tickcountG], 0		; reset tick count as well
	jmp checkBottomPart
	call generateColor
	coincmp2:
	mov bx, [redcoinPos]
	cmp ax, bx
	jne exitt
	
	; if it is in contact with the 2nd coin
	mov word[collisionFlag], 1
	call resetR
	call printFishAtPos
	add word[points], 50
	cmp word[randomColor2], 4	; if that coin is red then 50 points
	je points10_2
	sub word[points], 45		; 5 points otherwise
	points10_2:
	mov word[tickcountR], 0
	call generateColor2
	jmp checkBottomPart

	exitt:
	add dx, 2
	loop checkUpperPart
	; for bottom half of fish
	mov dx, 0
	mov cx, 6
	checkBottomPart:
	mov ax, [fishPos]
	add ax, dx
	add ax, 160
	mov bx, [greencoinPos]
	cmp ax, bx
	jne coincmp3
	; if it is in contact with the 1st coin
	mov word[collisionFlag], 1
	call resetG
	call printFishAtPos
	add word[points], 10
	cmp word[randomColor1], 2
	je points10_
	sub word[points], 5
	points10_:
	mov word[tickcountG], 0
	call generateColor
	jmp exitcollision
	coincmp3:
	mov bx, [redcoinPos]
	cmp ax, bx
	jne exitt2
	; if it is in contact with the 2nd coin
	mov word[collisionFlag], 1
	call resetR
	call printFishAtPos
	add word[points], 50
	cmp word[randomColor2], 4
	je points10_2_
	sub word[points], 45
	points10_2_:
	mov word[tickcountR], 0
	call generateColor2
	jmp exitcollision
	exitt2:
	add dx, 2
	loop checkBottomPart

	exitcollision:
	pop di
	popa
	pop ds
	pop es
	ret 
;--------------------------------------------------------
; check lifespan of both coins
;--------------------------------------------------------
checkLifeSpan:
	cmp  word [cs:tickcountR], 91	; check if 5 seconds have passed
	jb nextTimecmp
	
	call generateColor2
	call randR						; to assign new position to coin
	mov word[cs:tickcountR], 0		; and reset tickcount

	nextTimecmp:
	cmp  word [cs:tickcountG], 182	; check if 10 seconds have passed
	jb exitCLS

	call generateColor
	call randG						; to assign new position to coin
	mov word[cs:tickcountG], 0

	exitCLS:
	ret
;--------------------------------------------------------
; timer interrupt service routine
;--------------------------------------------------------
timer:
	push ax

	; rand function
	call generateValue

	inc word [cs:tickcountR]; increment tick count for red coin
	inc word [cs:tickcountG]; increment tick count for green coin

	cmp word[cs:terminate], 0
	jne pausetime

	; two conditions, collision & lifespan: 

	call Collision
	cmp word[collisionFlag], 0	; if collision didnt happen, it checks the lifespan of the coin
	jne keepPrinting
	
	call checkLifeSpan

	keepPrinting:
	call printGreenCoin
	call printRedCoin

	; print points on top of screen
	mov ax, [cs:points]
	push ax
	call printnum

	; move first two halves of screen
	call moveScreen
	pausetime:
	mov al, 0x20
	out 0x20, al				; end of interrupt

	pop ax
	iret						; return from interrupt
;--------------------------------------------------------
; Input
;--------------------------------------------------------
EnterInput:
	push ax
	push es
	push di

	mov ax,0xb800
	mov es,ax
	call clrscr
	; printing input prompt
	mov ax,0x07
	push ax
	mov ax,inputmsg	;Prints Instruction
	push ax
	mov ax,0x1600
	push ax
	mov ax,17
	push ax
	call PrintMsg
	mov cx, [maxlength] ; load maximum length in cx
	mov si, buffer ; point si to start of buffer
	nextchar: 
	mov ah, 1 ; service 1 � read character
	int 0x21 ; dos services
	cmp al, 13 ; is enter pressed
	je exitin ; yes, leave input
	mov [si], al ; no, save this character
	inc si ; increment buffer pointer
	loop nextchar ; repeat for next input char
	exitin: 
	pop di
	pop es
	pop ax
	ret						
;-------------------------------------------------------------------
start:
	
	call EnterInput
	xor ax, ax
	mov es, ax			; point es to IVT base
	mov ax, [es:9*4]
	mov [oldkb], ax		; save offset of old routine
	mov ax, [es:9*4+2]
	mov [oldkb+2], ax	; save segment of old routine
	mov ax, [es:8*4]
	mov [oldtimer], ax		; save offset of old routine
	mov ax, [es:8*4+2]
	mov [oldtimer+2], ax	; save segment of old routine

	cli		
	mov word [es:9*4], kbisr	; store offset at n*4
	mov [es:9*4+2], cs			; store segment at n*4+2
	mov word [es:8*4], timer	; store offset at n*4
	mov [es:8*4+2], cs			; store segment at n*4+2
	sti	

	call StartPrompt
waitStart:
	cmp word[terminate],2							; is the Esc key pressed
	je waitStart
	cmp word[terminate],1							; is the Esc key pressed
	je earlyterminate

	call Initialize
	
prel1: call PlayPrompt
l1:			
		cmp word[terminate],0							; is the Esc key pressed
		je l1											; if no, check for next key
		call EscPrompt
postl1:
		cmp word[terminate],1
		je postl1

		cmp word[terminate],0
		je prel1
		
		call clrscr
earlyterminate:
		mov ax, [oldkb]									; read old offset in ax
		mov bx, [oldkb+2]								; read old segment in bx

		cli												; disable interrupts
		mov [es:9*4], ax								; restore old offset from ax
		mov [es:9*4+2], bx								; restore old segment from bx
		sti												; enable interrupts

		mov ax, [oldtimer]								; read old offset in ax
		mov bx, [oldtimer+2]							; read old segment in bx
		cli												; disable interrupts
		mov [es:8*4], ax								; restore old offset from ax
		mov [es:8*4+2], bx								; restore old segment from bx
		sti	

		mov ax, 0x4c00									; terminate program
		int 0x21


; ROWs
; 0 to 6-------0 to 960
; 1stline: 1120
; 8 to 14 -----1280 to 2240
; 2ndline: 2400
; 16 to 22-----2560 to 3520
; 3rdline: 3680
; :>