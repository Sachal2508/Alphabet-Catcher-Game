[org 0x0100]
jmp start
index: db 0
content: dw 0,0,0,0,0
Alphabet_Swap_Times: dw 0,0,0,0,0
Alphabet_Locations: dw 0,0,0,0,0,0
box_location: dw 3906
rand: dw 0
randnum: dw 0
no_of_alphabets: dw 0
; taking n as parameter, generate random number from 0 to n nad return in the stack
randG:
   push bp
   mov bp, sp
   pusha
   cmp word [rand], 0
   jne next

  MOV AH, 00h   ; interrupt to get system timer in CX:DX 
  INT 1AH
  inc word [rand]
  mov [randnum], dx
  jmp next1

  next:
  mov ax, 25173          ; LCG Multiplier
  mul word  [randnum]     ; DX:AX = LCG multiplier * seed
  add  ax, 13849          ; Add LCG increment value
  ; Modulo 65536, AX = (multiplier*seed+increment) mod 65536
  mov [randnum], ax          ; Update seed = return value

 next1:xor dx, dx
 mov ax, [randnum]
 mov cx, [bp+4]
 inc cx
 div cx
 
 mov [bp+6], dx
 popa
 pop bp
 ret 2

   
delay:
    push ax
    push cx
    push dx
    mov cx, 0100h   ; Outer loop count
outer_loop:
    mov dx, 0100h   ; Inner loop count
inner_loop:
    nop             ; No operation, just burns time
    dec dx          ; Decrement inner loop counter
    jnz inner_loop  ; If not zero, keep looping
    dec cx          ; Decrement outer loop counter
    jnz outer_loop  ; If not zero, keep looping
    pop dx
    pop cx
    pop ax
    ret


clrscr:
 push es
 push ax 
 push cx 
 push di 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 xor di, di ; point di to top left column 
 mov ax, 0x0720 ; space char in normal attribute 
 mov cx, 2000 ; number of screen locations 
 cld ; auto increment mode 
 rep stosw ; clear the whole screen 
 pop di 
 pop cx 
 pop ax 
 pop es 
 ret
 
 Alphaets_Printer:
     pusha
	 ;To get starting position to print
	 cmp word [no_of_alphabets],5
	 je Alphaets_Printer_exit
	 add word [no_of_alphabets],1
	 sub sp, 2
	 push 66
	 call randG
	 pop dx
	 shl dx,1
	 
	 ;TO Get AH value so alphabets get print in differnet color 
	 loooping_shooping:
	 sub sp, 2
	 push 10
	 call randG
	 xor cx,cx
	 mov ax,112
	 pop cx
	 cmp cx,7
	 je loooping_shooping

	 add ax,cx
	 mov bx,ax;
	 mov ah,bl
	 
	 ;TO Calculate which alphabet is gonna print
	 sub sp, 2
	 push 25
	 call randG
	 xor cx,cx
	 pop cx
	 add cl,65;
	 mov al,cl;
	 mov di,dx
	 push 0xb800
	 pop es
	 mov word [es:di],ax
	 xor bx,bx
	 mov bl,[index]
	 shl bl,1
	 mov word [content+bx],ax
	 mov word [Alphabet_Locations+bx],di
	 add byte [index],1
	 
Alphaets_Printer_exit:
     popa
     ret

move_cursor:
    in al, 0x60    ; Read keyboard input
	cmp byte [missed], 10
    je exit       ; Ignore other keys
    cmp al, 0x4D   ; Right arrow key
    je move_right
    cmp al, 0x4B   ; Left arrow key
    je move_left	
	
exit:
	
	jmp far [cs:oldisr]

move_right:
    mov di,[box_location]
    add di,2 
    cmp di,3968 
    jae exit4 
    sub di,2 
    mov cx,3
    mov ax, 0xb800
    mov es, ax
    mov ax, 0x7020
    mov di, [box_location]
    cld
    rep stosw
    add word [box_location], 2
    call cursor
    jmp exit4

move_left:
    mov di, [box_location]
    sub di, 2 
    cmp di, 3840 
    jb exit4 
    add di, 2 

    sub di, 14
    mov cx, 3
    mov ax, 0xb800
    mov es, ax
    mov ax, 0x7020
    mov di, [box_location]
    add di, 4
    cld
    rep stosw
    sub word [box_location], 2
    call cursor
    jmp exit4

exit4:
    mov al, 0x20
    out 0x20, al
    iret


bg_print:
	 push es
	 push ax 
	 push cx 
	 push di 
	 mov ax, 0xb800 
	 mov es, ax ; point es to video base 
	 xor di, di ; point di to top left column 
	 mov ax, 0x7020
	 mov cx, 2000 ; number of screen locations 
	 cld ; auto increment mode 
	 rep stosw ; clear the whole screen 
	 pop di 
	 pop cx 
	 pop ax 
	 pop es 
	 ret

 right_border:
	 push es
	 push ax 
	 push cx 
	 push di 
	 mov ax, 0xb800 
	 mov es, ax ; point es to video base 
	 mov cx,25
	 mov dx,14
	 mov ax,0x4020
	 mov si,136
	 mov di,136
	 outerloop:
	  mov dx,12
	  mov di,si
	  innerloop:
	  mov [es:di],ax
	  add di, 2
	  sub dx,1
	  jnz innerloop
	  add si,160;
	  loop outerloop
	  pop di 
	  pop cx 
	  pop ax 
	  pop es 
	  ret
 
 cursor:

	 push 0xb800
	 pop es
	 mov di, [box_location]
	 mov ax, 0x00DC
	 mov cx, 0
	 again:
	 add di, 2
	 mov [es:di], ax
	 add cx, 1
	 cmp cx, 3
	 jne again
	 ret


printmiss: 
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	mov cx, 8 ; save length in cx
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov di, 300 ; point di to required location
	mov si, str9 ; point si to string
	mov ah, 0x47 ; load attribute in ah
	cld ; auto increment mode
	nextchar: lodsb ; load next char in al
	stosw ; print char/attribute pair
	loop nextchar ; repeat for the whole string
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret

printScore: 
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	mov cx, 8 ; save length in cx
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax,140
	mov di,ax ; point di to required location
	mov si, str8 ; point si to string
	mov ah, 0x47 ; load attribute in ah
	cld ; auto increment mode
	nextchar1: lodsb ; load next char in al
	stosw ; print char/attribute pair
	loop nextchar1 ; repeat for the whole string
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret


prstr: 
	 push bp 
	 mov bp, sp 
	 push es 
	 push ax 
	 push cx 
	 push si 
	 push di 
	 mov ax, 0xb800 
	 mov es, ax ; point es to video base 
	 mov di, [bp+4] ; point di to required location 
	 mov si, [bp+6] ; point si to string 
	 mov cx, [bp+8] ; load length of string in cx 
	 mov ah, [bp+10] 
	 cld ; auto increment mode 
	 nextchar2: 
	 lodsb ; load next char in al 
	 stosw ; print char/attribute pair 
	 loop nextchar2 ; repeat for the whole string 
	 pop di 
	 pop si 
	 pop cx 
	 pop ax 
	 pop es 
	 pop bp 
	 ret 8


printmenu:
	call clrscr
	top:
	push 0x8E
	push 9
	push str1
	push 1170
	call prstr

	push 0x07
	push 7
	push str2
	push 1490
	call prstr

	push 0x07
	push 4
	push str3
	push 1810
	call prstr

	input:
	mov ah, 0
	int 16h
	cmp ah, 0x50
	je down1
	cmp ah, 0x1C
	je end
	jmp input

	end:
	ret

down1:
	call clrscr
	push 0x07
	push 9
	push str1
	push 1170
	call prstr

	push 0x8E
	push 7
	push str2
	push 1490
	call prstr

	push 0x07
	push 4
	push str3
	push 1810
	call prstr

	mov ah, 0
	int 16h
	cmp ah, 0x1C
	je prnames
	cmp ah, 0x50
	je down2
	cmp ah, 0x48
	je top
	jmp input

prnames:
	call clrscr
	push 0x07
	push 23
	push str4
	push 1170
	call prstr

	push 0x07
	push 25
	push str5
	push 1490
	call prstr

	push 0x8E
	push 18
	push str11
	push 1800
	call prstr
	
	mov ah, 0
	int 16h
	cmp ah, 0x01
	je down1
	jmp prnames

down2:
	call clrscr
	push 0x07
	push 9
	push str1
	push 1170
	call prstr

	push 0x07
	push 7
	push str2
	push 1490
	call prstr

	push 0x8E
	push 4
	push str3
	push 1810
	call prstr

	mov ah, 0
	int 16h
	cmp ah, 0x1C
	je prhelp
	cmp ah, 0x50
	je top
	cmp ah, 0x48
	je down1

prhelp:
	call clrscr
	push 0x07
	push 58
	push str6
	push 1120
	call prstr

	push 0x07
	push 57
	push str7
	push 1280
	call prstr
	
	push 0x8E
	push 18
	push str11
	push 1800
	call prstr

	mov ah, 0
	int 16h
	cmp ah, 0x01
	je down2
	jmp prhelp
	

printalpha:
	push 0xb800
	pop es
    mov bx, 0
l1:
    cmp word [no_of_alphabets], 0
    je gocall ; Use a far jump for the "equal" condition

continue_execution:
ll:
    mov bx, 0
    push 0xb800
    pop es
looop:
    mov di, [Alphabet_Locations+bx]
    cmp di, 0
    je skip_to_calling ; Use a near jump to handle the zero condition
    mov cx, [content+bx]
    mov word [es:di], 0x7020
    mov dx, [Alphabet_Swap_Times+bx]
    cmp dx, 22
    jg exittt
    add di, 160
    mov ax, cx
    mov word [es:di], ax
    add word [Alphabet_Locations+bx], 160
    add word [Alphabet_Swap_Times+bx], 1
    add bx, 2
    call delay
    jmp looop

gocall:
jmp calling

exittt:
    add di, 160
	mov word bp, [box_location]
	add word bp, 4
    cmp word bp, di
    je add_score
	sub bp, 2
    cmp word bp, di
    je add_score
	add word bp, 4
    cmp word bp, di
    je add_score

    jmp add_missed

add_score:
    add byte [str8+6], 1
	add byte [score], 1
    cmp byte [score], 10 
    je end_game
    call printScore
    jmp setting

add_missed:
    add byte [str9+7], 1
	add byte [missed], 1
	cmp byte [missed], 10
	je end_game
    call printmiss
	jmp setting
	
end_game:
	call clrscr
	push 0x84
	push 9
	push str10
	push 1344
	call prstr
	
	push 0x07
	push 15
	push str13
	push 1660
	call prstr
	
	push 0xb800
	pop es
	mov ah, 0x0E
	mov byte al, [str8+6]
	mov di, 1690
	mov [es:di], ax
    
	push 0x0E
	push 19
	push str12
	push 1976
	call prstr

    ret
	

setting:
    sub word [no_of_alphabets], 1
    mov word [Alphabet_Locations+bx], 0
	mov word [Alphabet_Swap_Times+bx], 0
    cmp byte [index], 5
    je add_one_byte
    
    jmp going

skip_to_calling:
    mov word [Alphabet_Locations+bx], 0
    jmp calling

add_one_byte:
mov byte [index], 0

going:
    call Alphaets_Printer
	jmp l1

calling:
    call delay
    call Alphaets_Printer
    jmp l1



start:
  
	call printmenu 
	xor ax, ax 
	mov es, ax
	mov ax, [es:9*4] 
	mov [oldisr], ax ; save offset of old routine 
	mov ax, [es:9*4+2] 
	mov [oldisr+2], ax 
	cli
	mov word [es:9*4], move_cursor
	mov word [es:9*4+2], cs
	sti
	call clrscr 
	call bg_print
	call right_border
	call printScore
	call printmiss
	call cursor
	call printalpha
	
	xor ax, ax
	mov es, ax
	cli
	mov ax, [oldisr]
	mov bx, [oldisr + 2]
	mov word [es:9*4], ax
	mov word [es:9*4 + 2], bx
	sti
	
	mov ax,0x4c00
	int 0x21
	oldisr: dd 0
	missed: db 0
	score: db 0
	str1: db 'Play Game'
	str2: db 'Credits'
	str3: db 'Help'
	str4: db '23L-3038  Muhammad Umar'
	str5: db '23L-0973  Muhammad Sachal'
	str6: db 'Press right arrow key to move right and left one for left.'
	str7: db 'If you catch you get a score if you miss 10 game is over.'
	str8: db 'Score:0',0
	str9: db 'Missed:0',0
	str10: db 'Game Over', 
	str11: db 'Press Esc for back'
	str12: db 'Thanks for playing!'
	str13: db 'Your Score is'