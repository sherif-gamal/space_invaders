.model small
.stack 200
.data
;=========================================
res		   db ?
invaders   db 10 dup (4)
left	   db 30
top		   db 10
dir		   db 0
bullet_f   db 0
hero_pos   dw 60300
bullet_pos dw 00000
rand_bul   dw 0
oldInt9    dw 2 dup(?)
timer	   db 1
num		   db 0
exit_f     db 0
welcome    db 'WELCOME$'
exit_msg   db 'BYE BYE YA 7ALAWA :)$'
.code
start:
	mov ax, @data
	mov ds, ax
	BTNSPACE 	equ 39h
	BTNRIGHT 	equ 4dh
	BTNLEFT	    equ	4bh
	BTNUP		equ 48h
	BTNDOWN		equ	50h
	ESCAPE		equ 1h
	cli ;Turn off interrupts!
	mov ax, 0
	mov es, ax
	mov ax, es:[9*4]
	mov word ptr OldInt9, ax
	mov ax, es:[9*4 + 2]
	mov word ptr OldInt9+2, ax
	mov word ptr es:[9*4], offset MyInt9
	mov word ptr es:[9*4+2], cs
	sti ;Okay, ints back on.
	lea dx,welcome
	call disp_msg
	mov ax,13h			;subfunction 0 select mode 19 (or 13h if prefer)
	int 10h			;call graphics interrupt
	;==========================
	call repaint
game_loop:
	lea di, exit_f
	cmp byte ptr [di],1
	je  exit
	lea bx, timer
	cmp byte ptr[bx], 0	; timer finished?
	je  lbl1			; timer hasn't finished
	lea di, num
	cmp byte ptr[di], 20	; repaint and animate after 20
	jb	lbl2
	mov byte ptr[di], 0	; zero out number of time slices elapsed
	call repaint
	call animate
lbl2:
	inc byte ptr[di]
	call draw_bullet	; draw bullet after .1 sec
	call delay_01sec
lbl1:
	jmp game_loop
exit:
	
	
	
  
delay:		; 1 second
	push ax
	push cx
	push dx
	mov ax, @data
	mov es, ax
	lea bx, timer
	mov cx, 1eh
	mov dx, 8480h
	mov ah, 86h
	int 15h
	pop dx
	pop cx
	pop ax
	ret

delay_01sec:	; .1 second for the bullet
	push ax
	push cx
	push dx
	mov ax, @data
	mov es, ax
	lea bx, timer
	mov byte ptr[bx], 0
	mov cx, 1h
	mov dx, 8000h
	mov ax, 8300h
	int 15h
	pop dx
	pop cx
	pop ax
	ret

animate:
	push si
	push di
	lea si, left
	lea di, dir
	cmp byte ptr [si], 50	; 320 - 250
	jae	l		; move right
	cmp byte ptr [si], 10
	jle r
	jmp move
l:
	mov byte ptr[di], 0
	jmp move
r:
	mov byte ptr[di], 1
	jmp move
move:
	cmp byte ptr[di], 0
	je here
	add byte ptr[si], 20
	jmp get_out
here:
	sub word ptr[si], 20
get_out:
	pop di
	pop si
	ret
	
	
	
	
repaint:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push ds
	mov ax, 13h
	int 10h	;cls
	call draw_enemies
	call draw_hero   
	pop ds
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hits invader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
hits_inv:
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	lea bx, invaders
	lea di, bullet_f
	cmp byte ptr[di], 0
	jne l1dash
	jmp out_of_range
l1dash:
	xor cx, cx
	mov cl, [bx]
	mov ax, 15				; hieght of each invader is 15 , 230w 200h
	mul cl
	mov di, ax				; di = 15 * cl
	mov ax, 320
	lea si, top
	xor dx, dx
	mov dl, byte ptr[si]
	add di, dx
	mul di	; ax = 320 * ([top] + 15 * vertical_offset)
	lea si, left
	add al, byte ptr[si]	; ax = 320 * ([top] + vertical_offset) + [left]
	lea si, invaders
	mov di, bx
	sub di, si
	mov si, ax	; si = 320 * ([top] + vertical_offset) + [left]
	mov ax, 25
	mul di
	add ax, si	; ax = 320 * ([top] + vertical_offset) + [left] + 25 * horizontal_offset
	; ax is pointing to the upper left of the invader
	mov si, 320
	div si
	mov si, ax		; si = row of top left of invader
	mov cx, dx		; cx = column of top left of invader
	lea di, bullet_pos
	mov ax, word ptr[di]
	xor dx, dx		
	mov di, 320
	div di			;dx:ax/arg quotient (row) in ax remainder (column) in dx
	; ax = row of bullet dx = column of bullet
	cmp dx, cx
	jl	out_of_range	; less than invader column so get out
	add cx, 20
	cmp dx, cx
	ja	continue		; see next invader
	add si, 6			; bottom of invader
	cmp ax, si
	ja	out_of_range	; still below
	dec byte ptr[bx]	; hits me
	lea di, bullet_f
	mov byte ptr[di], 0	; destroy bullet
	call repaint
	jmp out_of_range
continue:	
	lea di, invaders
	add di, 10	; last index of invaders
	inc bx
	cmp bx, di
	jl  l1dash
out_of_range:
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	ret

	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; random fire
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
draw_enemies:
	mov ax, 0a000h
	mov es, ax
	lea bx, invaders
l1:
	xor cx, cx
	mov cl, [bx]
l2:
	mov al, 15				; hieght of each invader is 15 , 230w 200h
	mul cl
	mov di, ax				; di = 15 * cl
	mov ax, 320
	lea si, top
	xor dx, dx
	mov dl, byte ptr[si]
	add di, dx
	mul di	; ax = 320 * ([top] + 15 * vertical_offset)
	lea si, left
	add al, byte ptr[si]	; ax = 320 * ([top] + vertical_offset) + [left]
	lea si, invaders
	mov di, bx
	sub di, si
	mov si, ax	; si = 320 * ([top] + vertical_offset) + [left]
	mov ax, 25
	mul di
	add si, ax	; si = 320 * ([top] + vertical_offset) + [left] + 25 * horizontal_offset
	call draw_inv
	loop l2
	lea di, invaders
	add di, 10	; last index of invaders
	inc bx
	cmp bx, di
	jl  l1
	ret
  
draw_inv:	; expects the es and si to be initialized
	push cx
	mov cx, 20
loop1:
	mov byte ptr es:[si], 1
	mov byte ptr es:[si+320], 1
	inc si
	loop loop1
	add si, 640-16
	mov cx, 12
loop2:
	mov byte ptr es:[si], 1
	mov byte ptr es:[si+320], 1
	inc si
	loop loop2
	add si, 640-9
	mov cx, 6
loop3:
	mov byte ptr es:[si], 1
	mov byte ptr es:[si+320], 1
	inc si
	loop loop3
	pop cx
	ret
	
draw_hero:
	mov ax, 0a000h
	mov es, ax
	lea bx, hero_pos
	mov si, [bx]
	push cx
	mov cx, 20
lp1:
	mov byte ptr es:[si], 1
	mov byte ptr es:[si-320], 1
	inc si
	loop lp1
	sub si, 640+16
	mov cx, 12
lp2:
	mov byte ptr es:[si], 1
	mov byte ptr es:[si-320], 1
	inc si
	loop lp2
	sub si, 640+9
	mov cx, 6
lp3:
	mov byte ptr es:[si], 1
	mov byte ptr es:[si-320], 1
	inc si
	loop lp3
	pop cx
	ret  

draw_bullet:
    lea si, bullet_f
    mov al, [si]
    cmp al,0
    je  ret_draw_bullet
	mov ax, 0a000h
	mov es, ax
	lea bx, bullet_pos
	mov si, word ptr [bx]
	mov byte ptr es:[si], 0
	mov byte ptr es:[si+320], 0 	 ; remove the bullet
	sub word ptr [bx],6*320          ; decrement the bullet pos
	mov si,word ptr [bx]
	mov byte ptr es:[si], 1
	mov byte ptr es:[si+320], 1
	call hits_inv
	cmp si, 3200						 ; reached top of the screen; invalidate it
	ja	ret_draw_bullet
	lea si, bullet_f
	mov byte ptr[si], 0
ret_draw_bullet:
    ret	

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interrupt handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
SetCmd proc near
	push cx
	push ax ;Save command value.
	cli ;Critical region, no ints now.
	; Wait until the 8042 is done processing the current command.
	xor cx, cx ;Allow 65,536 times thru loop.
Wait4Empty:
	in al, 64h 		;Read keyboard status register.
	test al, 10b 	;Input buffer full?
	loopnz Wait4Empty ;If so, wait until empty.
	; Okay, send the command to the 8042:
	pop ax ;Retrieve command.
	out 64h, al
	pop cx
	ret
SetCmd endp

MyInt9 proc far
	push ds
	push ax
	push cx
	cli
	mov ax, 40h
	mov ds, ax
	mov al, 0ADh ;Disable keyboard
	call SetCmd
	xor cx, cx
	mov ax, @data
	mov ds, ax	 ; restore ds
Wait4Data:
	in al, 64h ;Read kbd status port.
	test al, 10b ;Data in buffer?
	loopz Wait4Data ;Wait until data available.
	in al, 60h ;Get keyboard data.
	cmp al, BTNLEFT ;Is it the delete key?
	je handle_left
	cmp al, BTNRIGHT
	je handle_right
	cmp al, BTNUP
	je handle_up
	cmp al, BTNDOWN
	je handle_down
	cmp al, BTNSPACE
	je handle_space
	cmp al, ESCAPE
	je handle_esc
	jmp go_out
handle_left:
	lea di, hero_pos
	sub word ptr[di], 20
	call repaint
	jmp go_out
handle_right:
	lea di, hero_pos
	add byte ptr[di], 20
	call repaint
	jmp go_out
handle_up:
	lea di, hero_pos
	sub word ptr[di], 4*320
	call repaint
	jmp go_out
handle_down:
	lea di, hero_pos
	add word ptr[di], 4*320
	call repaint
	jmp go_out
handle_space:
    lea di, bullet_f
    mov byte ptr[di],1h
	lea di, hero_pos      
	lea si, bullet_pos
	mov ax, word ptr[di]                 
	mov word ptr[si], ax
	add word ptr[si], 10
	jmp go_out
handle_esc:
	lea  dx,exit_msg
    call disp_msg
	mov ah,00			;again subfunc 0
	mov al,03			;text mode 3
	int 10h			;call int
	mov ax,4c00h
	int 21h
go_out:
	mov al, 0AEh ;Reenable the keyboard
	call SetCmd
	mov al, 20h ;Send EOI (end of interrupt)
	out 20h, al ; to the 8259A PIC.
	sti
	pop cx
	pop ax
	pop ds
	iret
MyInt9 endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
disp_msg:         ; dx holds the string's address
    push ax
    push bx
    push dx
    mov ax, 13h
	int 10h	;cls
    mov  ah,2
    mov  bh,0
    mov  dh,20             ; row
    mov  dl,20             ; column
    int  10h
    pop  dx
    mov  ah,9
    int  21h 
    call delay
	mov ax, 13h
	int 10h	;cls
    pop  bx
    pop  ax
    ret
	


end start