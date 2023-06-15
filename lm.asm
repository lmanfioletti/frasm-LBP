; Lucas Manfioletti - 2020101417
; Sistemas Embarcados 2023/1

segment code
..start:
    		mov 		ax,data
    		mov 		ds,ax
    		mov 		ax,stack
    		mov 		ss,ax
    		mov 		sp,stacktop

; salvar modo corrente de video(vendo como est� o modo de video da maquina)
            mov  		ah,0Fh
    		int  		10h
    		mov  		[modo_anterior],al   

; alterar modo de video para gr�fico 640x480 16 cores
    	mov     	al,12h
   		mov     	ah,0
    	int     	10h

;draw layout base
	mov		byte[cor],branco 
	;global border
		;bottom
			mov		ax,0
			push		ax
			mov		ax,0
			push		ax
			mov		ax,639
			push		ax
			mov		ax,0
			push		ax
			call		line
		;right
			mov		ax,639
			push		ax
			mov		ax,0
			push		ax
			mov		ax,639
			push		ax
			mov		ax,479
			push		ax
			call		line
		;top
			mov		ax,639
			push		ax
			mov		ax,479
			push		ax
			mov		ax,0
			push		ax
			mov		ax,479
			push		ax
			call		line
		;left
			mov		ax,0
			push		ax
			mov		ax,479
			push		ax
			mov		ax,0
			push		ax
			mov		ax,0
			push		ax
			call		line

	;open button
		;border
			;top
				mov		ax,19 ;19px
				push		ax
				mov		ax,464 ;464px
				push		ax
				mov		ax,69
				push		ax
				mov		ax,464
				push		ax
				call		line
			;right
				mov		ax,69
				push		ax
				mov		ax,464
				push		ax
				mov		ax,69
				push		ax
				mov		ax, 414
				push		ax
				call		line
			;bottom
				mov		ax,69
				push		ax
				mov		ax,414
				push		ax
				mov		ax,19
				push		ax
				mov		ax,414
				push		ax
				call		line
			;left
				mov		ax,19
				push		ax
				mov		ax,414
				push		ax
				mov		ax,19
				push		ax
				mov		ax,464
				push		ax
				call		line
		;text
    		mov     	dh,2			;linha 0-29
    		mov     	dl,3			;coluna 0-79
			lea bx, [btn_string_open]
			call 	print_string
	;LBP button
		;border
			;top
				mov		ax,74 
				push		ax
				mov		ax,464
				push		ax
				mov		ax,124
				push		ax
				mov		ax,464
				push		ax
				call		line
			;right
				mov		ax,124
				push		ax
				mov		ax,464
				push		ax
				mov		ax,124
				push		ax
				mov		ax, 414
				push		ax
				call		line
			;bottom
				mov		ax,124
				push		ax
				mov		ax,414
				push		ax
				mov		ax,74
				push		ax
				mov		ax,414
				push		ax
				call		line
			;left
				mov		ax,74
				push		ax
				mov		ax,414
				push		ax
				mov		ax,74
				push		ax
				mov		ax,464
				push		ax
				call		line
		;text
    		mov     	dh,2			;linha 0-29
    		mov     	dl,10			;coluna 0-79
			lea bx, [btn_string_LBP]
			call 	print_string
	
	;Hist button
		;border
			;top
				mov		ax,129 
				push		ax
				mov		ax,464
				push		ax
				mov		ax,179
				push		ax
				mov		ax,464
				push		ax
				call		line
			;right
				mov		ax,179
				push		ax
				mov		ax,464
				push		ax
				mov		ax,179
				push		ax
				mov		ax, 414
				push		ax
				call		line
			;bottom
				mov		ax,179
				push		ax
				mov		ax,414
				push		ax
				mov		ax,129
				push		ax
				mov		ax,414
				push		ax
				call		line
			;left
				mov		ax,129
				push		ax
				mov		ax,414
				push		ax
				mov		ax,129
				push		ax
				mov		ax,464
				push		ax
				call		line
		;text
    		mov     	dh,2			;linha 0-29
    		mov     	dl,17			;coluna 0-79
			lea bx, [btn_string_Hist]
			call 	print_string
	;HistLBP button
		;border
			;top
				mov		ax,184 
				push		ax
				mov		ax,464
				push		ax
				mov		ax,234
				push		ax
				mov		ax,464
				push		ax
				call		line
			;right
				mov		ax,234
				push		ax
				mov		ax,464
				push		ax
				mov		ax,234
				push		ax
				mov		ax, 414
				push		ax
				call		line
			;bottom
				mov		ax,234
				push		ax
				mov		ax,414
				push		ax
				mov		ax,184
				push		ax
				mov		ax,414
				push		ax
				call		line
			;left
				mov		ax,184
				push		ax
				mov		ax,414
				push		ax
				mov		ax,184
				push		ax
				mov		ax,464
				push		ax
				call		line
		;text
    		mov     	dh,1			;linha 0-29
    		mov     	dl,24			;coluna 0-79
			lea bx, [btn_string_HistLBP]
			call 	print_string
			mov     	dh,2			;linha 0-29
    		mov     	dl,24			;coluna 0-79
			inc		bx
			call 	print_string

	;exit button
		;border
			;top
				mov		ax,239 
				push		ax
				mov		ax,464
				push		ax
				mov		ax,289
				push		ax
				mov		ax,464
				push		ax
				call		line
			;right
				mov		ax,289
				push		ax
				mov		ax,464
				push		ax
				mov		ax,289
				push		ax
				mov		ax, 414
				push		ax
				call		line
			;bottom
				mov		ax,289
				push		ax
				mov		ax,414
				push		ax
				mov		ax,239
				push		ax
				mov		ax,414
				push		ax
				call		line
			;left
				mov		ax,239
				push		ax
				mov		ax,414
				push		ax
				mov		ax,239
				push		ax
				mov		ax,464
				push		ax
				call		line
		;text
    		mov     	dh,2			;linha 0-29
    		mov     	dl,31			;coluna 0-79
			lea bx, [btn_string_exit]
			call 	print_string
	;footer
		;border
			;top
				mov		ax,19 
				push		ax
				mov		ax,69
				push		ax
				mov		ax,289
				push		ax
				mov		ax,69
				push		ax
				call		line
			;right
				mov		ax,289
				push		ax
				mov		ax,69
				push		ax
				mov		ax,289
				push		ax
				mov		ax,19
				push		ax
				call		line
		;text
    		mov     	dh,26			;linha 0-29
    		mov     	dl,3			;coluna 0-79
			lea bx, [footer_string]
			call 	print_string
			inc		bx
			mov     	dh,27			;linha 0-29
    		mov     	dl,3			;coluna 0-79
			call 	print_string

		call draw_original_image
		back_main_loop:
		jmp exit_program

ErrorOpening:
	mov dx, OpenError ; exibe um erro
	mov ah,09h      ; usando a função 09h
	int 21h         ; chama serviço do DOS
	mov ax,4C01h        ; termina programa com um errorlevel =1 
	int 21h
	jmp exit_program

exit_program:
	mov    	ah,08h
	int     21h
	mov  	ah,0   			; set video mode
	mov  	al,[modo_anterior]   	; modo anterior
	int  	10h
	mov     ax,4c00h
	int     21h

print_string:	
	call	cursor
	mov     al,[bx]
	cmp 	al, 0       ; verifica se é o caractere nulo
	call	caracter
	inc     bx			;proximo caracter
	inc		dl			;avanca a coluna
	mov     al,[bx]
	cmp 	al, 0       ; verifica se é o caractere nulo
	jne		print_string
	ret

open_file:
	;push all
		pushf
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		push bp
	;function body
		mov 	ah, 3Dh                   
		mov 	al, 0                     
		mov 	dx, file_name             
		int 	21h        
		jc ErrorOpening     ; jmp if flag is on - error!              
		mov		[file_handle], ax
	;pop all
		pop	bp
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx
		pop	ax
		popf
		ret	

draw_original_image:
		call open_file

		mov 	cx, 249           ; line's amount
		line_loop:

			call file_read_line; load line buffer
			mov dx, cx
			add dx, 100; fixing screen position
			push cx
			mov cx, 250; line's pixels lenght
				draw_pixels:
					mov bx, 250
					sub bx, cx
					mov al, byte[buffer_line_1 + bx]
					mov byte[buffer_byte], al
					call convert_vga_scale
					;plot pixel
						mov 	bx, 279
						sub 	bx, cx
						push 	bx
						push 	dx  
						call plot_xy
					loop draw_pixels               ; Repetir até copiar todos os bytes do buffer
			pop cx
			loop line_loop                  ; Continuar lendo do arquivo
		ret
		
ErrorReading:
	mov dx,ReadError ; exibe um erro
	mov ah,09h      ; usando a função 09h
	int 21h         ; chama serviço do DOS
	mov ax,4C02h        ; termina programa com um errorlevel =2
	int 21h
	jmp exit_program

close_file:
    ; Fechar o arquivo
    mov ah, 3Eh                    ; Função 3Eh - Fechar arquivo
    mov bx, [file_handle]                     ; Identificador de arquivo
    int 0x21                       ; Chamar a interrupção 21h
	jmp back_main_loop

read_one_byte:
	;push all
		pushf
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		push bp
	;function body
		mov ah, 3Fh      
		mov bx, [file_handle]                 
		mov cx, 1
		mov dx, buffer_byte
		int 21h                     
		jc ErrorReading     ; jmp if flag is on - error!  
		cmp ax, 0
		je close_file
	;pop all
		pop	bp
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx
		pop	ax
		popf
		ret	



file_read_line:
	;push all
		pushf
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		push bp
	; function body
		mov cx, 250; pixels line amount
		line_loop_buffer:
			mov bx, 0
			mov byte[pixel_size_byte], 0 ;used to know number size
			next_byte:
				call read_one_byte
				mov al, byte[buffer_byte]
				mov byte[buffer_pixel + bx], al
				cmp al, 20h
				je convert_ascii_int
				inc bx
				add byte[pixel_size_byte], 1
				jmp next_byte
			back_convert_ascii_int:
				mov bx, 249
				sub bx, cx
				mov al, byte[buffer_byte]
				mov [buffer_line_1 + bx], al
			loop line_loop_buffer
	;pop all
		pop	bp
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx
		pop	ax
		popf
		ret	

;convert to int
	convert_ascii_int:
		cmp byte[pixel_size_byte], 0
		je close_file
		cmp byte[pixel_size_byte], 1
		je convert_ascii_int_1
		cmp byte[pixel_size_byte], 2
		je convert_ascii_int_2
		cmp byte[pixel_size_byte], 3
		je convert_ascii_int_3
		
	convert_ascii_int_1:
		mov al, byte[buffer_pixel]
		sub al, 30h
		mov byte[buffer_byte], al
		jmp		back_convert_ascii_int

	convert_ascii_int_2:
		mov al, byte[buffer_pixel]
		sub al, 30h
		mov bl, 10
		mul bl
		mov byte[buffer_byte], al
		mov al, byte[buffer_pixel + 1]
		sub al, 30h
		add byte[buffer_byte], al
		jmp		back_convert_ascii_int

	convert_ascii_int_3:
		mov al, byte[buffer_pixel]
		sub al, 30h
		mov bl, 100
		mul bl
		mov byte[buffer_byte], al
		mov al, byte[buffer_pixel + 1]
		sub al, 30h
		mov bl, 10
		mul bl
		add byte[buffer_byte], al
		mov al, byte[buffer_pixel + 2]
		sub al, 30h
		add byte[buffer_byte], al
		jmp		back_convert_ascii_int

consolTest:
	mov dx,consolTestmsg ; exibe um erro
	mov ah,09h      ; usando a função 09h
	int 21h         ; chama serviço do DOS
	mov ax,4C02h        ; termina programa com um errorlevel =2
	int 21h
	jmp exit_program

;convert to vga scale
	convert_vga_scale:
		;push all
			pushf
			push ax
			push bx
			push cx
			push dx
		;function body
			mov ax, 0          ; Zerar o registrador AX
			mov bx, 0          ; Zerar o registrador BX
			mov al, byte[buffer_byte]        ; Dividendo (valor a ser dividido)
			mov bl, 16          ; Divisor
			div bl             ; Divide AX pelo divisor (BL)
			cmp al, 0
			je	scale0
			cmp al, 1
			je	scale1
			cmp al, 2
			je	scale2
			cmp al, 3
			je	scale3
			cmp al, 4
			je	scale4
			cmp al, 5
			je	scale5
			cmp al, 6
			je	scale6
			cmp al, 7
			je	scale7
			cmp al, 8
			je	scale8
			cmp al, 9
			je	scale9
			cmp al, 10
			je	scale10
			cmp al, 11
			je	scale11
			cmp al, 12
			je	scale12
			cmp al, 13
			je	scale13
			cmp al, 14
			je	scale14
			cmp al, 15
			je	scale15
			back_color_change_on_covertion_scales:
		;pop all
			pop	dx
			pop	cx
			pop	bx
			pop	ax
			popf
			ret	

	;convert scales
		scale0:
			mov byte[cor], preto
			jmp back_color_change_on_covertion_scales
		scale1:
			mov byte[cor], cinza
			jmp back_color_change_on_covertion_scales
		scale2:
			mov byte[cor], azul
			jmp back_color_change_on_covertion_scales
		scale3:
			mov byte[cor], azul_claro
			jmp back_color_change_on_covertion_scales
		scale4:
			mov byte[cor], verde
			jmp back_color_change_on_covertion_scales
		scale5:
			mov byte[cor], verde_claro
			jmp back_color_change_on_covertion_scales
		scale6:
			mov byte[cor], cyan
			jmp back_color_change_on_covertion_scales
		scale7:
			mov byte[cor], cyan_claro
			jmp back_color_change_on_covertion_scales
		scale8:
			mov byte[cor], vermelho
			jmp back_color_change_on_covertion_scales
		scale9:
			mov byte[cor], rosa
			jmp back_color_change_on_covertion_scales
		scale10:
			mov byte[cor], magenta
			jmp back_color_change_on_covertion_scales
		scale11:
			mov byte[cor], magenta_claro
			jmp back_color_change_on_covertion_scales
		scale12:
			mov byte[cor], marrom
			jmp back_color_change_on_covertion_scales
		scale13:
			mov byte[cor], amarelo
			jmp back_color_change_on_covertion_scales
		scale14:
			mov byte[cor], branco
			jmp back_color_change_on_covertion_scales
		scale15:
			mov byte[cor], branco_intenso
			jmp back_color_change_on_covertion_scales
;***************************************************************************
;
;   fun��o cursor
;
; dh = linha (0-29) e  dl=coluna  (0-79)
cursor:
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		push		bp
		mov     	ah,2
		mov     	bh,0
		int     	10h
		pop		bp
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		ret
;_____________________________________________________________________________
;
;   fun��o caracter escrito na posi��o do cursor
;
; al= caracter a ser escrito
; cor definida na variavel cor
caracter:
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		push		bp
    		mov     	ah,9
    		mov     	bh,0
    		mov     	cx,1
   		mov     	bl,[cor]
    		int     	10h
		pop		bp
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		ret
;_____________________________________________________________________________
;
;   fun��o plot_xy
;
; push x; push y; call plot_xy;  (x<639, y<479)
; cor definida na variavel cor
plot_xy:
		push		bp
		mov		bp,sp
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
	    mov     	ah,0ch
	    mov     	al,[cor]
	    mov     	bh,0
	    mov     	dx,479
		sub		dx,[bp+4]
	    mov     	cx,[bp+6]
	    int     	10h
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
		ret		4
;_____________________________________________________________________________
;    fun��o circle
;	 push xc; push yc; push r; call circle;  (xc+r<639,yc+r<479)e(xc-r>0,yc-r>0)
; cor definida na variavel cor
circle:
	push 	bp
	mov	 	bp,sp
	pushf                        ;coloca os flags na pilha
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di
	
	mov		ax,[bp+8]    ; resgata xc
	mov		bx,[bp+6]    ; resgata yc
	mov		cx,[bp+4]    ; resgata r
	
	mov 	dx,bx	
	add		dx,cx       ;ponto extremo superior
	push    ax			
	push	dx
	call plot_xy
	
	mov		dx,bx
	sub		dx,cx       ;ponto extremo inferior
	push    ax			
	push	dx
	call plot_xy
	
	mov 	dx,ax	
	add		dx,cx       ;ponto extremo direita
	push    dx			
	push	bx
	call plot_xy
	
	mov		dx,ax
	sub		dx,cx       ;ponto extremo esquerda
	push    dx			
	push	bx
	call plot_xy
		
	mov		di,cx
	sub		di,1	 ;di=r-1
	mov		dx,0  	;dx ser� a vari�vel x. cx � a variavel y
	
;aqui em cima a l�gica foi invertida, 1-r => r-1
;e as compara��es passaram a ser jl => jg, assim garante 
;valores positivos para d

stay:				;loop
	mov		si,di
	cmp		si,0
	jg		inf       ;caso d for menor que 0, seleciona pixel superior (n�o  salta)
	mov		si,dx		;o jl � importante porque trata-se de conta com sinal
	sal		si,1		;multiplica por doi (shift arithmetic left)
	add		si,3
	add		di,si     ;nesse ponto d=d+2*dx+3
	inc		dx		;incrementa dx
	jmp		plotar
inf:	
	mov		si,dx
	sub		si,cx  		;faz x - y (dx-cx), e salva em di 
	sal		si,1
	add		si,5
	add		di,si		;nesse ponto d=d+2*(dx-cx)+5
	inc		dx		;incrementa x (dx)
	dec		cx		;decrementa y (cx)
	
plotar:	
	mov		si,dx
	add		si,ax
	push    si			;coloca a abcisa x+xc na pilha
	mov		si,cx
	add		si,bx
	push    si			;coloca a ordenada y+yc na pilha
	call plot_xy		;toma conta do segundo octante
	mov		si,ax
	add		si,dx
	push    si			;coloca a abcisa xc+x na pilha
	mov		si,bx
	sub		si,cx
	push    si			;coloca a ordenada yc-y na pilha
	call plot_xy		;toma conta do s�timo octante
	mov		si,ax
	add		si,cx
	push    si			;coloca a abcisa xc+y na pilha
	mov		si,bx
	add		si,dx
	push    si			;coloca a ordenada yc+x na pilha
	call plot_xy		;toma conta do segundo octante
	mov		si,ax
	add		si,cx
	push    si			;coloca a abcisa xc+y na pilha
	mov		si,bx
	sub		si,dx
	push    si			;coloca a ordenada yc-x na pilha
	call plot_xy		;toma conta do oitavo octante
	mov		si,ax
	sub		si,dx
	push    si			;coloca a abcisa xc-x na pilha
	mov		si,bx
	add		si,cx
	push    si			;coloca a ordenada yc+y na pilha
	call plot_xy		;toma conta do terceiro octante
	mov		si,ax
	sub		si,dx
	push    si			;coloca a abcisa xc-x na pilha
	mov		si,bx
	sub		si,cx
	push    si			;coloca a ordenada yc-y na pilha
	call plot_xy		;toma conta do sexto octante
	mov		si,ax
	sub		si,cx
	push    si			;coloca a abcisa xc-y na pilha
	mov		si,bx
	sub		si,dx
	push    si			;coloca a ordenada yc-x na pilha
	call plot_xy		;toma conta do quinto octante
	mov		si,ax
	sub		si,cx
	push    si			;coloca a abcisa xc-y na pilha
	mov		si,bx
	add		si,dx
	push    si			;coloca a ordenada yc-x na pilha
	call plot_xy		;toma conta do quarto octante
	
	cmp		cx,dx
	jb		fim_circle  ;se cx (y) est� abaixo de dx (x), termina     
	jmp		stay		;se cx (y) est� acima de dx (x), continua no loop
	
	
fim_circle:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		6
;-----------------------------------------------------------------------------
;    fun��o full_circle
;	 push xc; push yc; push r; call full_circle;  (xc+r<639,yc+r<479)e(xc-r>0,yc-r>0)
; cor definida na variavel cor					  
full_circle:
	push 	bp
	mov	 	bp,sp
	pushf                        ;coloca os flags na pilha
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di

	mov		ax,[bp+8]    ; resgata xc
	mov		bx,[bp+6]    ; resgata yc
	mov		cx,[bp+4]    ; resgata r
	
	mov		si,bx
	sub		si,cx
	push    ax			;coloca xc na pilha			
	push	si			;coloca yc-r na pilha
	mov		si,bx
	add		si,cx
	push	ax		;coloca xc na pilha
	push	si		;coloca yc+r na pilha
	call line
	
		
	mov		di,cx
	sub		di,1	 ;di=r-1
	mov		dx,0  	;dx ser� a vari�vel x. cx � a variavel y
	
;aqui em cima a l�gica foi invertida, 1-r => r-1
;e as compara��es passaram a ser jl => jg, assim garante 
;valores positivos para d

stay_full:				;loop
	mov		si,di
	cmp		si,0
	jg		inf_full       ;caso d for menor que 0, seleciona pixel superior (n�o  salta)
	mov		si,dx		;o jl � importante porque trata-se de conta com sinal
	sal		si,1		;multiplica por doi (shift arithmetic left)
	add		si,3
	add		di,si     ;nesse ponto d=d+2*dx+3
	inc		dx		;incrementa dx
	jmp		plotar_full
inf_full:	
	mov		si,dx
	sub		si,cx  		;faz x - y (dx-cx), e salva em di 
	sal		si,1
	add		si,5
	add		di,si		;nesse ponto d=d+2*(dx-cx)+5
	inc		dx		;incrementa x (dx)
	dec		cx		;decrementa y (cx)
	
plotar_full:	
	mov		si,ax
	add		si,cx
	push	si		;coloca a abcisa y+xc na pilha			
	mov		si,bx
	sub		si,dx
	push    si		;coloca a ordenada yc-x na pilha
	mov		si,ax
	add		si,cx
	push	si		;coloca a abcisa y+xc na pilha	
	mov		si,bx
	add		si,dx
	push    si		;coloca a ordenada yc+x na pilha	
	call 	line
	
	mov		si,ax
	add		si,dx
	push	si		;coloca a abcisa xc+x na pilha			
	mov		si,bx
	sub		si,cx
	push    si		;coloca a ordenada yc-y na pilha
	mov		si,ax
	add		si,dx
	push	si		;coloca a abcisa xc+x na pilha	
	mov		si,bx
	add		si,cx
	push    si		;coloca a ordenada yc+y na pilha	
	call	line
	
	mov		si,ax
	sub		si,dx
	push	si		;coloca a abcisa xc-x na pilha			
	mov		si,bx
	sub		si,cx
	push    si		;coloca a ordenada yc-y na pilha
	mov		si,ax
	sub		si,dx
	push	si		;coloca a abcisa xc-x na pilha	
	mov		si,bx
	add		si,cx
	push    si		;coloca a ordenada yc+y na pilha	
	call	line
	
	mov		si,ax
	sub		si,cx
	push	si		;coloca a abcisa xc-y na pilha			
	mov		si,bx
	sub		si,dx
	push    si		;coloca a ordenada yc-x na pilha
	mov		si,ax
	sub		si,cx
	push	si		;coloca a abcisa xc-y na pilha	
	mov		si,bx
	add		si,dx
	push    si		;coloca a ordenada yc+x na pilha	
	call	line
	
	cmp		cx,dx
	jb		fim_full_circle  ;se cx (y) est� abaixo de dx (x), termina     
	jmp		stay_full		;se cx (y) est� acima de dx (x), continua no loop
	
	
fim_full_circle:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		6
;-----------------------------------------------------------------------------
;
;   fun��o line
;
; push x1; push y1; push x2; push y2; call line;  (x<639, y<479)
line:
		push		bp
		mov		bp,sp
		pushf                        ;coloca os flags na pilha
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		mov		ax,[bp+10]   ; resgata os valores das coordenadas
		mov		bx,[bp+8]    ; resgata os valores das coordenadas
		mov		cx,[bp+6]    ; resgata os valores das coordenadas
		mov		dx,[bp+4]    ; resgata os valores das coordenadas
		cmp		ax,cx
		je		line2
		jb		line1
		xchg		ax,cx
		xchg		bx,dx
		jmp		line1
line2:		; deltax=0
		cmp		bx,dx  ;subtrai dx de bx
		jb		line3
		xchg		bx,dx        ;troca os valores de bx e dx entre eles
line3:	; dx > bx
		push		ax
		push		bx
		call 		plot_xy
		cmp		bx,dx
		jne		line31
		jmp		fim_line
line31:		inc		bx
		jmp		line3
;deltax <>0
line1:
; comparar m�dulos de deltax e deltay sabendo que cx>ax
	; cx > ax
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		ja		line32
		neg		dx
line32:		
		mov		[deltay],dx
		pop		dx

		push		ax
		mov		ax,[deltax]
		cmp		ax,[deltay]
		pop		ax
		jb		line5

	; cx > ax e deltax>deltay
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx

		mov		si,ax
line4:
		push		ax
		push		dx
		push		si
		sub		si,ax	;(x-x1)
		mov		ax,[deltay]
		imul		si
		mov		si,[deltax]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar1
		add		ax,si
		adc		dx,0
		jmp		arc1
ar1:		sub		ax,si
		sbb		dx,0
arc1:
		idiv		word [deltax]
		add		ax,bx
		pop		si
		push		si
		push		ax
		call		plot_xy
		pop		dx
		pop		ax
		cmp		si,cx
		je		fim_line
		inc		si
		jmp		line4

line5:		cmp		bx,dx
		jb 		line7
		xchg		ax,cx
		xchg		bx,dx
line7:
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx



		mov		si,bx
line6:
		push		dx
		push		si
		push		ax
		sub		si,bx	;(y-y1)
		mov		ax,[deltax]
		imul		si
		mov		si,[deltay]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar2
		add		ax,si
		adc		dx,0
		jmp		arc2
ar2:		sub		ax,si
		sbb		dx,0
arc2:
		idiv		word [deltay]
		mov		di,ax
		pop		ax
		add		di,ax
		pop		si
		push		di
		push		si
		call		plot_xy
		pop		dx
		cmp		si,dx
		je		fim_line
		inc		si
		jmp		line6

fim_line:
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
		ret		8
;*******************************************************************
segment data

cor		db		branco_intenso

;	I R G B COR
;	0 0 0 0 preto
;	0 0 0 1 azul
;	0 0 1 0 verde
;	0 0 1 1 cyan
;	0 1 0 0 vermelho
;	0 1 0 1 magenta
;	0 1 1 0 marrom
;	0 1 1 1 branco
;	1 0 0 0 cinza
;	1 0 0 1 azul claro
;	1 0 1 0 verde claro
;	1 0 1 1 cyan claro
;	1 1 0 0 rosa
;	1 1 0 1 magenta claro
;	1 1 1 0 amarelo
;	1 1 1 1 branco intenso

preto		equ		0
azul		equ		1
verde		equ		2
cyan		equ		3
vermelho	equ		4
magenta		equ		5
marrom		equ		6
branco		equ		7
cinza		equ		8
azul_claro	equ		9
verde_claro	equ		10
cyan_claro	equ		11
rosa		equ		12
magenta_claro	equ		13
amarelo		equ		14
branco_intenso	equ		15

modo_anterior	db		0
linha   	dw  		0
coluna  	dw  		0
deltax		dw		0
deltay		dw		0

btn_string_open     db      'Abrir',0 
btn_string_LBP     db      'LBP',0
btn_string_Hist     db      'Hist',0
btn_string_HistLBP     db      'Hist',0,'LBP',0
btn_string_exit     db      'Sair',0
footer_string       db      'Lucas Manfioletti turma 6.1',0, 'Sistemas Embarcados 2023/1',0

buffer_byte db 0
buffer_pixel times  4  db 0
buffer_line_1 times 250 db 0
pixel_size_byte	db	0
file_name		db		"imagem.txt", 0
file_handle		dw	0
OpenError DB "Ocorreu um erro(abrindo)!$"
ReadError DB "Ocorreu um erro(lendo)!$"
consolTestmsg DB "Follow from here!$"
;*************************************************************************
segment stack stack
    		resb 		512
stacktop: