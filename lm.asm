; Lucas Manfioletti - 2020101417
; Sistemas Embarcados 2023/1 - turme 6.1

segment code
..start:
    		mov 		ax,data
    		mov 		ds,ax
    		mov 		ax,stack
    		mov 		ss,ax
    		mov 		sp,stacktop

; load interruption table
    cli
        xor     ax, ax
        mov     es, ax
        mov     ax, [es:int9*4]
        mov     [offset_dos], ax       
        mov     ax, [es:int9*4+2]   
        mov     [cs_dos], ax	
        mov     [es:int9*4+2], cs
        mov     word[es:int9*4], keyint
    sti


; save correct video mode (seeing how the machine's video mode is
            mov  		ah,0Fh
    		int  		10h
    		mov  		[modo_anterior],al   

; change video mode to graphic 640x480 16 colors
    	mov     	al,12h
   		mov     	ah,0
    	int     	10h


call draw_layout_base_default
   main_loop:
       call check_keyboad_input
       call state_update
       ;call speed_delay
       jmp main_loop
;_____________________________________________________________________________
;
;   UI layout functions
;
;-->
    ; this function call another base function with default color to reset UI
    draw_layout_base_default:
        call draw_layout_base_default_global_border
        call draw_header_layout
        mov byte[cor], vermelho
        call draw_ball
        mov byte[cor], branco_intenso
        call draw_stick
        ret
    ;_____________________________________________________________________________
	; this function draw global UI border
    draw_layout_base_default_global_border:
        mov		byte[cor],branco_intenso 
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
			mov		ax,478
			push		ax
			mov		ax,0
			push		ax
			mov		ax,478
			push		ax
			call		line
		;left
			mov		ax,0
			push		ax
			mov		ax,478
			push		ax
			mov		ax,0
			push		ax
			mov		ax,0
			push		ax
			call		line
		;header divider
			mov		ax,639
			push		ax
			mov		ax,421
			push		ax
			mov		ax,0
			push		ax
			mov		ax,421
			push		ax
			call		line
        ret
    ;_____________________________________________________________________________
	; this function draw all strings on UI
    draw_header_layout:
        mov		byte[cor],branco_intenso 
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
            ;first line
                mov     	dh,1			;line 0-29
                mov     	dl,9			;column 0-79
                mov bx, string_header
                call 	print_string
            ;second line
                mov     	dh,2			;line 0-29
                mov     	dl,9			;column 0-79
                mov bx, string_student_name
                call 	print_string
                call draw_and_update_scores
                mov     	dh,2			;line 0-29
                mov     	dl,31			;column 0-79
                mov bx, string_x
                call 	print_string
                mov     	dh,2			;line 0-29
                mov     	dl,37			;column 0-79
                mov bx, string_computer_name
                call 	print_string
                mov     	dh,2			;line 0-29
                mov     	dl,51			;column 0-79
                mov bx, string_speed
                call 	print_string
                call draw_and_update_speed
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
    ;_____________________________________________________________________________
   	; this function draw and update scores
    draw_and_update_scores:
        mov byte[cor], branco_intenso

        ; Convert the number to a string
        mov al, [ball_crash_stick_counter]  ; Move the value to AL register
        xor ah, ah                         ; Clear AH register
        mov bl, 10                         ; Divisor for extracting tens digit
        div bl                             ; Divide AL by BL, quotient in AL, remainder in AH

        add ax, '00'                       ; Convert the digits to their ASCII representations
        mov [string_student_score], ax    ; Store the converted digits in the string

        mov     	dh,2			;line 0-29
        mov     	dl,28			;column 0-79
        mov bx, string_student_score
        call 	print_string

        ; Convert the number to a string
        mov al, [ball_crash_right_counter]  ; Move the value to AL register
        xor ah, ah                         ; Clear AH register
        mov bl, 10                         ; Divisor for extracting tens digit
        div bl                             ; Divide AL by BL, quotient in AL, remainder in AH

        add ax, '00'                       ; Convert the digits to their ASCII representations
        mov [string_computer_score], ax    ; Store the converted digits in the string

        update_computer_score:
        mov     	dh,2			;line 0-29
        mov     	dl,33			;column 0-79
        mov bx, string_computer_score
        call 	print_string
        ret
    ;_____________________________________________________________________________
   	; this function draw and update the speed
    draw_and_update_speed:
        mov     	dh,2			;line 0-29
        mov     	dl,69			;column 0-79
        mov bx, string_current_speed
        call 	print_string
        ret
    ;_____________________________________________________________________________
   	; this function draw and update the ball, basead on color defined by user
	draw_ball:
        mov ax, [ball_x_position]
        push ax
        mov ax, [ball_y_position]
        push ax
        mov ax, 8
        push ax
        call full_circle
        ret
    ;_____________________________________________________________________________
   	; this function draw and update stick, basead on color defined by user
    draw_stick:
		mov		ax, [stick_x_position]
		push		ax
		mov		ax, [stick_y_position]
		push		ax
		mov		ax, [stick_x_position]
		push		ax
		mov		ax, [stick_y_position]
        sub     ax, 50
		push		ax
		call		line
        ret


;_____________________________________________________________________________
;
;   Ball position update
;
;-->
    update_ball:
        ; erase old ball
        mov byte[cor], preto
        call draw_ball
        ; ball_y_movement:
            xor     ax, ax
            cmp     byte[is_ball_crash_top], 1
            je      ball_down_movement
        ball_up_movement:
            add     word[ball_y_position], 1
            jmp     ball_x_movement
        ball_down_movement:
            sub     word[ball_y_position], 1
        ball_x_movement:
            cmp     byte[is_ball_crash_stick], 1
            je     ball_left_movement
        ball_right_movement:
            cmp     byte[is_ball_crash_right], 1
            je      restart_ball
            add     word[ball_x_position], 1
            jmp     draw_updated_ball
        ball_left_movement:
            sub     word[ball_x_position], 1
            jmp     draw_updated_ball
        restart_ball:
            mov     word[ball_x_position], 80
            mov byte[is_ball_crash_right], 0
            mov byte[is_ball_crash_top], 0
            mov byte[is_ball_crash_stick], 0

        draw_updated_ball:
            mov byte[cor], vermelho
            call draw_ball
        ret

;_____________________________________________________________________________
;
;   Stick position update
;
;-->
    update_stick:
        ; erase old ball
        mov byte[cor], preto
        call draw_stick

        cmp byte[isUpPressed], 1
        jne down_key_pressed
        mov byte[isUpPressed], 0
        xor ax, ax
        mov ax, [jump_lenght]
        add ax, [stick_y_position]
        cmp ax, [limit_top_border]
        jnb draw_updated_stick
        mov [stick_y_position], ax
        jmp draw_updated_stick
        down_key_pressed:
        mov byte[isDownPressed], 0
        xor ax, ax
        mov ax, [stick_y_position]
        sub ax, [stick_lenght]
        sub ax, [jump_lenght]
        cmp ax, [limit_bottom_border]
        jl draw_updated_stick
        add ax, [stick_lenght]
        mov [stick_y_position], ax
        draw_updated_stick:
            mov byte[cor], branco_intenso
            call draw_stick
        ret


;_____________________________________________________________________________
;
;   State check and call update
;
;-->
    state_update:
        ;stick state (the states are controled on keyboard input check bellow)
            check_stick_change:
            cmp byte[isUpPressed], 1
            je call_update_stick
            cmp byte[isDownPressed], 1
            jne skip_update_stick
            call_update_stick:
            call update_stick
            skip_update_stick:
        ;ball state
            ;check x crash:
            check_right_crash:
            cmp word[ball_x_position], 630 ;639px(max width) - 1px(border) - ball radio
            jb  check_stick_crash
            mov byte[is_ball_crash_right], 1
            add byte[ball_crash_right_counter], 1
            call draw_and_update_scores
            cmp byte[ball_crash_right_counter], 99
            jne  check_top_crash
            call exit_program
            ;call machine_win
            check_stick_crash:
            cmp word[ball_x_position], 590 ;639px(max width) - 1px(border) - stick position - ball radio
            jne check_left_crash
            mov ax, [ball_y_position]
            sub ax, 8 ;geting ball y bottom position
            cmp ax, [stick_y_position]
            jnb check_left_crash
            mov ax, [ball_y_position]
            add ax, 8 ;geting ball y bottom postion
            mov bx, [stick_y_position]
            sub bx, 50 ; getting stick y botton postion
            cmp ax, bx
            jb check_left_crash
            mov byte[is_ball_crash_stick], 1
            add byte[ball_crash_stick_counter], 1 
            call draw_and_update_scores
            ; check_student_win:
            cmp byte[ball_crash_stick_counter], 99
            jne  check_top_crash
            call exit_program
            ;call student_win
            jmp check_top_crash
            check_left_crash:
            cmp word[ball_x_position], 9 ;ball radio + 1px(border)
            jnb check_top_crash
            mov byte[is_ball_crash_stick], 0
            ;check_y_crash:
            check_top_crash:
            cmp word[ball_y_position], 412
            jb  check_bottom_crash
            mov byte[is_ball_crash_top], 1
            jmp call_update_ball
            check_bottom_crash:
            cmp word[ball_y_position], 10
            jnb call_update_ball
            mov byte[is_ball_crash_top], 0
            call_update_ball:
            call update_ball
        ret

consolTest:
	mov dx,consolTestmsg ; exibe um erro
	mov ah,09h      ; usando a função 09h
	int 21h         ; chama serviço do DOS
	mov ax,4C02h        ; termina programa com um errorlevel =2
	int 21h
	jmp exit_program
    
    ;add speed controller


;_____________________________________________________________________________
;
;   Check keyboard input and handle it
;
;-->
    check_keyboad_input:
        mov     ax,[p_i]
		cmp     ax,[p_t]
		jne     check_which_input
        ret
        check_which_input:
		inc     word[p_t]
		and     word[p_t],7
		mov     bx,[p_t]

		xor     AX, AX
		mov     AL, [bx+tecla]

        ;check esc key
        cmp al, 1h
        jne check_up_key
		call exit_program

        check_up_key:
        cmp al, 48h
		jne check_down_key
        mov byte[isUpPressed], 1
        check_down_key:
        cmp al, 50h
		jne check_plus_key
        mov byte[isDownPressed], 1
        check_plus_key:
        ret	

;_____________________________________________________________________________
;   Function to print string on UI
;
; 	the user give string to pirnt on bx, and collum on dl and line on dh
;-->
	print_string:	
		call	cursor
		mov     al,[bx]
		cmp 	al, 0       ; verify if the caracter is null
		call	caracter
		inc     bx			;next caracter
		inc		dl			;next columm
		mov     al,[bx]
		cmp 	al, 0       ; verify if the caracter is null
		jne		print_string
		ret
;_____________________________________________________________________________
;
;   function to exit from program
;
;-->
	exit_program:
        ;removing keyint and reset original ISR 
        cli
        xor     ax, ax
        mov     es, ax
        mov     ax, [cs_dos]
        mov     [es:int9*4+2], ax
        mov     ax, [offset_dos]
        mov     [es:int9*4], ax 
        mov     AH, 4Ch
        int     21h

        mov    	ah,08h
        int     21h
        mov  	ah,0   			; set video mode
        mov  	al,[modo_anterior]   	; before mode
        int  	10h
        mov     ax,4c00h
        int     21h

;_____________________________________________________________________________
;
;
;***************************************************************************
;FUNCTIONS FROM linec.asm
;***************************************************************************
;-->
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
;FUNCTION FROM tecbuf.asm
;***************************************************************************
;-->
   	keyint:
		push    ax
		push    bx
		push    ds
		mov     ax,data
		mov     ds,ax
		in      al, kb_data
		inc     WORD [p_i]
		and     WORD [p_i],7
		mov     bx,[p_i]
		mov     [bx+tecla],al
		in      al, kb_ctl
		or      al, 80h
		out     kb_ctl, al
		and     al, 7Fh
		out     kb_ctl, al
		mov     al, eoi
		out     pictrl, al
		pop     ds
		pop     bx
		pop     ax
		iret 
;***************************************************************************
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

;Stick
stick_lenght     dw      50
stick_x_position     dw  599
stick_y_position     dw  220 ;this represents top pixel from stick ex:(sticker bettewen 290 and 240)
isUpPressed     db      0
isDownPressed       db      0
jump_lenght     dw      33
limit_top_border        dw      419
limit_bottom_border        dw      1

;Ball
ball_x_position     dw  80
ball_y_position     dw  20 ;this represents the main point in ball
is_ball_crash_top   db  1
is_ball_crash_right   db  0
ball_crash_right_counter     db 0
is_ball_crash_stick   db  0
ball_crash_stick_counter   db  0

;Strings
string_header    	db  		'Exercicio de Programacao de Sistemas Embarcados 1 - 2023/1', 0
string_student_name     db      'Lucas Manfioletti', 0
string_student_score        db      '00', 0 
string_x        db      'x', 0
string_computer_score       db      '00', 0
string_computer_name        db      'Computador', 0
string_speed        db      'Velocidade atual: ', 0
string_current_speed        db      '1', 0

;Keyint tecbuf.asm variables
    kb_data equ 60h  ;PORTA DE LEITURA DE TECLADO
    kb_ctl  equ 61h  ;PORTA DE RESET PARA PEDIR NOVA INTERRUPCAO
    pictrl  equ 20h	 ;ENDEREÇO ONDE ESTÁ LOCALIZADO O PIC8259
    eoi     equ 20h  ;PARA OCW2: SINALIZA TÉRMINO DE INTERRUPÇÃO NORMAL
    int9    equ 9h   ;NÚMERO DA INTERRUPÇÃO DE TECLADO NO PC
    cs_dos  dw  1
    offset_dos  dw 1
    tecla_u db 0
    tecla   resb  8 
    p_i     dw  0   ;ponteiro p/ interrupcao (qnd uma tecla é pressionada)  
    p_t     dw  0   ;ponterio p/ interrupcao (qnd uma tecla é liberada)    
    teclasc db  0,0,13,10,'$'
    


consolTestmsg DB "Follow from here!$"

;*************************************************************************
segment stack stack
    		resb 		512
stacktop: