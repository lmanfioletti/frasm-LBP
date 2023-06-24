; Lucas Manfioletti - 2020101417
; Sistemas Embarcados 2023/1

segment code
..start:
    		mov 		ax,data
    		mov 		ds,ax
    		mov 		ax,stack
    		mov 		ss,ax
    		mov 		sp,stacktop

; save correct video mode (seeing how the machine's video mode is
            mov  		ah,0Fh
    		int  		10h
    		mov  		[modo_anterior],al   

; change video mode to graphic 640x480 16 colors
    	mov     	al,12h
   		mov     	ah,0
    	int     	10h

		call draw_layout_base_default
		jmp start_mouse
;_____________________________________________________________________________
;
;   Mouse click functions
;
;-->
	; this function start mouse
	start_mouse:
		mov ax,0
		int 33h
		mov ax,1
		int 33h 
		jmp click_check

	;_____________________________________________________________________________
	; this function check if mouse click is on range from buttons on UI and call choose_click if this is true
	click_check:
		mov ax,5              
		mov bx,0
		int 33h               
		cmp bx,0              
		je click_check ; if don't have click on screen -> bx = 0
		cmp dx, 14
		jb click_check ; if y < 14 is false
		cmp dx, 64
		jnb click_check ; if y > 64 is false
		call choose_click
		jmp click_check

	;_____________________________________________________________________________
	; this function finds out which button was clicked on click check and call respectve button select function	
	choose_click:
		;check open button
			cmp cx, 19
			jb click_check
			cmp cx, 69
			jnb check_LBP_button
			call open_button_select
			ret
		;check LBP button
		check_LBP_button:
			cmp cx, 74
			jb click_check
			cmp cx, 124
			jnb check_Hist_button
			call LBP_button_select
			ret
		ret
		;check Hist button
		check_Hist_button:
			cmp cx, 129
			jb click_check
			cmp cx, 179
			jnb check_HistLBP_button
			call Hist_button_select
			ret
		ret
		;check HistLBP button
		check_HistLBP_button:
			cmp cx, 184
			jb click_check
			cmp cx, 234
			jnb check_exit_button
			call HistLBP_button_select
			ret
		ret
		;check exit button
		check_exit_button:
			cmp cx, 239
			jb click_check
			cmp cx, 289
			jnb click_check
			call exit_button_select
			ret
		ret
;_____________________________________________________________________________
;
;   Buttons to call to your functions
;
;-->
	; this function call function to draw original image and change button color to yellow
	open_button_select:
		call reset_layout_base_buttons 
		mov		byte[cor],amarelo
		call draw_layout_base_default_open_button
		call draw_original_image 
		ret
	;_____________________________________________________________________________
	; this function call function to draw lbp image and change button color to yellow
	LBP_button_select:
		call reset_layout_base_buttons 
		mov		byte[cor],amarelo
		call draw_layout_base_default_LBP_button
		call draw_lbp_image 
		ret
	;_____________________________________________________________________________
	; this function call function to draw Histogram from original image and change button color to yellow
	Hist_button_select:
		call reset_layout_base_buttons 
		mov		byte[cor],amarelo
		call draw_layout_base_default_Hist_button
		call draw_layout_base_default_Hist
		mov bx, bufferHistImg
		call draw_image_hist 
		ret
	;_____________________________________________________________________________
	; this function call function to open Histogram from LBP image and change button color to yellow
	HistLBP_button_select:
		call reset_layout_base_buttons 
		mov		byte[cor],amarelo
		call draw_layout_base_default_HistLBP_button
		call draw_layout_base_default_Hist
		mov bx, bufferHistImgLBP
		call draw_image_hist  
		ret
	;_____________________________________________________________________________
	; this function call function to exit program image and change button color to yellow
	exit_button_select:
		call reset_layout_base_buttons 
		mov		byte[cor],amarelo
		call draw_layout_base_default_exit_button 
		jmp exit_program
		ret

;_____________________________________________________________________________
;
;   UI layout functions
;
;-->
	; this function call another buttons base with default color to reset UI buttons
	reset_layout_base_buttons:
		mov		byte[cor],branco_intenso 
		call draw_layout_base_default_open_button
		call draw_layout_base_default_LBP_button
		call draw_layout_base_default_Hist_button
		call draw_layout_base_default_HistLBP_button
		call draw_layout_base_default_exit_button
		ret
	;_____________________________________________________________________________
	; this function call another base function with default color to reset UI
	draw_layout_base_default:
		mov		byte[cor],branco_intenso 
		call draw_layout_base_default_global_border
		call draw_layout_base_default_open_button
		call draw_layout_base_default_LBP_button
		call draw_layout_base_default_Hist_button
		call draw_layout_base_default_HistLBP_button
		call draw_layout_base_default_exit_button
		call draw_layout_base_default_footer
		ret
	;_____________________________________________________________________________
	; this function draw global UI border
	draw_layout_base_default_global_border:
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
		ret
	;_____________________________________________________________________________
	; this function draw open button
	draw_layout_base_default_open_button:
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
		ret
	;_____________________________________________________________________________
	; this function draw LBP button
	draw_layout_base_default_LBP_button:
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
		ret
	;_____________________________________________________________________________
	; this function draw Hist button
	draw_layout_base_default_Hist_button:
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
		ret
	;_____________________________________________________________________________
	; this function draw Hist LBP button
	draw_layout_base_default_HistLBP_button:
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
		ret
	;_____________________________________________________________________________
	; this function draw exit button
	draw_layout_base_default_exit_button:
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
		ret
	;_____________________________________________________________________________
	; this function draw footer
	draw_layout_base_default_footer:
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
		ret
	;_____________________________________________________________________________
	; this function erase hist slot to black
	draw_layout_base_default_Hist:
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
			mov cx, 16
			mov dx, 346
			mov byte[cor], preto
			hist_loop_image_default:
				;plot pixel
					push cx
					mov cx, 16
					repeat_lines_to_large_hist_default:
						push bx
						mov		ax, dx
						push		ax
						mov		ax, 19
						push		ax
						mov		ax, dx
						push		ax

						mov		ah, 0
						mov al, 160
						add ax, 19
						push		ax
						call		line
						add dx, 1
						pop bx  
						loop repeat_lines_to_large_hist_default
					pop cx
				loop hist_loop_image_default
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
;
;   function to exit program
;
;-->
	exit_program:
	mov    	ah,08h
	int     21h
	mov  	ah,0   			; set video mode
	mov  	al,[modo_anterior]   	; before mode
	int  	10h
	mov     ax,4c00h
	int     21h

;_____________________________________________________________________________
;
;   function to print screen on UI
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
;   function to draw original image on UI
;
; this function open the file and read line by line with file_read_line
; and plot pixel by pixel on screen after convertion to VGA scale convert_vga_scale
;-->
	draw_original_image:
		call open_file
		mov 	cx, 250           ; line's amount
		line_loop:

			call file_read_line; load line buffer
			mov dx, cx
			add dx, 100; fixing screen position
			push cx
			mov cx, 250; line's pixels lenght
				draw_pixels:
					mov bx, 250
					sub bx, cx
					mov al, byte[buffer_line + bx]
					mov byte[buffer_byte], al
					mov byte[isHistLBP], 0
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
;_____________________________________________________________________________
;
;   function to draw lbp image on UI
;
; this function open the file and take 3 buffers for each line.
; with buffers, create_lbp_number is called and a pixel from the new value
; from the pixel is ploted
;-->
	draw_lbp_image:
		call open_file
		call file_read_line ; load line buffer
		mov dx, buffer_line ; origin
		mov bx, buffer_line_1 ; destiny
		call copy_buffer
		call file_read_line ; load line buffer
		mov dx, buffer_line ; origin
		mov bx, buffer_line_2 ; destiny
		call copy_buffer
		call file_read_line ; load line buffer
		mov dx, buffer_line ; origin
		mov bx, buffer_line_3 ; destiny
		call copy_buffer
		mov 	cx, 247           ; line's amount
		
		line_loop_lbp:
			mov dx, cx
			add dx, 200; fixing screen position
			push cx

			mov cx, 250; line's pixels lenght
				draw_pixels_lbp:
					mov bx, 250
					sub bx, cx
					;access each position and create a new value
					call create_lbp_number
					mov byte[isHistLBP], 1
					call convert_vga_scale
					;plot pixel
						mov 	bx, 596
						sub 	bx, cx
						push 	bx
						push 	dx  
						call plot_xy
					loop draw_pixels_lbp               ; Repetir até copiar todos os bytes do buffer			
			pop cx
			mov dx, buffer_line_2 ; origin
			mov bx, buffer_line_1 ; destiny
			call copy_buffer
			mov dx, buffer_line_3 ; origin
			mov bx, buffer_line_2 ; destiny
			call copy_buffer
			call file_read_line
			mov dx, buffer_line ; origin
			mov bx, buffer_line_3 ; destiny
			call copy_buffer
			loop line_loop_lbp                  ; Continuar lendo do arquivo
		ret
;_____________________________________________________________________________
;
;   function to create lbp number
;
; take the clustered pixels and make LBP algorithm. The new 
; number is put back on [buffer_byte]   
;-->
	create_lbp_number:
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
		mov al, 00000000b
		mov cl, byte[buffer_line_2 + bx + 1] ; bit central
		mov dl, byte[buffer_line_1 + bx] ; 1st bit
		cmp dl, cl
		jb verify_2nd_bit
		xor al, 10000000b
		verify_2nd_bit:
		mov dl, byte[buffer_line_1 + bx + 1] ; 2nd bit
		cmp dl, cl
		jb verify_3rd_bit
		xor al, 01000000b
		verify_3rd_bit:
		mov dl, byte[buffer_line_1 + bx + 2] ; 3rd bit
		cmp dl, cl
		jb verify_4th_bit
		xor al, 00100000b
		verify_4th_bit:
		mov dl, byte[buffer_line_2 + bx + 2] ; 4th bit
		cmp dl, cl
		jb verify_5th_bit
		xor al, 00010000b
		verify_5th_bit:
		mov dl, byte[buffer_line_3 + bx + 2] ; 5th bit
		cmp dl, cl
		jb verify_6th_bit
		xor al, 00001000b
		verify_6th_bit:
		mov dl, byte[buffer_line_3 + bx + 1] ; 6th bit
		cmp dl, cl
		jb verify_7th_bit
		xor al, 00000100b
		verify_7th_bit:
		mov dl, byte[buffer_line_3 + bx] ; 7th bit
		cmp dl, cl
		jb verify_8th_bit
		xor al, 00000010b
		verify_8th_bit:
		mov dl, byte[buffer_line_2 + bx] ; 8nd bit
		cmp dl, cl
		jb convert_bin_to_int
		xor al, 00000001b
		convert_bin_to_int:
		sub al, 30h
		mov byte[buffer_byte], al	
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
;
;   function to copy a buffer to another
;
; the user give the origin on dx, and destiny on bx. 
; All content from dx is copyed to bx
;-->
	copy_buffer:
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
		mov cx, 250
		mov di, 0
		map_vector:
			push bx
			mov bx, dx
			mov al, byte[bx + di]
			pop bx
			mov byte[bx + di], al
			add di, 1
			loop map_vector
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
;
;   function draw histogram graphic from image or LBP image 
;
; take address buffer from that user give on bx
; (can be image histogram or image LBP histogram)
; and follow all positions color and display this information 
; like a graphic
;-->
	draw_image_hist:
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
		mov cx, 16
		mov dx, 346
		mov si, 0
		hist_loop_image:
			mov al, byte[fixed_scale_vector + si] 
			mov byte[cor], al
			;plot pixel
				push cx
				mov cx, 16
				repeat_lines_to_large_hist:
					push bx
					mov		ax, dx
					push		ax
					mov		ax, 19
					push		ax
					mov		ax, dx
					push		ax

					mov		ah, 0
					mov al, byte[bx]
					mov bh, 0
					mov bl, 16
					div bl
					mov		ah, 0
					
					add ax, 19
					push		ax
					call		line
					add dx, 1
					pop bx  
					loop repeat_lines_to_large_hist
				pop cx
				add bx, 2
				inc si
			loop hist_loop_image
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
;
;   functions to read file and check error
;
;--> 
	ErrorReading:
		mov dx,ReadError ; show a error
		mov ah,09h      
		int 21h         
		mov ax,4C02h        
		int 21h
		jmp exit_program
	;_____________________________________________________________________________
	; this function just read 1 byte from file and put this byte into buffer_byte
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
	;_____________________________________________________________________________
	; this function call the function read_one_byte for all line, and
	; convert pixel by pixel with convert_ascii_int, after convertion, 
	; the pixels are storaged in a buffer from line ([buffer_line])
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
					mov [buffer_line + bx], al
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
;_____________________________________________________________________________
;
;   functions to open, close file and check error on open
;
;-->
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
	close_file:
		mov ah, 3Eh                    ; 3Eh - close file
		mov bx, [file_handle]                     
		int 0x21                       
		jmp click_check
	ErrorOpening:
		mov dx, OpenError ; show a error
		mov ah,09h      
		int 21h        
		mov ax,4C01h       
		int 21h
		jmp exit_program

;_____________________________________________________________________________
;
;   function convert pixel number to byte number
;
; take buffer from pixel read([buffer_pixel]), and convert this to 
; 1 byte number
;-->
	;check size number([pixel_size_byte]) and convert byte by byte to 1 byte buffer([buffer_byte])
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

;_____________________________________________________________________________
;
;   function convert to vga scale
;
; take buffer from pixel converted to byte, 
; and convert this to VGA scale diving by 16
;-->
	;inc hist
		inc_hist_img:
			add byte[bufferHistImg + bx], 1
			jmp back_inc_hist_img

		inc_hist_img_LBP:
			add byte[bufferHistImgLBP + bx], 1
			jmp back_inc_hist_img	
	convert_vga_scale:
		;push all
			pushf
			push ax
			push cx
			push dx
			push bx
		;function body
			mov ax, 0          
			mov bx, 0          
			mov al, byte[buffer_byte]
			mov bl, 16          
			div bl            
			push ax
			mov bx, 0
			mov bl, 2
			mul bl
			mov bx, 0
			mov bl, al
			cmp byte[isHistLBP], 0
			je inc_hist_img
			jmp inc_hist_img_LBP
			back_inc_hist_img:
			pop ax
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
			pop bx
			pop	dx
			pop	cx
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
;_____________________________________________________________________________
;
;
;***************************************************************************
;FUNCTIONS FROM LINEC.asm
;***************************************************************************
;
;   fun��o cursor
;
; dh = linha (0-29) e  dl=coluna  (0-79)
;-->
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
;-->
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
;-->
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

;-----------------------------------------------------------------------------
;
;   fun��o line
;
; push x1; push y1; push x2; push y2; call line;  (x<639, y<479)
;--> 
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

fixed_scale_vector db 0, 8, 1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7,15

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
buffer_line times 250 db 0
buffer_line_1 times 250 db 0
buffer_line_2 times 250 db 0
buffer_line_3 times 250 db 0
pixel_size_byte	db	0
file_name		db		"imagem.txt", 0
file_handle		dw	0
OpenError DB "Ocorreu um erro(abrindo)!$"
ReadError DB "Ocorreu um erro(lendo)!$"

isHistLBP db 0
bufferHistImg times 16 dw 0000
bufferHistImgLBP times 16 dw 0000
;*************************************************************************
segment stack stack
    		resb 		512
stacktop: