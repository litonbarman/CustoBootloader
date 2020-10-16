; Master Boot Record (Single stage Bootloader) 
; Written by Liton Barman  (16/10/2019)
; You are free to use and modify this code as long as you give credit to its author
; Caution: Use at your own risk
; I am not responsible for the use or misuse of this code and if anyway hardware damaged
; can be compiled by NASM Assembler as
; nasm -f bin -o Bootloader.bin Bootloader.asm



[BITS 16]
[ORG 0x7c00]

  mov [BOOT_DRIVE], dl                  ; bios store boot drive in dl
  
  mov   bp,  08000h                     ; stack
  mov   sp,  bp
    
  
  push 0x8000                           ; kernel loading address
  pop  es


 ; loading disk
  mov  al,  2                           ; number of sector we are reading 2 sector means 1kb
  mov  cl,  2
  mov  bx,  0x00                        ; es:bx position to load
  mov  dl,  [BOOT_DRIVE]
  call disk_read
      

checkA20Line:
    push es
 
    xor  ax, ax                         ; ax = 0
    mov  es, ax
	mov  byte[es:0500],  0x00
	
	not  ax
	mov  es, ax
	mov  byte[es:0510],  0xFF
	
	xor  ax, ax
	mov  es, ax 
	cmp  byte[es:0500],  0xff
    je  _checkA20Line_exit
    
	cmp  byte[A20CON], 0
	je  _byDefault
	
	cmp  byte[A20CON], 1
    je  _byBios	
	
	cmp  byte[A20CON], 2
	je  _byKeyboard
	
	cmp  byte[A20CON], 3
	je  _byFastgate
	
   _byFastgate:
    mov  si,  A20FASTGATE
	call print
	jmp _A20End
	
   _byKeyboard:
    mov  si,  A20BYKEYBOARD
	call print
	jmp _A20End
   
   _byBios:
    mov  si,  A20BYBIOS
	call print
	jmp _A20End
	
   _byDefault:
	mov  si,  A20ALREADY
	call print	
	jmp _A20End
	
   _checkA20Line_exit:   
	inc  byte[A20CON]
	
	cmp  byte[A20CON], 1
	je   EnableA20ByBIOS

    cmp  byte[A20CON], 2
	je   EnableA20ByKEYBOARD
	
	cmp  byte[A20CON], 3
	je   EnableA20FastGate
	
	cmp  byte[A20CON], 4
	je  _A20Fail
	
	jmp  checkA20Line
 
   _A20Fail:
    mov  si,  A20FAIL
	call print
   _A20End:
	
	pop  es                              ; end of A20 routine
  
 

    mov  si,  PM
	call print
 
    mov   ah,  0
    int   0x16                           ; wait for keypress
 
    cmp   al,  0x0d                      ; ENTER for shutdown else switch_to_pm
    je    shutdown

setPixelMode:
    xor ax,  ax                          ; necessary because remain some junk in al 
    mov ax,  0x13                        ;  320x200 pixel mode
    int 0x10
	jmp  switch_to_pm
    
   
shutdown:
  mov ax, 0x1000
  mov ax, ss
  mov sp, 0xf000
  mov ax, 0x5307
  mov bx, 0x0001
  mov cx, 0x0003
  int 0x15
 
  ret                                    ; if interrupt doesnt work
	
    jmp  $

 
; A20 functions__________________________________________________________________

EnableA20ByBIOS:
    mov  ax,   0x2501
	int  0x15
	ret
	
	
EnableA20FastGate:    ; our last option
    in   al,   0x92
    or   al,   2
    out  0x92, al
    ret
	
EnableA20ByKEYBOARD:
    cli
	call    Wait_8042_command
    mov     al,0xAD
    out     0x64,al

    call    Wait_8042_command
    mov     al,0xD0
    out     0x64,al

    call    Wait_8042_data
    in      al,0x60
    push    eax
	
    call    Wait_8042_command
    mov     al,0xD1
    out     0x64,al            

    call    Wait_8042_command
    pop     eax
    or      al,2
    out     0x60,al

    call    Wait_8042_command
    mov     al,0xAE
    out     0x64,al

    call    Wait_8042_command

    sti 
    ret	

Wait_8042_command:
    in      al,0x64
    test    al,2
    jnz     Wait_8042_command
    ret
  
Wait_8042_data:
    in      al,0x64
    test    al,1
    jz      Wait_8042_data
    ret                                   

; A20 end___________________________________________________________


print:                                    ; argument in si
   pusha 
   mov   ah,  0x0e
   
  _repeat:
   lodsb
   cmp   al,  0
   je   _done
   int   010h
   jmp  _repeat
  _done:
   
   popa
   ret
   
   
clrscr:
   mov   ax,  0x0600              ; Fn 06 of int 10h,scroll window up,if al = 0 clrscr
   mov   cx,  0x0000              ; Clear window from 0,0
   mov   dx,  0x174f              ; to 23,79
   mov   bh,  0                   ; fill with colour 0
   int   0x10                     ; call bios interrupt 10h
   ret
   
setCursorPos:
   mov  ah,   0x02                ; this is the function number
   mov  bh,   0                   ; page default to zero
   mov  dh,   0                   ; row
   mov  dl,   0                   ; column
   int  0x10
   ret



  ; al = number of sectors to read
  ; cl = sector number to read form 
  ; bx = load in loaction  | in es:bx
  ; dl = drive number | 0x80 is the default

disk_read:

	; load disk sector into memory

	mov ah, 0x02                    ; load second stage to memory / function number for read
	mov ch, 0                       ; cylinder number
	mov dh, 0                       ; head number
	int 0x13                        ; disk I/O interrupt
    
	ret
   
 
 
 switch_to_pm:
   cli
   lgdt  [gdt_descriptor]
   
   mov   eax,  cr0
   or    eax,  0x1
   mov   cr0,  eax
   jmp   CODE_SEG:init_pm
   
   
  [BITS 32]
 
 init_pm:
   mov  ax,   DATA_SEG       ; data segment
   mov  ds,   ax
   mov  ss,   ax
   mov  es,   ax
   mov  fs,   ax
   mov  gs,   ax
   
   mov  ebp,  0x9000        ; new stack
   mov  esp,  ebp
   
   call 0x8000              ; calling a first 32 bit function 


 
 BOOT_DRIVE    db 0
 
 A20CON        db 0
 A20FAIL       db "A20 fail", 0
 A20ALREADY    db "A20 dflt", 0
 A20BYBIOS     db "A20 BIOS", 0
 A20BYKEYBOARD db "A20 KEYB", 0
 A20FASTGATE   db "A20 FastGate"
 PM            db 0ah,"Enter for 32 bin kernel, any key to shutdown", 0
 
; GDT 


gdt_start:
  gdt_null:                           ; the mandatory null descriptor 
   dd 0x0                             ; ’dd’ means define double word (i.e. 4 bytes) 
   dd 0x0
  gdt_code:                           ; the code segment descriptor 
                                      ; base=0x0, limit=0xfffff , 
                                      ; 1st flags: (present)1 (privilege)00 (descriptor type)1 -> 1001b 
                                      ; type flags: (code)1 (conforming)0 (readable)1 (accessed)0 -> 1010b 
                                      ; 2nd flags: (granularity)1 (32-bit default)1 (64-bit seg)0 (AVL)0 -> 1100b 
   dw 0xffff                          ; Limit (bits 0-15) 
   dw 0x0                             ; Base (bits 0-15) 
   db 0x0                             ; Base (bits 16-23) 
   db 10011010b                       ; 1st flags , type flags 
   db 11001111b                       ; 2nd flags , Limit (bits 16-19) 
   db 0x0                             ; Base (bits 24-31)
  gdt_data:                           ;the data segment descriptor 
                                      ; Same as code segment except for the tye flags: 
                                      ; type flags: (code)0 (expand down)0 (writable)1 (accessed)0 -> 0010b 
   dw 0xffff                          ; Limit (bits 0-15) 
   dw 0x0                             ; Base (bits 0-15) 
   db 0x0                             ; Base (bits 16-23) 
   db 10010010b                       ; 1st flags , type flags 
   db 11001111b                       ; 2nd flags , Limit (bits 16-19) 
   db 0x0                             ; Base (bits 24-31)
  gdt_end:                            ; The reason for putting a label at the end of the 
                                      ; GDT is so we can have the assembler calculate 
				                      ; the size of the GDT for the GDT decriptor (below)
                                      ; GDT descriptior 
  gdt_descriptor: 
   dw gdt_end - gdt_start - 1         ; Size of our GDT, always less one 
                                      ; of the true size 
   dd gdt_start 
							          ; Start address of our GDT
  ; Define some handy constants for the GDT segment descriptor offsets , which 
 
   CODE_SEG equ gdt_code - gdt_start 
   DATA_SEG equ gdt_data - gdt_start 
 
times 510-($-$$) db 0
dw 0xaa55

load_here:      ; load here is the location where kernel was initially store as a raw binary image the 
                ; after that it was loaded in 0x8000  (main memory address)

 ; %include "kernel.asm"