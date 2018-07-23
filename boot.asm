; %define _BOOT_DEBUG_

%ifdef _BOOT_DEBUG_
    org 0100h
%else
    org 07c00h      ; Bios of IBM PC compatible machine is ruled so
%endif

;==================================================
%ifdef _BOOT_DEBUG_
    BaseOfStack equ 0100h
%else
    BaseOfStack equ 07c00h
%endif

BaseOfLoader            equ     09000h
OffsetOfLoader          equ     0100h   ; LOADER.BIN -> [BaseOfLoader:OffsetOfLoader]
RootDirSectors          equ     14      ; (0xE0 RootDirEnt) * (32 B/DirEnt) / (512 B/Sec) = 14 sectors
SectorNoOfRootDirectory equ     19      ; The sector number of the First sector in RootDir field
;==================================================


    ; FAT12 boot sector
    jmp short LABEL_START
    nop
	BS_OEMName      DB  'Yuhui Wu'
	BPB_BytsPerSec  DW  512
	BPB_SecPerClus  DB  1
	BPB_RsvdSecCnt  DW  1
	BPB_NumFATs     DB  2
	BPB_RootEntCnt  DW  224
	BPB_TotSec16    DW  2880
	BPB_Media       DB  0xF0
	BPB_FATSz16     DW  9
	BPB_SecPerTrk   DW  18
	BPB_NumHeads    DW  2
	BPB_HiddSec     DD  0
	BPB_TotSec32    DD  0
	BS_DrvNum       DB  0
	BS_Reserved1    DB  0
	BS_BootSig      DB  29h
	BS_VolID        DD  0
	BS_VolLab       DB 'OrangeS0.02'
	BS_FileSysType  DB 'FAT12   '

LABEL_START:                        mov ax, cs
                                    mov ds, ax
                                    mov es, ax
                                    mov ss, ax
                                    mov sp, BaseOfStack

                                    ; Reset Disk Drive A (AH=0, DL=0)
                                    xor ah, ah
                                    xor dl, dl
                                    int 13h

                                    mov word [wSectorNo], SectorNoOfRootDirectory
LABEL_SEARCH_IN_ROOT_DIR_BEGIN:     cmp word [wRootDirSizeForLoop], 0
                                    jz  LABEL_NO_LOADERBIN
                                    dec word [wRootDirSizeForLoop]
                                    mov ax, BaseOfLoader
                                    mov es, ax                          ; es <- BaseOfLoader
                                    mov bx, OffsetOfLoader              ; bx <- OffsetOfLoader
                                    mov ax, [wSectorNo]                 ; ax <- the number of the sector we want to read in Root Directory
                                    mov cl, 1
                                    call ReadSector
                                    
                                    
                                    mov	di, OffsetOfLoader              ; es:di -> BaseOfLoader:0100
                                    mov	si, LoaderFileName              ; ds:si -> "LOADER  BIN"
                                    cld
                                    mov	dx, 10h
LABEL_SEARCH_FOR_LOADERBIN:         cmp	dx, 0
                                    jz  LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR
                                    dec dx
                                    mov cx, 11
LABEL_CMP_FILENAME:                 cmp cx, 0
                                    jz  LABEL_FILENAME_FOUND            ; if all 11 chars are the same, then we find LOADER.BIN
                                    dec cx
                                    lodsb                               ; load [ds:si] into al
                                    cmp al, byte [es:di]
                                    jnz LABEL_DIFFERENT                 ; 1 byte diff means it's not LOADER.BIN
                                    inc di
                                    jmp LABEL_CMP_FILENAME
LABEL_DIFFERENT:                    and di, 0FFE0h                  ; let di points to the beginning of cunrrent entry
                                    add di, 20h                     ; di += 20h, points to the beginning of next entry
                                    mov si, LoaderFileName
                                    jmp LABEL_SEARCH_FOR_LOADERBIN
LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR: add word [wSectorNo], 1
                                    jmp LABEL_SEARCH_IN_ROOT_DIR_BEGIN


LABEL_NO_LOADERBIN:                 mov dh, 2
                                    call DispStr    ; DispStr "No LOADER."
                                %ifdef  _BOOT_DEBUG_
                                    mov ax, 4c00h
                                    int 21h         ; Didn't find LOADER.BIN, return to DOS
                                %else
                                    jmp $           ; Didn't find LOADER.BIN, dead loop here
                                %endif

LABEL_FILENAME_FOUND:               jmp $           ; 代码暂时停在这里

;==================================================
; vars used above
wRootDirSizeForLoop dw  RootDirSectors
wSectorNo           dw  0   ; the number of the sectors you want to read
bOdd                db  0   ; odd or even

LoaderFileName		db	"LOADER  BIN", 0 ; LOADER.BIN 's file name

MessageLength   equ 9           ; 3 strings below are all 9 Bytes long
BootMessage     db  "Booting  " ; [0]
Message1        db  "Ready.   " ; [1]
Message2        db  "No LOADER" ; [2]
;==================================================

;----------------------------------------------------------------------------
; Function: DispStr
;----------------------------------------------------------------------------
; Usage:
;   Arg:
;       arg0 in DH
;   Func:
;       Display BootMessage[arg0] on screen
DispStr:
	mov ax, MessageLength
	mul dh
	add ax, BootMessage
	mov bp, ax
	mov ax, ds
	mov es, ax
	mov cx, MessageLength
	mov ax, 01301h
	mov bx, 0007h
	mov dl, 0
	int 10h
	ret


;----------------------------------------------------------------------------
; Function: ReadSector
;----------------------------------------------------------------------------
; Usage:
;   Args:
;       arg0 in AX;
;       arg1 in CL;
;   Bios INT 13h:
;       AH  02h
;       AL  number of sectors to read
;       CH  Cylinder                    (0~79)
;       CL  Sector                      (on cylinder; 1~18)
;       DH  Head                        (0 or 1)
;       DL  Disk Drive                  (0 means "A:/"")
;   Func:
;       Start at NO.<arg0> sector, read continuous #arg1 sectors into [ES:BX]
ReadSector:
	push    bp
	mov bp, sp
	sub esp, 2

	mov byte [bp-2], cl
	push bx
	mov bl, [BPB_SecPerTrk]
	div bl
	inc ah
	mov cl, ah
	mov dh, al
	shr al, 1
	mov ch, al
	and dh, 1
	pop bx
	mov dl, [BS_DrvNum]
.GoOnReading:
	mov ah, 2
	mov al, byte [bp-2]
	int 13h
	jc .GoOnReading     ; if error (CF is set to 1) then
                        ; keep trying until reading successfully
	add esp, 2
	pop bp
	ret

times 510-($-$$) db 0   ; fill the rest space with 0
dw 	0xaa55              ; boot sector signature