; asmsyntax=ca65

.include "nes2header.inc"
nes2mapper 1
nes2prg 16 * 16 * 1024  ; 256k PRG
nes2chr 0
nes2chrram 1 * 8 * 1024 ; 8k CHR RAM
;nes2wram 1 * 8 * 1024
nes2mirror 'V'
nes2tv 'N'
nes2end

.feature leading_dot_in_identifiers
.feature underline_in_numbers

PPU_MASK_EMPH_RED = %0010_0000
PPU_MASK_EMPH_GREEN = %0100_0000
PPU_MASK_EMPH_BLUE = %1000_0000
PPU_MASK_SHOWSPRITES = %0001_0000
PPU_MASK_SHOWBACKGROUND = %0000_1000
PPU_MASK_LEFTSPRITES = %0000_0100
PPU_MASK_LEFTBACKGROUND = %0000_0010
PPU_MASK_GREYSCALE = %0000_0001

PPU_MASK_ON = PPU_MASK_SHOWBACKGROUND ;| PPU_MASK_LEFTSPRITES | PPU_MASK_LEFTBACKGROUND

.macro .Update_PpuMask args
    lda #args
    sta $2001
.endmacro

PPU_CTRL_NMI = %1000_0000
PPU_CTRL_MASTER = %0100_0000
PPU_CTRL_8x16 = %0010_0000
PPU_CTRL_BG_PATTERN = %0001_0000
PPU_CTRL_SP_PATTERN = %0000_1000
PPU_CTRL_VERTICAL = %0000_0100
PPU_CTRL_NAMETABLE = %0000_0011

;PPU_CTRL_VAL_VERT = PPU_CTRL_NMI | PPU_CTRL_VERTICAL

.macro .Update_PpuControl args
    lda #args
    sta $2000
.endmacro

.macro .Update_PpuControl_Var args
    lda args
    sta $2000
.endmacro

.segment "VECTORS"
    .word NMI
    .word RESET
    .word IRQ

.segment "ZEROPAGE"
Sleeping: .res 1
PpuControl: .res 1
AddressPointer0: .res 2
IdxA: .res 1
IdxB: .res 1
IdxC: .res 1

TmpX: .res 1
TmpZ: .res 1

; Every page starts with its page number in the
; first byte. This is so we can return to
; previously loaded pages easier.
.repeat 15, i
.segment .sprintf("PAGE%02d", i)
    .byte i
.endrepeat

.segment "PAGE07"

WaveChrData:
    .incbin "waves.chr"

.segment "PAGE_FIXED"
    .byte 15

BgPalette:
    ; some shades of blue
    .byte $01, $11, $21, $31

IRQ:
    rti

NMI:
    pha
    txa
    pha
    tya
    pha

    dec z:waves_AnimWait
    bne @noAnim
    lda #8
    sta z:waves_AnimWait

    lda z:waves_AnimOdd
    beq :+
    lda #0
    sta z:waves_AnimOdd
    lda #%10001000
    sta PpuControl
    jmp @animDone
:
    lda #1
    sta z:waves_AnimOdd
    lda #%10010000
    sta PpuControl

@animDone:
    inc z:waves_currentFrame
    lda z:waves_currentFrame
    cmp #15
    bcc :+
    lda #0
    sta z:waves_currentFrame
:
@noAnim:
    lda z:waves_AnimOdd
    jsr waves_WriteCachedRow

    bit $2002
    lda #0
    sta $2005
    sta $2005

    .Update_PpuMask PPU_MASK_SHOWBACKGROUND | PPU_MASK_LEFTBACKGROUND

    lda PpuControl
    sta $2000

    dec Sleeping

    pla
    tay
    pla
    tax
    pla
    rti

RESET:
    sei         ; Disable IRQs
    cld         ; Disable decimal mode

    ldx #$40
    stx $4017   ; Disable APU frame IRQ

    ldx #$FF
    txs         ; Setup new stack

    inx         ; Now X = 0

    stx $2000   ; disable NMI
    stx $2001   ; disable rendering
    stx $4010   ; disable DMC IRQs

:   ; First wait for VBlank to make sure PPU is ready.
    bit $2002   ; test this bit with ACC
    bpl :- ; Branch on result plus

:   ; Clear RAM
    lda #$00
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x

    inx
    bne :-  ; loop if != 0

:   ; Second wait for vblank.  PPU is ready after this
    bit $2002
    bpl :-

    ; Clear attribute table
    ldx #64
    bit $2000
    lda #$23
    sta $2006
    lda #$C0
    sta $2006
    lda #0
:
    sta $2007
    dex
    bne :-

    ; set the bg pal
    lda #$3F
    sta $2006
    lda #$00
    sta $2006
    ldx #0
:
    lda BgPalette, x
    sta $2007
    inx
    cpx #4
    bne :-

    lda #$88
    sta PpuControl
    sta $2000

    jsr MMC1_Init

    lda #$00
    jsr Waves_LoadFrame
    jsr Wave_DrawBackground

    lda #8
    sta z:waves_AnimWait
    lda #0
    sta z:waves_AnimOdd

; Main frame loop
:   jsr waves_CacheRow
    jsr WaitForNMI
    jmp :-

WaitForNMI:
:   bit Sleeping
    bpl :-
    lda #0
    sta Sleeping
    rts

MMC1_Init:
    ; Set flag
    jsr MMC1_Select_Vert

    ; select CHR 0
    lda #%00000000

    sta $A000
    lsr a
    sta $A000
    lsr a
    sta $A000
    lsr a
    sta $A000
    lsr a
    sta $A000
    rts

MMC1_Select_Vert:
    ; control stuff
    ; vertical mirroring, switchable $8000, fixed $C000, chr 8k
    ; %0000 1110
    lda #%00001110
    jmp MMC1_Setup

MMC1_Select_Horiz:
    lda #%00001111

MMC1_Setup:
    sta $8000
    lsr a
    sta $8000
    lsr a
    sta $8000
    lsr a
    sta $8000
    lsr a
    sta $8000
    rts

MMC1_Select_Page:
    sta $E000
    lsr a
    sta $E000
    lsr a
    sta $E000
    lsr a
    sta $E000
    lsr a
    sta $E000
    rts

.include "bg_anim.asm"
