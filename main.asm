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

; Button Constants
BUTTON_A        = 1 << 7
BUTTON_B        = 1 << 6
BUTTON_SELECT   = 1 << 5
BUTTON_START    = 1 << 4
BUTTON_UP       = 1 << 3
BUTTON_DOWN     = 1 << 2
BUTTON_LEFT     = 1 << 1
BUTTON_RIGHT    = 1 << 0

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
TmpY: .res 1
TmpZ: .res 1

BlockUpdate: .res 1
BlockDestroy: .res 2
BlockNew: .res 2
controller:     .res 1
controller_Old: .res 1

; Do a brick thing every time this hits zero
FrameCount: .res 1

BlockIdx: .res 1
Blocks: .res 16 ; NT address for each block
BlockDirection: .res 1

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

    lda BlockUpdate
    beq @NoBlockUpdate
    lda #0
    sta BlockUpdate

    ; Calculate the NT address for the X/Y coord
    clc
    ldx BlockNew+1 ; Y
    lda BlockNew+0 ; X
    adc NTLow, x
    tay
    lda #0
    adc NTHigh, x

    bit $2002
    sta $2006
    sty $2006

    ; write block tile
    lda #0
    sta $2007

    ; Remove the oldest block
    clc
    ldx BlockDestroy+1 ; Y
    lda BlockDestroy+0 ; X
    adc NTLow, x
    tay
    lda #0
    adc NTHigh, x
    sta $2006
    sty $2006

    ; find BG tile ID
    lda BlockDestroy+1
    and #7
    tax
    lda Index_BgAnimRows, x
    sta TmpY

    lda BlockDestroy+0
    and #7
    clc
    adc TmpY
    sta $2007

@NoBlockUpdate:
    lda #0
    sta BlockDestroy+0
    sta BlockDestroy+1
    sta BlockNew+0
    sta BlockNew+1

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

    ; Load the block tile
    lda #0
    sta $2006
    sta $2006
    lda #$FF
    ldx #16
:
    sta $2007
    dex
    bne :-

    lda #$10
    sta $2006
    lda #$00
    sta $2006
    lda #$FF
    ldx #16
:
    sta $2007
    dex
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

    lda #BUTTON_DOWN
    sta BlockDirection

; Main frame loop
FrameLoop:
    dec FrameCount
    bne :+

    lda #5
    sta FrameCount
    jsr BlockThing
:

    jsr waves_CacheRow
    jsr WaitForNMI
    jmp FrameLoop

; Animate/move Blocks
BlockThing:
    jsr ReadController
    inc BlockUpdate

    ldx BlockDirection
    lda #BUTTON_LEFT
    and controller
    beq :+
    ldx #1
    jmp @draw
:
    lda #BUTTON_RIGHT
    and controller
    beq :+
    ldx #0
    jmp @draw
:
    lda #BUTTON_UP
    and controller
    beq :+
    ldx #3
    jmp @draw
:
    lda #BUTTON_DOWN
    and controller
    beq :+
    ldx #2
:

@draw:
    stx BlockDirection

    ;; Prep Draw new block
    lda BlockIdx
    asl a
    tax

    ; grab last block's x/y
    lda Blocks+0, x
    sta BlockNew+0
    lda Blocks+1, x
    sta BlockNew+1

    ldy BlockDirection
    bne :+
    ; Right
    inc BlockNew+0
    jmp @mathdone
:
    cpy #1
    bne :+
    ; Left
    dec BlockNew+0
    jmp @mathdone
:
    cpy #3
    bne :+
    ; Up
    dec BlockNew+1
    jmp @mathdone
:
    ; down
    inc BlockNew+1

@mathdone:
    ; Handle X wrap
    lda BlockNew+0
    cmp #32
    bne :+
    lda #0
    sta BlockNew+0
:
    cmp #$FF
    bne :+
    lda #31
    sta BlockNew+0
:

    ; Handle Y wrap
    lda BlockNew+1
    cmp #30
    bne :+
    lda #0
    sta BlockNew+1
:
    cmp #$FF
    bne :+
    lda #29
    sta BlockNew+1
:

    ; remove oldest block
    inc BlockIdx
    lda BlockIdx
    cmp #8
    bne :+
    lda #0
    sta BlockIdx
:
    asl a
    tax
    lda Blocks+0, x
    sta BlockDestroy+0
    lda Blocks+1, x
    sta BlockDestroy+1

    ; save new block's coords in place of the old block
    lda BlockNew+0
    sta Blocks+0, x
    lda BlockNew+1
    sta Blocks+1, x
    rts

;    ; find new block x,y
;    tay
;    clc
;    lda Blocks+0, y
;    adc BlockMoveX, x
;    sta TmpY
;
;    clc
;    lda Blocks+1, y
;    adc BlockMoveY, x
;    sta TmpX
;
;    ; find the oldest (next) block
;    inc BlockIdx
;    lda BlockIdx
;    cmp #8
;    bne :+
;    ; handle wrap around
;    lda #0
;    sta BlockIdx
;:
;
;    ; remove oldest block (next in the queue)
;    asl a
;    tay
;    ldx Blocks+0, y
;    lda NTHigh, x
;    sta BlockDestroy+1
;    lda NTLow, x
;    sta BlockDestroy+0
;
;    clc
;    lda Blocks+1, y
;    adc BlockDestroy+0
;    sta BlockDestroy+0
;    lda #0
;    adc BlockDestroy+1
;    sta BlockDestroy+1
;
;    ; New block
;    ldx TmpY
;    lda NTHigh, x
;    sta BlockNew+1
;    lda NTLow, x
;    sta BlockNew+0
;
;    clc
;    lda TmpX
;    adc 
;
;
;    rts

; Nametable row address starts (Y values)
NTHigh:
.repeat 32, i
    .byte .hibyte($2000 + (i * 32))
.endrepeat

NTLow:
.repeat 32, i
    .byte .lobyte($2000 + (i * 32))
.endrepeat

; These movement 
BlockMoveX:
    ;     R    L  D  U
    .byte 1, $FF, 0, 0

BlockMoveY:
    ;     R  L   D    U
    .byte 0, 0, 32, $E0

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

ReadController:
    lda controller
    sta controller_Old

    ; Freeze input
    lda #1
    sta $4016
    lda #0
    sta $4016

    ldx #$08
:
    lda $4016
    lsr A           ; Bit0 -> Carry
    rol controller ; Bit0 <- Carry
    dex
    bne :-
    rts

.include "bg_anim.asm"

; Tile ID's of the start of each row of tiles in
; the background animation.
Index_BgAnimRows:
    .repeat 8, i
    .byte (i * 8) + $C0
    .endrepeat
