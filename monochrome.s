; SPREADPOINT
; MONOCHROME (AMIGA, PAL, >= OCS, >= 68000, >= 512 KB)
; (C) 2022 DEPECHE

; Build with vasm
; vasmm68k_mot -kick1hunks -Fhunkexe -o monochrome -nosym monochrome.s

AbsExecBase     equ 4
OpenLibrary     equ -552
CloseLibrary    equ -414
Write           equ -48
Output          equ -60
AvailMem        equ -216
AllocMem        equ -198
FreeMem         equ -210
TypeOfMem       equ -534
WaitTOF         equ -270
Forbid          equ -132
Permit          equ -138
LoadView        equ -222

custom          equ $dff000

pwidth          equ 40
lwidth          equ 2*40
pheight         equ 256
psize           equ pheight*pwidth
tsize           equ 16*pwidth
numtbuffers     equ 8

goldeny         equ 58

logowestsize    equ pheight*lwidth

; bitplanes
;
;              north (bitplane1base)
;            +------------+
;            | canvas 256 |
;            | psize      |
;            | north      |     south (bitplane2base)
;            +------------+   +------------+
;            | canvas 256 |   | canvas 256 |
;            | psize      |   | psize      |
;            | hidden     |   | hidden     |
;            +------------+   +------------+
;                             | canvas 256 |
;                             | psize      |
;                             | south      |
;                             +------------+
;
; logowest(base)
;            +------------+------------+
;            | height 256 | height 256 |
;            |            |            |
;            | pwidth     | pwidth     |
;            +------------+------------+
;            |        lpwidth          |
;

; debug modes
testing         equ 0
soliderase      equ 0
numbers         equ 0
instruments     equ 0   ; in combination with "numbers" only
testpattern     equ 0

; DMACON
SET             equ 1<<15           ; 0=clear, 1=set bits that are set to 1 below
BLTPRI          equ 1<<10           ; Blitter DMA priority (over CPU) "blitter nasty"
DMAEN           equ 1<<9            ; Enable all DMA below
BPLEN           equ 1<<8            ; Bit plane DMA
COPEN           equ 1<<7            ; Copper DMA
BLTEN           equ 1<<6            ; Blitter DMA
SPREN           equ 1<<5            ; Sprite DMA

*------ MAIN -------------------------------------------------------------*

base
    movem.l a0-a6/d0-d7,-(a7)       ;
    bsr     alloc                   ;
    bne     .exit                   ; out of memory error?

    if testing
    move.l  AbsExecBase.w,a6        ;
    move.l  #MEMF_CHIP,d1           ;
    jsr     AvailMem(a6)            ;
    move.l  d0,$210.w               ; Free (available) memory
    endif

    move.l  AbsExecBase.w,a6        ;
    moveq   #0,d0                   ;
    lea     gfx(pc),a1              ;
    jsr     OpenLibrary(a6)         ; open gfx library
    tst.l   d0                      ;
    beq     .exit                   ; couldn't open gfx library!
    move.l  d0,a6                   ;
    move.l  34(a6),-(a7)            ; view
    move.l  d0,-(a7)                ; gfx base
    move.l  38(a6),-(a7)            ; copper list
    sub.l   a1,a1                   ;
    jsr     LoadView(a6)            ;
    jsr     WaitTOF(a6)             ;
    jsr     WaitTOF(a6)             ;
    move.l  AbsExecBase.w,a6        ;
    jsr     Forbid(a6)              ;
    
    lea     vars(pc),a5             ;
    lea     custom,a6               ;
    bsr     waitblitter             ;

    move.w  $02(a6),-(a7)           ; store DMA control
    move.w  $1c(a6),-(a7)           ; store interrupt enable bits
    move.l  $6c.w,-(a7)             ; store irq3
    move.w  #$7fff,d0               ;
    move.w  d0,$9a(a6)              ;
    move.w  d0,$9c(a6)              ;
    move.w  d0,$96(a6)              ; disable all DMAs

    clr.w   -(a7)                   ; store LED state
    btst    #1,$bfe001              ;
    beq     .ledst                  ;
    not.w   (a7)                    ;
.ledst
    bset    #1,$bfe001              ; LED dark

*------ INIT ------------------------------------------------------------------*

    move.l  bitplane1base(pc),a0    ; init bitplane double buffering
    add.w   #10*pwidth,a0           ; extra padding (see fullscreen scroll)
    move.l  a0,vbitplane1(a5)       ;
    move.l  bitplane2base(pc),a0    ;
    add.w   #10*pwidth,a0           ; extra padding (see fullscreen scroll)
    move.l  a0,vbitplane2(a5)       ;

    move.w  #tsize,vtextplaneoffset(a5) ; first text buffer (blank) is shown
    move.w  #1,vusedtbuffers(a5)    ;

    lea     playcmds(pc),a0         ;
    move.l  a0,vcmdspointer(a5)     ;

    lea     linedata(pc),a0         ;
    move.l  a0,vlinedatapointer(a5) ;

    st      vdraw(a5)               ;
    move.w  #4,vstepper(a5)         ;
    
    if instruments                  ;
    move.w  #$ffff,vinstlow(a5)     ;
    clr.w   vinsthigh(a5)           ;
    endif                           ;
    
    btst    #10,$16(a6)             ; right mouse button pressed?
    bne     .nopressed              ;
    bsr     hiddenmessage           ; this is for Lord :-)
.nopressed
    bsr     lspinit                 ;

    lea     irq3(pc),a0             ;
    move.l  a0,$6c.w                ; vertical blanking interrupt

    bsr     waitraster              ; no flickering (?)

    move.w  #$0030,$9c(a6)          ; kill vertb and coper interrupt request
    move.l  clistbase(pc),$80(a6)   ;
    move.w  #SET+DMAEN+BLTPRI+BLTEN+COPEN+SPREN,$96(a6) ;
    move.w  #$c030,$9a(a6)          ; enable vertb and coper interrupt

*------ IDLE LOOP -------------------------------------------------------------*

.idleloop
    tst.b   vinitlogo(a5)           ;
    beq     .noinitlogo             ;
    bsr     initlogo                ;
    st      vshowlogo(a5)           ;
    clr.b   vinitlogo(a5)           ;
    move.l  clist2base(pc),$80(a6)  ;

.noinitlogo
    bsr     handletextbuffers       ;
    tst.w   vquit(a5)               ;
    beq     .idleloop               ;
    
*------ RESTORE STATE AND EXIT ------------------------------------------------*

    bsr     waitblitter             ;
    
    tst.w   (a7)+                   ; restore state
    bne     .leddark                ;
    bclr    #1,$bfe001              ; LED bright
.leddark
    move.w  #$7fff,d0               ;
    move.w  d0,$9a(a6)              ;
    move.w  d0,$9c(a6)              ;
    move.w  d0,$96(a6)              ;
    
    moveq   #0,d0                   ; volume to zero
    move.w  d0,$a8(a6)              ;
    move.w  d0,$b8(a6)              ;
    move.w  d0,$c8(a6)              ;
    move.w  d0,$d8(a6)              ;
    
    move.l  (a7)+,$6c.w             ;
    move.w  (a7)+,d0                ;
    or.w    #$c000,d0               ;
    move.w  d0,$9a(a6)              ;
    move.w  (a7)+,d0                ;
    or.w    #$8000,d0               ;
    move.w  d0,$96(a6)              ;

    move.l  (a7)+,$80(a6)           ; copper list
    move.l  (a7)+,a6                ; gfx base
    move.l  (a7)+,a1                ; view
    jsr     LoadView(a6)            ;
    jsr     WaitTOF(a6)             ;
    jsr     WaitTOF(a6)             ;
    move.l  a6,a1                   ; parameter for CloseLibrary
    move.l  AbsExecBase.w,a6        ;
    jsr     CloseLibrary(a6)        ; close gfx library
    jsr     Permit(a6)              ;

    bsr     dealloc                 ;
.exit
    movem.l (a7)+,a0-a6/d0-d7       ;
    moveq   #0,d0                   ;
    rts                             ;

gfx dc.b    "graphics.library",0
    even

    rsreset
vbitplane1          rs.l    1
vbitplane2          rs.l    1
vquit               rs.w    1
vframe              rs.w    1
vlinedatapointer    rs.l    1
vstepper            rs.w    1
vtextpointer        rs.w    1
veasep              rs.l    1

; text
vgive               rs.w    1
vshow               rs.w    1
vwaiting            rs.w    1
vtextplaneoffset    rs.w    1
vshowtextploffset   rs.w    1
vusedtbuffers       rs.w    1

; player
vcmdspointer        rs.l    1
    if instruments
vinstlow            rs.w    1
vinsthigh           rs.w    1
vinst0              rs.w    1
vinst1              rs.w    1
vinst2              rs.w    1
vinst3              rs.w    1
    endif
vwait               rs.b    1
vscroll             rs.b    1
vdraw               rs.b    1
vfullscreen         rs.b    1

vinitlogo           rs.b    1
vshowlogo           rs.b    1
vautotext           rs.b    1
vdynamic            rs.b    1
vtriggertext        rs.b    1

sizeofvars          rs.w    0
vars    ds.b    sizeofvars,0

    even
peaksff76
    dc.w    7<<8, wd-base, 0*2  ; $ff76 (-138)
    dc.w    7<<8, wd-base, 1*2  ; $ff7c (-132)
    dc.w    7<<8, wd-base, 2*2  ; $ff82 (-126)
    dc.w    7<<8, wd-base, 3*2  ; $ff88 (-120)
    dc.w    7<<8, wd-base, 4*2  ; $ff8e (-114)
    dc.w    7<<8, wd-base, 4*2  ; $ff94 (-108)
    dc.w    10<<8, wf-base, 0*2 ; $ff9a (-102)
    dc.w    7<<8, wd-base, 5*2  ; $ffa0 (-96)
    dc.w    7<<8, wd-base, 6*2  ; $ffa6 (-90)
    dc.w    7<<8, wd-base, 6*2  ; $ffac (-84)
peakstest
    dc.w    15<<8, wg-base, 0*2 ; $ffb2 (-78)
    dc.w    7<<8, wd-base, 8*2  ; $ffb8 (-72)
    dc.w    7<<8, wd-base, -3*2 ; $ffbe (-66)  analog low to high (left most)
    dc.w    7<<8, wd-base, 7*2  ; $ffc4 (-60)
    dc.w    7<<8, wd-base, 7*2  ; $ffca (-54)
    dc.w    7<<8, wd-base, 7*2  ; $ffd0 (-48)
    dc.w    7<<8, wd-base, 8*2  ; $ffd6 (-42)
    dc.w    7<<8, wd-base, 8*2  ; $ffdc (-36)
    dc.w    7<<8, wd-base, 9*2  ; $ffe2 (-30)
    dc.w    7<<8, wd-base, 9*2  ; $ffe8 (-24)
    dc.w    7<<8, wd-base, 10*2 ; $ffee (-18)
    dc.w    7<<8, wd-base, 11*2 ; $fff4 (-12)
    dc.w    7<<8, wd-base, 12*2 ; $fffa (-6)
peaks0000
    dc.w    7<<8, wd-base, 13*2 ; $0000 (0)
    dc.w    7<<8, wd-base, 8*2  ; $0006 (6)
    dc.w    7<<8, wd-base, 15*2 ; $000c (12)
    dc.w    7<<8, wd-base, 16*2 ; $0012 (18)
    dc.w    7<<8, wd-base, 17*2 ; $0018 (24)
    dc.w    7<<8, wd-base, (60-3-15)*2  ; $001e (30) snare (right most)
    dc.w    (79-3)<<8, wh-base, 22*2    ; $0024 (36) analog high to low
    dc.w    7<<8, wd-base, 18*2 ; $002a (42)
    dc.w    6<<8, wb-base, 10*2 ; $0030 (48)
    dc.w    7<<8, wd-base, 21*2 ; $0036 (54)
    dc.w    7<<8, wd-base, 22*2 ; $003c (60)
    dc.w    7<<8, wd-base, 23*2 ; $0042 (66)
    dc.w    7<<8, wd-base, 24*2 ; $0048 (72)
    dc.w    7<<8, wd-base, 25*2 ; $004e (78)
    dc.w    4<<8, we-base, 26*2 ; $0054 (84)
    dc.w    7<<8, wd-base, 27*2 ; $005a (90)
    dc.w    7<<8, wd-base, 28*2 ; $0060 (96)
    dc.w    7<<8, wd-base, 29*2 ; $0066 (102)
    dc.w    7<<8, wd-base, 30*2 ; $006c (108) Drum
    dc.w    7<<8, wd-base, 31*2 ; $0072 (114)
    dc.w    7<<8, wd-base, (64-3-15)*2  ; $0078 (120) Woooo
    dc.w    7<<8, wd-base, 32*2 ; $007e (126)
    dc.w    7<<8, wd-base, 23*2 ; $0084 (132)
    dc.w    7<<8, wd-base, 34*2 ; $008a (138)
    dc.w    7<<8, wd-base, 34*2 ; $0090 (144)
    dc.w    7<<8, wd-base, 35*2 ; $0096 (150)
    dc.w    7<<8, wd-base, 36*2 ; $009c (156)
    dc.w    7<<8, wd-base, 37*2 ; $00a2 (162)
    dc.w    7<<8, wd-base, 38*2 ; $00a8 (168)
    dc.w    7<<8, wd-base, 39*2 ; $00ae (174)
    dc.w    7<<8, wd-base, 40*2 ; $00b4 (180)
    dc.w    7<<8, wd-base, 41*2 ; $00ba (186)
    dc.w    7<<8, wd-base, 42*2 ; $00c0 (192)

*------ HANDLE TEXT BUFFERS ---------------------------------------------------*
    
handletextbuffers
    cmp.w   #numtbuffers,vusedtbuffers(a5)
    beq     .nofreebuffer           ;

    lea     text(pc),a0             ;
    add.w   vtextpointer(a5),a0     ;
    lea     textend(pc),a1          ;
    cmp.l   a0,a1                   ;
    bne     .gottext                ;
    clr.w   vtextpointer(a5)        ;
    lea     text(pc),a0             ;
.gottext
    bsr     textwidth               ;
    move.l  textplanebase(pc),a1    ;
    add.w   vtextplaneoffset(a5),a1 ;
    
    move.w  #tsize/4-1,d7           ;
    move.l  a1,a2                   ;
.clear
    clr.l   (a2)+                   ;
    dbf     d7,.clear               ;
    
    add.l   d0,a1                   ;
    bsr     printtext               ;
    
    addq.w  #1,vusedtbuffers(a5)    ;
    add.w   #tsize,vtextplaneoffset(a5) ; ring buffer
    cmp.w   #numtbuffers*tsize,vtextplaneoffset(a5)
    bne     .done                   ;
    clr.w   vtextplaneoffset(a5)    ;
.done
.nofreebuffer
    tst.w   vgive(a5)               ;
    beq     .nogive                 ;
    cmp.w   #2,vusedtbuffers(a5)    ;
    bcs     .nobufferready          ;
    clr.w   vgive(a5)               ; no skipping
    st      vshow(a5)               ; vertb will consume this
    clr.w   vwaiting(a5)            ;

.nobufferready
.noshow
.nogive
    rts                             ;

*------ PRINT NUMBER ----------------------------------------------------------*

; d0: number, d1: pos
    if numbers
printnumber
    move.l  vbitplane2(a5),a0       ;
    add.w   d1,a0                   ;

    moveq   #4-1,d7                 ; 1 word = 4 digits
.loop
    move.w  d0,d1                   ; number
    and.w   #$000f,d1               ; mask digit out
    asl.w   #3,d1                   ; offset to font data
    lea     digits(pc,d1.w),a1      ;
    move.b  (a1)+,(a0)              ; print digit
    move.b  (a1)+,pwidth(a0)        ;
    move.b  (a1)+,2*pwidth(a0)      ;
    move.b  (a1)+,3*pwidth(a0)      ;
    move.b  (a1)+,4*pwidth(a0)      ;
    asr.w   #4,d0                   ; next digit
    subq.w  #1,a0                   ; next x position
    dbf     d7,.loop                ;
    rts                             ;

digits
    dc.b    %11111000               ; 0
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %00100000               ; 1
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    ds.b    3,0
    
    dc.b    %11111000               ; 2
    dc.b    %00001000
    dc.b    %11111000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 3
    dc.b    %00001000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %10001000               ; 4
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %00001000
    ds.b    3,0

    dc.b    %11111000               ; 5
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 6
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 7
    dc.b    %00001000
    dc.b    %00010000
    dc.b    %00100000
    dc.b    %00100000
    ds.b    3,0

    dc.b    %11111000               ; 8
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 9
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; A
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %10001000
    dc.b    %10001000
    ds.b    3,0

    dc.b    %11110000               ; B
    dc.b    %10001000
    dc.b    %11110000
    dc.b    %10001000
    dc.b    %11110000
    ds.b    3,0

    dc.b    %11111000               ; C
    dc.b    %10000000
    dc.b    %10000000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11110000               ; D
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %11110000
    ds.b    3,0

    dc.b    %11111000               ; E
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; F
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %10000000
    dc.b    %10000000
    ds.b    3,0
    
    even
    endif

*------ HIDDEN MESSAGE FOR LORD -----------------------------------------------*

hiddenmessage
    lea     .snowflake(pc),a3       ; snowflake
    moveq   #1,d4                   ; start x (pixel)
    ror.b   d4,d4                   ; d4 = %1000 0000
    move.l  vbitplane2(a5),a1       ;
    add.w   #6*16*pwidth+22,a1      ;
    lea     fontwest(pc),a4         ;
    moveq   #pwidth,d2              ;
    bsr     printtextlarge          ;

    move.w  #texthidden-text,vtextpointer(a5)
    move.w  #5*16*pwidth+8,d6       ;
    moveq   #6-1,d7                 ; 6 lines of text
.loop
    move.l  vbitplane2(a5),a1       ;
    add.w   d6,a1                   ;
    add.w   #16*pwidth,d6           ; next line
    movem.l d6/d7,-(a7)             ;
    moveq   #1,d4                   ; start x (pixel)
    ror.b   d4,d4                   ; d4 = %1000 0000
    bsr     printtext               ;
    movem.l (a7)+,d6/d7             ;
    dbf     d7,.loop                ;

    clr.w   vtextpointer(a5)        ; reset text
    rts                             ;

.snowflake
    dc.b    10,-1
    even

*------ CLEAR BITPLANES (CPU) -------------------------------------------------*
    
clearbitplanes
    move.l  bitplane1base(pc),a0    ;
    move.l  bitplane2base(pc),a1    ;
    moveq   #0,d0                   ;
    move.w  #2*psize/16-1,d7        ;
.clear
    rept 4
    move.l  d0,(a0)+                ;
    move.l  d0,(a1)+                ;
    endr
    dbf     d7,.clear               ;
    rts                             ;

*------ CLEAR BITPLANE (BLITTER) ----------------------------------------------*

cls move.l  bitplane1base(pc),a0    ;
    moveq   #0,d0                   ;
    bsr     waitblitter             ;
    move.l  a0,$54(a6)              ; destination D
    move.w  #$0100,$40(a6)          ; bltcon0
    move.w  d0,$42(a6)              ; bltcon1
    move.l  d0,$64(a6)              ; modulos a,d
    move.w  #(pheight*2<<6)+(pwidth/2),$58(a6); bltsize and start
    rts                             ;

*------ INIT LOGO -------------------------------------------------------------*
    
initlogo
    bsr     waitblitter             ; wait copy/scroll
    bsr     clearbitplanes          ;

    lea     textspread(pc),a3       ; SPREAD
    moveq   #1,d4                   ; start x (pixel)
    ror.b   d4,d4                   ; d4 = %1000 0000
    move.l  logowestbase(pc),a1     ;
    add.w   #goldeny*lwidth+2,a1    ; golden ratio = 58 (logo = 102 height), x=2 bytes
    lea     fontwest(pc),a4         ; west
    moveq   #lwidth,d2              ;
    bsr     printtextlarge          ;

    move.l  bitplane1base(pc),a1    ; north (note: unbufferd bitplane)
    add.w   #goldeny*pwidth+2,a1    ; pos
    lea     fontnorth(pc),a4        ;
    moveq   #pwidth,d2              ;
    bsr     printtextlarge          ;

    move.l  bitplane2base(pc),a1    ; south (note: unbufferd bitplane)
    add.w   #goldeny*pwidth+2,a1    ; pos
    add.w   #psize,a1               ;
    lea     fontsouth(pc),a4        ;
    moveq   #pwidth,d2              ;
    bsr     printtextlarge          ;

    lea     textpoint(pc),a3        ; POINT
    moveq   #1,d4                   ; start x (pixel)
    ror.b   #3,d4                   ; d4 = %0010 0000
    move.l  logowestbase(pc),a1     ; west
    add.w   #(goldeny*lwidth)+(3*18*lwidth)+5+2,a1      ;
    lea     fontwest(pc),a4         ;
    moveq   #lwidth,d2              ;
    bsr     printtextlarge          ;

    move.l  bitplane1base(pc),a1    ; north (note: unbufferd bitplane)
    add.w   #3*18*pwidth+5+2,a1     ;
    add.w   #goldeny*pwidth,a1      ;
    lea     fontnorth(pc),a4        ;
    moveq   #pwidth,d2              ;
    bsr     printtextlarge          ;

    move.l  bitplane2base(pc),a1    ; south (note: unbufferd bitplane)
    add.w   #(goldeny*pwidth)+(3*18*pwidth)+5+2,a1      ;
    add.w   #psize,a1               ;
    lea     fontsouth(pc),a4        ;
    moveq   #pwidth,d2              ;
    bsr     printtextlarge          ;
    rts                             ;

*------ WAIT ------------------------------------------------------------------*

waitblitter
    btst.b  #14-8,$02(a6)           ; DMAB_BLTDONE = 14
.wait
    btst.b  #14-8,$02(a6)           ;
    bne     .wait                   ;
    rts                             ;

waitraster
    bsr     waitblitter             ; taken from Photon's MiniWrapper
    move.w  #$138,d0                ;
.wait
    move.l  4(a6),d1                ;
    lsr.l   #1,d1                   ;
    lsr.w   #7,d1                   ;
    cmp.w   d0,d1                   ;
    bne     .wait                   ;
    rts                             ;

*------ IRQ3 ------------------------------------------------------------------*

irq3
    movem.l a0-a6/d0-d7,-(a7)       ;
    
    move.w  $1e(a6),d0              ; interrupt request bits
    btst    #4,d0                   ;
    beq     .notcoper               ;
    bsr     lspplay                 ; LSP player tick (call once per frame)
    moveq   #$0010,d0               ;
    bra     .done                   ;

.notcoper
    btst    #5,d0                   ;
    beq     .exit                   ;
    bsr     vertb                   ;
    moveq   #$0020,d0               ;
    if testing
    move.w  #$0040,$180(a6)         ;
    endif

.done
    move.w  d0,$9c(a6)              ; delete request bit
    move.w  d0,$9c(a6)              ; 3 times? https://amycoders.org/tutorials/frametime.html
    move.w  d0,$9c(a6)              ;
.exit
    movem.l (a7)+,a0-a6/d0-d7       ;
    rte                             ;

*------ VERTB -----------------------------------------------------------------*

vertb
    btst    #6,$bfe001              ; left mouse button pressed?
    bne     .dontquit               ;
    st      vquit(a5)               ; -> signal idle loop to quit
.dontquit
    tst.b   vshowlogo(a5)           ;
    beq     .notlogo                ;
    bsr     effectlogo              ;
    bra     .end                    ;
.notlogo
    bsr     effect1919              ;
    
.end
    if numbers
    bsr     waitblitter             ;
    move.w  vframe(a5),d0           ;
    move.w  #2*8*pwidth+3,d1        ;
    bsr     printnumber             ;
    endif
    
    if instruments
    move.w  vinstlow(a5),d0         ;
    move.w  #4*8*pwidth+3,d1        ;
    bsr     printnumber             ;

    move.w  vinsthigh(a5),d0        ;
    move.w  #5*8*pwidth+3,d1        ;
    bsr     printnumber             ;


    move.w  vinst0(a5),d0           ;
    move.w  #7*8*pwidth+3,d1        ;
    bsr     printnumber             ;

    move.w  vinst1(a5),d0           ;
    move.w  #8*8*pwidth+3,d1        ;
    bsr     printnumber             ;

    move.w  vinst2(a5),d0           ;
    move.w  #9*8*pwidth+3,d1        ;
    bsr     printnumber             ;

    move.w  vinst3(a5),d0           ;
    move.w  #10*8*pwidth+3,d1       ;
    bsr     printnumber             ;
    endif

    addq.w  #1,vframe(a5)           ; advance frame number
    rts                             ;

*------ 1919 EFFECT -----------------------------------------------------------*

effect1919
    tst.b   vinitlogo(a5)           ;
    bne     .nodraw                 ;

    bsr     play                    ;

    movem.l vbitplane1(a5),d0/d1    ; bitplane double buffering
    exg     d0,d1                   ;
    move.l  d0,vbitplane1(a5)       ;
    move.l  d1,vbitplane2(a5)       ;
    move.l  d0,$e0(a6)              ;
;   move.l  textplanebase(pc),d0    ; testing handle printtext
    move.l  d0,$e4(a6)              ; shadow plane

; textplane
    tst.w   vwaiting(a5)            ;
    bne     .waiting                ;

    tst.w   vshow(a5)               ;
    beq     .ringb                  ;

    clr.w   vshow(a5)               ;
    clr.w   vgive(a5)               ;
    clr.w   vwaiting(a5)            ;
    subq.w  #1,vusedtbuffers(a5)    ;
    
    add.w   #tsize,vshowtextploffset(a5)    ; ring buffer
    cmp.w   #numtbuffers*tsize,vshowtextploffset(a5)
    bne     .ringb                  ;
    clr.w   vshowtextploffset(a5)
.ringb

    tst.b   vautotext(a5)           ;
    beq     .notrigger              ;
    
    tst.b   vtriggertext(a5)        ;
    beq     .notrigger              ;
    clr.b   vtriggertext(a5)        ;
    st      vgive(a5)               ;
    st      vwaiting(a5)            ; semaphore
.notrigger

.waiting
    move.l  textplanebase(pc),a0    ;
    add.w   vshowtextploffset(a5),a0    ;
    move.l  a0,d0                   ;
    move.l  clistbase(pc),a1        ;
    add.w   #textplanep-clist,a1    ;
    move.w  d0,4(a1)                ;
    swap    d0                      ;
    move.w  d0,(a1)                 ;

    bsr     copy                    ;

    tst.b   vscroll(a5)             ;
    beq     .noscroll               ;
    bsr     scroll                  ;
    addq.w  #1,vstepper(a5)         ;
.noscroll
    bsr     waitblitter             ; wait scroll
    
    cmp.w   #4,vstepper(a5)         ; (4 is good, 3 too?)
    bne     .nodraw                 ;
    clr.w   vstepper(a5)            ;

    tst.b   vdraw(a5)               ;
    beq     .nodraw                 ;

    tst.b   vdynamic(a5)            ;
    beq     .pulsar                 ;
    move.l  vbitplane2(a5),a0       ;
    move.l  #$ffffffff,238*pwidth+8(a0)     ; begin of line (flatline)
    move.l  #$7ffffffc,238*pwidth+8+20(a0)  ; end of line (flatline)

    bsr     calcdynamic             ;
    lea     dynamicline(pc),a1      ;
    move.w  #238*pwidth+12,d0       ; start y, start x = 64 (8*8) + 32 (4*8)
    bsr     draw1919                ;
.nodraw
    rts                             ;

.pulsar
    move.l  vlinedatapointer(a5),a1 ;
    move.w  #238*pwidth+8,d0        ; start y, start x = 64 (8*8)
    tst.b   vfullscreen(a5)         ;
    beq     .no                     ;
    move.w  #255*pwidth+8,d0        ; start y, start x = 64 (8*8)
.no
    bsr     draw1919                ;
    cmp.l   #linedataend,a1         ;
    bne     .notatend               ;
;   lea     linedata(pc),a1         ; no need to reset
    clr.b   vscroll(a5)             ; "stop" demo here
.notatend
    move.l  a1,vlinedatapointer(a5) ;
    rts                             ;

*------- LOGO EFFECT ----------------------------------------------------------*

effectlogo
    move.l  veasep(a5),a1           ;
    movem.w (a1)+,d0/d1             ; y256 y320

    move.l  d1,d4                   ;

    move.l  logowestbase(pc),a0     ; west
    move.w  #335,d3                 ;
    sub.w   d1,d3                   ;
    move.w  d3,d2                   ;
    asr.w   #3,d3                   ;
    add.w   d3,a0                   ;
    move.l  a0,$e4(a6)              ;
    and.w   #$000f,d2               ;
    moveq   #$f,d1                  ;
    sub.w   d2,d1                   ;
    asl.w   #4,d1                   ;
    move.w  d1,$102(a6)             ;

    movem.l a0-a5/d0-d7,-(a7)       ; east
    move.l  #320,d0                 ;
    sub.w   d4,d0                   ;
    add.l   #180,d0                 ;
    move.l  #126,d1                 ;
    bsr     updatesprites           ;
    movem.l (a7)+,a0-a5/d0-d7       ;

    move.l  bitplane1base(pc),a0    ; north (note: unbufferd bitplane)
    move.w  #psize,d2               ; (psize -> hide 100%)
    sub.w   d0,d2                   ;
    add.w   d2,a0                   ;
    move.l  a0,$e0(a6)              ;
        
    move.l  bitplane2base(pc),a0    ; south (note: unbufferd bitplane)
    add.w   d0,a0                   ; (d0 = psize -> show 100%)
    move.l  a0,$e8(a6)              ;

    move.w  d4,d0                   ;
    sub.w   #320-255,d0             ; crappy
    bpl     .inrange                ;
    moveq   #0,d0                   ;
.inrange
    asr.w   #4,d0                   ; r value
    move.w  d0,d1                   ;
    asl.w   #4,d1                   ;
    or.w    d1,d0                   ; g value
    asl.w   #4,d1                   ;
    or.w    d1,d0                   ; b value

    lea     $182(a6),a0             ; bitplane colors $182 - $18e (7 regs)
    move.w  d0,(a0)+                ;
    move.w  d0,(a0)+                ;
    move.w  d0,(a0)+                ;
    move.w  d0,(a0)+                ;
    move.w  d0,(a0)+                ;
    move.w  d0,(a0)+                ;
    move.w  d0,(a0)                 ;
    move.w  d0,$1a2(a6)             ; sprite (east) colors
    move.w  d0,$1aa(a6)             ;
    move.w  d0,$1b2(a6)             ;
    move.w  d0,$1ba(a6)             ;

    lea     easeend(pc),a2          ;
    cmp.l   a1,a2                   ;
    bne     .notdone                ;

    bsr     cls                     ; quickly back to 1919 effect
    bsr     waitblitter             ;
    clr.b   vshowlogo(a5)           ;
    move.l  clistbase(pc),a1        ;
    move.w  #$2200,bplcon0-clist(a1)        ;
    move.w  #$1200,bplcon0text-clist(a1)    ;
    move.l  clistbase(pc),$80(a6)   ;
.notdone
    move.l  a1,veasep(a5)           ;
    rts                             ;

*------ DRAW 1919 -------------------------------------------------------------*

; params d0, a1

draw1919
    moveq   #0,d1                   ;
    move.b  1(a1),d1                ;
    and.w   #%111,d1                ; safety (y 0...7 is valid)
    
    move.w  d1,d2                   ; eraser offset
    asl.w   #2,d2                   ; *4 (long word offset)
    lea     erasers(pc),a4          ;
    add.w   d2,a4                   ;

    add.w   d1,d1                   ;
    move.l  vbitplane2(a5),a0       ;
    add.w   d0,a0                   ; start pixel
    sub.w   pwidthmultiples(pc,d1.w),a0 ; adjust start y

    moveq   #1,d4                   ; start x (pixel)
    ror.b   d4,d4                   ; d4 = %1000 0000
    
    moveq   #0,d7                   ; clear bit 16 (control bit) (no other free registers)
.lineloop
    moveq   #0,d0                   ; x1
    move.b  (a1)+,d0                ;
    moveq   #0,d1                   ; y1
    move.b  (a1)+,d1                ;
    moveq   #0,d2                   ; x2
    move.b  (a1)+,d2                ;
    cmp.b   #-1,d2                  ; end of data?
    beq     .done                   ;
    moveq   #0,d3                   ; y2
    move.b  (a1)+,d3                ;

    bsr     drawline                ;
    
    subq.w  #2,a1                   ; current p2 becomes p1
    bra     .lineloop               ;

.done
    rts                             ;

pwidthmultiples
    dc.w    0*pwidth
    dc.w    1*pwidth
    dc.w    2*pwidth
    dc.w    3*pwidth
    dc.w    4*pwidth
    dc.w    5*pwidth
    dc.w    6*pwidth
    dc.w    7*pwidth

*------ CALCULATE DYNAMIC LINE ------------------------------------------------*

calcdynamic
    lea     active+1(pc),a0         ; skip x value
    moveq   #0,d1                   ;
    rept 64
    move.b  d1,(a0)                 ; reset y
    addq.w  #2,a0                   ; next y
    endr

    moveq   #(138+192)/6-1,d6       ;
    lea     peaksff76(pc),a1        ;
    
;   moveq   #1-1,d6                 ; Testing only
;   lea     peakstest(pc),a1        ; Testing only
.loop
    moveq   #0,d2                   ; clear d2 (lower word)
    move.b  1(a1),d2                ; peak value
    beq     .nopeak                 ;
    subq.b  #1,1(a1)                ;

    lea     base(pc),a0             ;
    add.w   2(a1),a0                ; "wa"
    asl.w   #2,d2                   ; *4 (make offset)
    add.w   d2,a0                   ;
    move.l  (a0),a0                 ; address to values

    move.w  4(a1),d0                ; start x offset (must be even (*2))
    add.b   (a0)+,d0                ; pattern/wave start x offset (must be even (*2))
    lea     active+1(pc),a2         ;
    add.w   d0,a2                   ; begin at this position

    moveq   #0,d7                   ;
    move.b  (a0)+,d7                ;

    rept 18
    move.b  (a0)+,d0                ; 18 values at least (save dbf overhead)
    add.b   d0,(a2)                 ;
    addq.w  #2,a2                   ;
    endr

.innerloop
    move.b  (a0)+,d0                ;
    add.b   d0,(a2)                 ;
    addq.w  #2,a2                   ;
    dbf     d7,.innerloop           ;
.nopeak
    addq.w  #6,a1                   ; next instrument
    dbf     d6,.loop                ;

    lea     activeend(pc),a0        ; end of line
    move.b  #161,(a0)+              ; x value (31 + 2*64 = 159 -> next is 161)
    clr.b   (a0)+                   ; y value
    move.b  #-1,(a0)                ; end of line/data
    rts                             ;

*------ WAVE PATTERNS ---------------------------------------------------------*

wb10    dc.b    0*2, 27-18-1,  0,0,1,3,6,11,17,25,32,38,40,38,33,26,19,14,11,9,9,10,10,9,8,6,4,2,1
wb9     dc.b    2*2, 27-18-1,  0,0,1,3,5,10,16,22,29,34,36,34,29,23,17,12,9,8,8,9,9,8,7,5,4,2,1
wb8     dc.b    4*2, 27-18-1,  0,0,1,2,5,9,14,20,26,30,32,30,26,21,15,11,8,7,7,8,8,7,6,5,3,2,1
wb7     dc.b    6*2, 27-18-1,  0,0,1,2,4,8,12,17,22,26,28,26,23,18,13,10,7,6,6,7,7,6,5,4,3,2,1
wb6     dc.b    8*2, 27-18-1,  0,0,0,2,3,6,10,15,19,22,24,22,19,15,11,8,6,5,5,6,6,5,4,3,2,1,0
wb5     dc.b    10*2, 27-18-1,  0,0,0,1,3,5,8,12,16,19,20,19,16,13,9,7,5,4,4,5,5,4,4,3,2,1,0
wb4     dc.b    12*2, 27-18-1,  0,0,0,1,2,4,7,10,13,15,16,15,13,10,7,5,4,3,3,4,4,3,3,2,1,1,0
wb3     dc.b    14*2, 27-18-1,  0,0,0,1,1,3,5,7,9,11,12,11,9,7,5,4,3,2,2,3,3,2,2,1,1,0,0
wb2     dc.b    16*2, 27-18-1,  0,0,0,0,1,2,3,5,6,7,8,7,6,5,3,2,2,1,1,2,2,1,1,1,0,0,0
wb1     dc.b    18*2, 27-18-1,  0,0,0,0,0,1,1,2,3,3,4,3,3,2,1,1,1,0,0,1,1,0,0,0,0,0,0

we10    dc.b    -18*2, 20-18-1,  0,0,1,3,6,11,17,25,32,38,40,38,32,25,17,11,6,3,1,0
we9     dc.b    -16*2, 20-18-1,  0,0,1,3,5,10,16,22,29,34,36,34,29,22,16,10,5,3,1,0
we8     dc.b    -14*2, 20-18-1,  0,0,1,2,5,9,14,20,26,30,32,30,26,20,14,9,5,2,1,0
we7     dc.b    -12*2, 20-18-1,  0,0,1,2,4,8,12,17,22,26,28,26,22,17,12,8,4,2,1,0
we6     dc.b    -10*2, 20-18-1,  0,0,0,2,3,6,10,15,19,22,24,22,19,15,10,6,3,2,0,0
we5     dc.b    -8*2, 20-18-1,  0,0,0,1,3,5,8,12,16,19,20,19,16,12,8,5,3,1,0,0
we4     dc.b    -6*2, 20-18-1,  0,0,0,1,2,4,7,10,13,15,16,15,13,10,7,4,2,1,0,0
we3     dc.b    -4*2, 20-18-1,  0,0,0,1,1,3,5,7,9,11,12,11,9,7,5,3,1,1,0,0
we2     dc.b    -2*2, 20-18-1,  0,0,0,0,1,2,3,5,6,7,8,7,6,5,3,2,1,0,0,0
we1     dc.b    0*2, 20-18-1,  0,0,0,0,0,1,1,2,3,3,4,3,3,2,1,1,0,0,0,0

wd1     dc.b    0*2, 20-18-1,  0,0,0,0,0,0,1,1,2,2,2,2,2,1,1,0,0,0,0,0
wd2     dc.b    0*2, 20-18-1,  0,0,0,0,0,1,2,3,4,4,5,4,4,3,2,1,0,0,0,0
wd3     dc.b    0*2, 20-18-1,  0,0,0,0,1,2,3,4,6,7,7,7,6,4,3,2,1,0,0,0
wd4     dc.b    0*2, 20-18-1,  0,0,0,0,1,2,4,6,8,9,10,9,8,6,4,2,1,0,0,0
wd5     dc.b    0*2, 20-18-1,  0,0,0,1,2,3,5,7,10,11,12,11,10,7,5,3,2,1,0,0
wd6     dc.b    0*2, 20-18-1,  0,0,0,1,2,4,6,9,12,14,15,14,12,9,6,4,2,1,0,0
wd7     dc.b    0*2, 20-18-1,  0,0,0,1,2,5,7,11,14,16,17,16,14,11,7,5,2,1,0,0

;wh1    dc.b    3*2, 20-18-1,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;wh2    dc.b    6*2, 20-18-1,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;wh3    dc.b    9*2, 20-18-1,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
wh4     dc.b    12*2, 20-18-1,  0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0
wh5     dc.b    15*2, 20-18-1,  0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0
wh6     dc.b    17*2, 20-18-1,  0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0
wh7     dc.b    19*2, 20-18-1,  0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0
wh8     dc.b    20*2, 20-18-1,  0,0,0,0,0,0,0,1,1,1,2,1,1,1,0,0,0,0,0,0
wh9     dc.b    21*2, 20-18-1,  0,0,0,0,0,0,1,1,1,2,2,2,1,1,1,0,0,0,0,0
wh10    dc.b    22*2, 20-18-1,  0,0,0,0,0,0,1,1,2,2,2,2,2,1,1,0,0,0,0,0
wh11    dc.b    21*2, 20-18-1,  0,0,0,0,0,0,1,1,2,2,2,2,2,1,1,0,0,0,0,0
wh12    dc.b    20*2, 20-18-1,  0,0,0,0,0,0,1,1,2,2,3,2,2,1,1,0,0,0,0,0
wh13    dc.b    19*2, 20-18-1,  0,0,0,0,0,0,1,2,2,3,3,3,2,2,1,0,0,0,0,0
wh14    dc.b    17*2, 20-18-1,  0,0,0,0,0,1,1,2,2,3,3,3,2,2,1,1,0,0,0,0
wh15    dc.b    15*2, 20-18-1,  0,0,0,0,0,1,1,2,3,3,3,3,3,2,1,1,0,0,0,0
wh16    dc.b    12*2, 20-18-1,  0,0,0,0,0,1,1,2,3,3,4,3,3,2,1,1,0,0,0,0
wh17    dc.b    9*2, 20-18-1,  0,0,0,0,0,1,1,2,3,4,4,4,3,2,1,1,0,0,0,0
wh18    dc.b    6*2, 20-18-1,  0,0,0,0,0,1,2,2,3,4,4,4,3,2,2,1,0,0,0,0
wh19    dc.b    3*2, 20-18-1,  0,0,0,0,0,1,2,3,3,4,4,4,3,3,2,1,0,0,0,0
wh20    dc.b    0*2, 20-18-1,  0,0,0,0,0,1,2,3,4,4,5,4,4,3,2,1,0,0,0,0
wh21    dc.b    -3*2, 20-18-1,  0,0,0,0,0,1,2,3,4,4,5,4,4,3,2,1,0,0,0,0
wh22    dc.b    -6*2, 20-18-1,  0,0,0,0,0,1,2,3,4,5,5,5,4,3,2,1,0,0,0,0
wh23    dc.b    -9*2, 20-18-1,  0,0,0,0,0,1,2,3,4,5,5,5,4,3,2,1,0,0,0,0
wh24    dc.b    -12*2, 20-18-1,  0,0,0,0,0,1,2,3,4,5,6,5,4,3,2,1,0,0,0,0
wh25    dc.b    -15*2, 20-18-1,  0,0,0,0,1,1,2,3,5,5,6,5,5,3,2,1,1,0,0,0
wh26    dc.b    -17*2, 20-18-1,  0,0,0,0,1,1,2,4,5,6,6,6,5,4,2,1,1,0,0,0
wh27    dc.b    -19*2, 20-18-1,  0,0,0,0,1,1,3,4,5,6,6,6,5,4,3,1,1,0,0,0
wh28    dc.b    -20*2, 20-18-1,  0,0,0,0,1,2,3,4,5,6,7,6,5,4,3,2,1,0,0,0
wh29    dc.b    -21*2, 20-18-1,  0,0,0,0,1,2,3,4,5,6,7,6,5,4,3,2,1,0,0,0
wh30    dc.b    -22*2, 20-18-1,  0,0,0,0,1,2,3,4,6,7,7,7,6,4,3,2,1,0,0,0
wh31    dc.b    -21*2, 20-18-1,  0,0,0,0,1,2,3,4,6,7,7,7,6,4,3,2,1,0,0,0
wh32    dc.b    -20*2, 20-18-1,  0,0,0,0,1,2,3,5,6,7,8,7,6,5,3,2,1,0,0,0
wh33    dc.b    -19*2, 20-18-1,  0,0,0,0,1,2,3,5,6,7,8,7,6,5,3,2,1,0,0,0
wh34    dc.b    -17*2, 20-18-1,  0,0,0,0,1,2,3,5,6,8,8,8,6,5,3,2,1,0,0,0
wh35    dc.b    -15*2, 20-18-1,  0,0,0,0,1,2,3,5,7,8,8,8,7,5,3,2,1,0,0,0
wh36    dc.b    -12*2, 20-18-1,  0,0,0,0,1,2,4,5,7,8,9,8,7,5,4,2,1,0,0,0
wh37    dc.b    -9*2, 20-18-1,  0,0,0,0,1,2,4,5,7,8,9,8,7,5,4,2,1,0,0,0
wh38    dc.b    -6*2, 20-18-1,  0,0,0,0,1,2,4,6,7,9,9,9,7,6,4,2,1,0,0,0
wh39    dc.b    -3*2, 20-18-1,  0,0,0,0,1,2,4,6,7,9,9,9,7,6,4,2,1,0,0,0
wh40    dc.b    0*2, 20-18-1,  0,0,0,0,1,2,4,6,8,9,10,9,8,6,4,2,1,0,0,0
wh41    dc.b    3*2, 20-18-1,  0,0,0,0,1,2,4,6,8,9,10,9,8,6,4,2,1,0,0,0
wh42    dc.b    6*2, 20-18-1,  0,0,0,0,1,3,4,6,8,9,10,9,8,6,4,3,1,0,0,0
wh43    dc.b    9*2, 20-18-1,  0,0,0,0,1,3,4,6,8,10,10,10,8,6,4,3,1,0,0,0
wh44    dc.b    12*2, 20-18-1,  0,0,0,0,1,3,4,7,9,10,11,10,9,7,4,3,1,0,0,0
wh45    dc.b    15*2, 20-18-1,  0,0,0,0,1,3,5,7,9,10,11,10,9,7,5,3,1,0,0,0
wh46    dc.b    17*2, 20-18-1,  0,0,0,0,1,3,5,7,9,10,11,10,9,7,5,3,1,0,0,0
wh47    dc.b    19*2, 20-18-1,  0,0,0,1,1,3,5,7,9,11,11,11,9,7,5,3,1,1,0,0
wh48    dc.b    20*2, 20-18-1,  0,0,0,1,1,3,5,7,9,11,12,11,9,7,5,3,1,1,0,0
wh49    dc.b    21*2, 20-18-1,  0,0,0,1,2,3,5,7,10,11,12,11,10,7,5,3,2,1,0,0
wh50    dc.b    22*2, 20-18-1,  0,0,0,1,2,3,5,7,10,11,12,11,10,7,5,3,2,1,0,0
wh51    dc.b    21*2, 20-18-1,  0,0,0,1,2,3,5,8,10,12,12,12,10,8,5,3,2,1,0,0
wh52    dc.b    20*2, 20-18-1,  0,0,0,1,2,3,5,8,10,12,13,12,10,8,5,3,2,1,0,0
wh53    dc.b    19*2, 20-18-1,  0,0,0,1,2,3,5,8,10,12,13,12,10,8,5,3,2,1,0,0
wh54    dc.b    17*2, 20-18-1,  0,0,0,1,2,3,6,8,11,12,13,12,11,8,6,3,2,1,0,0
wh55    dc.b    15*2, 20-18-1,  0,0,0,1,2,3,6,8,11,13,13,13,11,8,6,3,2,1,0,0
wh56    dc.b    12*2, 20-18-1,  0,0,0,1,2,4,6,8,11,13,14,13,11,8,6,4,2,1,0,0
wh57    dc.b    9*2, 20-18-1,  0,0,0,1,2,4,6,9,11,13,14,13,11,9,6,4,2,1,0,0
wh58    dc.b    6*2, 20-18-1,  0,0,0,1,2,4,6,9,11,13,14,13,11,9,6,4,2,1,0,0
wh59    dc.b    3*2, 20-18-1,  0,0,0,1,2,4,6,9,12,14,14,14,12,9,6,4,2,1,0,0
wh60    dc.b    0*2, 20-18-1,  0,0,0,1,2,4,6,9,12,14,15,14,12,9,6,4,2,1,0,0
wh61    dc.b    -3*2, 20-18-1,  0,0,0,1,2,4,6,9,12,14,15,14,12,9,6,4,2,1,0,0
wh62    dc.b    -6*2, 20-18-1,  0,0,0,1,2,4,6,9,12,14,15,14,12,9,6,4,2,1,0,0
wh63    dc.b    -9*2, 20-18-1,  0,0,0,1,2,4,7,10,12,14,15,14,12,10,7,4,2,1,0,0
wh64    dc.b    -12*2, 20-18-1,  0,0,0,1,2,4,7,10,13,15,16,15,13,10,7,4,2,1,0,0
wh65    dc.b    -15*2, 20-18-1,  0,0,0,1,2,4,7,10,13,15,16,15,13,10,7,4,2,1,0,0
wh66    dc.b    -17*2, 20-18-1,  0,0,0,1,2,4,7,10,13,15,16,15,13,10,7,4,2,1,0,0
wh67    dc.b    -19*2, 20-18-1,  0,0,0,1,2,4,7,10,13,15,16,15,13,10,7,4,2,1,0,0
wh68    dc.b    -20*2, 20-18-1,  0,0,0,1,2,4,7,10,13,16,17,16,13,10,7,4,2,1,0,0
wh69    dc.b    -21*2, 20-18-1,  0,0,0,1,2,4,7,10,14,16,17,16,14,10,7,4,2,1,0,0
wh70    dc.b    -22*2, 20-18-1,  0,0,0,1,2,5,7,11,14,16,17,16,14,11,7,5,2,1,0,0
wh71    dc.b    -21*2, 20-18-1,  0,0,0,1,2,5,7,11,14,16,17,16,14,11,7,5,2,1,0,0
wh72    dc.b    -20*2, 20-18-1,  0,0,0,1,2,5,8,11,14,17,18,17,14,11,8,5,2,1,0,0
wh73    dc.b    -19*2, 20-18-1,  0,0,0,1,3,5,8,11,14,17,18,17,14,11,8,5,3,1,0,0
wh74    dc.b    -17*2, 20-18-1,  0,0,0,1,3,5,8,11,15,17,18,17,15,11,8,5,3,1,0,0
wh75    dc.b    -15*2, 20-18-1,  0,0,0,1,3,5,8,11,15,17,18,17,15,11,8,5,3,1,0,0
wh76    dc.b    -12*2, 20-18-1,  0,0,0,1,3,5,8,12,15,18,19,18,15,12,8,5,3,1,0,0
wh77    dc.b    -9*2, 20-18-1,  0,0,0,1,3,5,8,12,15,18,19,18,15,12,8,5,3,1,0,0
wh78    dc.b    -6*2, 20-18-1,  0,0,0,1,3,5,8,12,15,18,19,18,15,12,8,5,3,1,0,0
wh79    dc.b    -3*2, 20-18-1,  0,0,0,1,3,5,8,12,16,18,19,18,16,12,8,5,3,1,0,0

wf1     dc.b    0*2, 64-18-1,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
wf2     dc.b    0*2, 64-18-1,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
wf3     dc.b    0*2, 64-18-1,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,3,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0
wf4     dc.b    0*2, 64-18-1,  0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,3,3,3,3,3,3,3,3,3,2,2,2,2,2,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0
wf5     dc.b    0*2, 64-18-1,  0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,4,4,4,4,4,4,4,4,3,3,3,3,2,2,2,2,2,1,1,1,1,1,0,0,0,0,0,0,0,0,0
wf6     dc.b    0*2, 64-18-1,  0,0,0,0,0,0,0,0,0,1,1,1,1,1,2,2,2,2,3,3,3,4,4,4,4,5,5,5,5,5,5,5,6,5,5,5,5,5,5,5,4,4,4,4,3,3,3,2,2,2,2,1,1,1,1,1,0,0,0,0,0,0,0,0
wf7     dc.b    0*2, 64-18-1,  0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,6,6,6,7,6,6,6,6,6,6,6,5,5,5,4,4,4,3,3,3,2,2,2,2,1,1,1,1,0,0,0,0,0,0,0
wf8     dc.b    0*2, 64-18-1,  0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,4,4,5,5,5,6,6,6,7,7,7,7,7,7,8,7,7,7,7,7,7,6,6,6,5,5,5,4,4,3,3,3,2,2,2,2,1,1,1,1,0,0,0,0,0,0
wf9     dc.b    0*2, 64-18-1,  0,0,0,0,0,0,1,1,1,1,1,2,2,2,3,3,4,4,4,5,5,6,6,6,7,7,8,8,8,8,8,8,9,8,8,8,8,8,8,7,7,6,6,6,5,5,4,4,4,3,3,2,2,2,1,1,1,1,1,0,0,0,0,0
wf10    dc.b    0*2, 64-18-1,  0,0,0,0,0,1,1,1,1,1,2,2,2,3,3,4,4,4,5,5,6,6,7,7,8,8,8,9,9,9,9,9,10,9,9,9,9,9,8,8,8,7,7,6,6,5,5,4,4,4,3,3,2,2,2,1,1,1,1,1,0,0,0,0
wf11    dc.b    0*2, 64-18-1,  0,0,0,0,0,1,1,1,1,2,2,2,3,3,3,4,4,5,5,6,7,7,8,8,9,9,9,10,10,10,10,10,11,10,10,10,10,10,9,9,9,8,8,7,7,6,5,5,4,4,3,3,3,2,2,2,1,1,1,1,0,0,0,0
wf12    dc.b    0*2, 64-18-1,  0,0,0,0,1,1,1,1,1,2,2,3,3,3,4,4,5,5,6,7,7,8,8,9,9,10,10,11,11,11,11,11,12,11,11,11,11,11,10,10,9,9,8,8,7,7,6,5,5,4,4,3,3,3,2,2,1,1,1,1,1,0,0,0
wf13    dc.b    0*2, 64-18-1,  0,0,0,0,1,1,1,1,2,2,2,3,3,4,4,5,5,6,7,7,8,8,9,10,10,11,11,12,12,12,12,12,13,12,12,12,12,12,11,11,10,10,9,8,8,7,7,6,5,5,4,4,3,3,2,2,2,1,1,1,1,0,0,0
wf14    dc.b    0*2, 64-18-1,  0,0,0,1,1,1,1,1,2,2,3,3,4,4,5,5,6,6,7,8,8,9,10,10,11,12,12,12,13,13,13,13,14,13,13,13,13,12,12,12,11,10,10,9,8,8,7,6,6,5,5,4,4,3,3,2,2,1,1,1,1,1,0,0
wf15    dc.b    0*2, 64-18-1,  0,0,0,1,1,1,1,2,2,2,3,3,4,4,5,6,6,7,8,8,9,10,10,11,12,12,13,13,14,14,14,14,15,14,14,14,14,13,13,12,12,11,10,10,9,8,8,7,6,6,5,4,4,3,3,2,2,2,1,1,1,1,0,0
wf16    dc.b    0*2, 64-18-1,  0,0,0,1,1,1,1,2,2,3,3,4,4,5,5,6,7,7,8,9,10,10,11,12,13,13,14,14,15,15,15,15,16,15,15,15,15,14,14,13,13,12,11,10,10,9,8,7,7,6,5,5,4,4,3,3,2,2,1,1,1,1,0,0
wf17    dc.b    0*2, 64-18-1,  0,0,1,1,1,1,2,2,2,3,3,4,4,5,6,6,7,8,9,10,10,11,12,13,13,14,15,15,16,16,16,16,17,16,16,16,16,15,15,14,13,13,12,11,10,10,9,8,7,6,6,5,4,4,3,3,2,2,2,1,1,1,1,0
wf18    dc.b    0*2, 64-18-1,  0,0,1,1,1,1,2,2,2,3,3,4,5,5,6,7,8,8,9,10,11,12,13,13,14,15,16,16,17,17,17,17,18,17,17,17,17,16,16,15,14,13,13,12,11,10,9,8,8,7,6,5,5,4,3,3,2,2,2,1,1,1,1,0
wf19    dc.b    0*2, 64-18-1,  0,0,1,1,1,1,2,2,3,3,4,4,5,6,6,7,8,9,10,11,12,13,13,14,15,16,16,17,18,18,18,18,19,18,18,18,18,17,16,16,15,14,13,13,12,11,10,9,8,7,6,6,5,4,4,3,3,2,2,1,1,1,1,0

wg1     dc.b    14*2, 54-18-1,  0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
wg2     dc.b    13*2, 54-18-1,  0,0,0,0,0,0,1,1,2,2,3,2,2,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
wg3     dc.b    12*2, 54-18-1,  0,0,0,0,0,1,2,2,3,4,4,4,3,2,2,1,0,0,0,0,0,1,1,1,2,2,2,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0
wg4     dc.b    11*2, 54-18-1,  0,0,0,0,0,1,2,3,4,5,6,5,4,3,2,1,1,0,0,0,0,1,1,2,2,3,2,2,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0
wg5     dc.b    10*2, 54-18-1,  0,0,0,0,1,2,3,4,6,7,7,7,6,4,3,2,1,0,0,0,1,1,2,3,3,3,3,3,2,1,1,0,0,0,0,0,1,1,1,2,2,2,1,1,1,0,0,0,0,0,0,0,0,0
wg6     dc.b    9*2, 54-18-1,  0,0,0,0,1,2,4,5,7,8,9,8,7,5,4,2,1,0,0,0,1,2,2,3,4,4,4,3,2,2,1,0,0,0,0,0,1,1,2,2,2,2,2,1,1,0,0,0,0,0,0,0,0,0
wg7     dc.b    8*2, 54-18-1,  0,0,0,0,1,3,4,6,8,9,10,9,8,6,4,3,1,1,0,1,1,2,3,4,4,5,4,4,3,2,1,0,0,0,0,0,1,2,2,2,3,2,2,2,1,0,0,0,0,0,0,0,0,0
wg8     dc.b    7*2, 54-18-1,  0,0,0,1,1,3,5,7,9,11,12,11,9,7,5,3,2,1,1,1,1,2,3,4,5,6,5,4,3,2,1,1,0,0,0,1,1,2,2,3,3,3,2,2,1,1,0,0,0,0,0,0,0,0
wg9     dc.b    6*2, 54-18-1,  0,0,0,1,2,3,6,8,11,12,13,12,11,8,6,3,2,1,1,1,2,3,4,5,6,6,6,5,4,3,1,1,0,0,0,1,1,2,3,3,4,3,3,2,1,1,0,0,0,0,0,0,0,0
wg10    dc.b    5*2, 54-18-1,  0,0,0,1,2,4,6,9,12,14,15,14,12,9,6,4,2,1,1,1,2,3,4,6,7,7,7,6,4,3,2,1,0,0,0,1,2,2,3,4,4,4,3,2,2,1,0,0,0,0,0,0,0,0
wg11    dc.b    4*2, 54-18-1,  0,0,0,1,2,4,7,10,13,15,16,15,13,10,7,4,2,1,1,1,2,3,5,6,7,8,7,6,5,3,2,1,0,0,0,1,2,3,4,4,4,4,4,3,2,1,0,0,0,0,0,0,0,0
wg12    dc.b    3*2, 54-18-1,  0,0,0,1,2,5,8,11,14,17,18,17,14,11,8,5,3,1,1,1,2,4,5,7,8,9,8,7,5,4,2,1,0,0,1,1,2,3,4,5,5,5,4,3,2,1,0,0,0,0,0,0,0,0
wg13    dc.b    2*2, 54-18-1,  0,0,0,1,3,5,8,12,15,18,19,18,15,12,8,5,3,2,1,1,2,4,6,7,9,9,9,7,6,4,2,1,1,0,1,1,2,3,4,5,5,5,4,3,2,1,0,0,0,0,0,0,0,0
wg14    dc.b    1*2, 54-18-1,  0,0,0,1,3,6,9,13,17,19,21,19,17,13,9,6,3,2,1,2,3,4,6,8,9,10,9,8,6,4,3,1,1,0,1,1,2,4,5,5,6,5,5,4,2,1,1,0,0,0,0,0,0,0
wg15    dc.b    0*2, 54-18-1,  0,0,0,1,3,6,10,14,18,21,22,21,18,14,10,6,3,2,1,2,3,5,7,9,10,11,10,9,7,5,3,1,1,1,1,2,3,4,5,6,6,6,5,4,3,1,1,0,0,0,0,0,0,0

    even
punks
wg  dc.l    0
    dc.l    wg1-base
    dc.l    wg2-base
    dc.l    wg3-base
    dc.l    wg4-base
    dc.l    wg5-base
    dc.l    wg6-base
    dc.l    wg7-base
    dc.l    wg8-base
    dc.l    wg9-base
    dc.l    wg10-base
    dc.l    wg11-base
    dc.l    wg12-base
    dc.l    wg13-base
    dc.l    wg14-base
    dc.l    wg15-base
    
wb  dc.l    0
    dc.l    wb1-base
    dc.l    wb2-base
    dc.l    wb3-base
    dc.l    wb4-base
    dc.l    wb5-base
    dc.l    wb6-base
    dc.l    wb7-base
    dc.l    wb8-base
    dc.l    wb9-base
    dc.l    wb10-base

we  dc.l    0
    dc.l    we1-base
    dc.l    we2-base
    dc.l    we3-base
    dc.l    we4-base
    dc.l    we5-base
    dc.l    we6-base
    dc.l    we7-base
    dc.l    we8-base
    dc.l    we9-base
    dc.l    we10-base

wh  dc.l    0
;   dc.l    wh1-base
;   dc.l    wh2-base
;   dc.l    wh3-base
    dc.l    wh4-base
    dc.l    wh5-base
    dc.l    wh6-base
    dc.l    wh7-base
    dc.l    wh8-base
    dc.l    wh9-base
    dc.l    wh10-base
    dc.l    wh11-base
    dc.l    wh12-base
    dc.l    wh13-base
    dc.l    wh14-base
    dc.l    wh15-base
    dc.l    wh16-base
    dc.l    wh17-base
    dc.l    wh18-base
    dc.l    wh19-base
    dc.l    wh20-base
    dc.l    wh21-base
    dc.l    wh22-base
    dc.l    wh23-base
    dc.l    wh24-base
    dc.l    wh25-base
    dc.l    wh26-base
    dc.l    wh27-base
    dc.l    wh28-base
    dc.l    wh29-base
    dc.l    wh30-base
    dc.l    wh31-base
    dc.l    wh32-base
    dc.l    wh33-base
    dc.l    wh34-base
    dc.l    wh35-base
    dc.l    wh36-base
    dc.l    wh37-base
    dc.l    wh38-base
    dc.l    wh39-base
    dc.l    wh40-base
    dc.l    wh41-base
    dc.l    wh42-base
    dc.l    wh43-base
    dc.l    wh44-base
    dc.l    wh45-base
    dc.l    wh46-base
    dc.l    wh47-base
    dc.l    wh48-base
    dc.l    wh49-base
    dc.l    wh50-base
    dc.l    wh51-base
    dc.l    wh52-base
    dc.l    wh53-base
    dc.l    wh54-base
    dc.l    wh55-base
    dc.l    wh56-base
    dc.l    wh57-base
    dc.l    wh58-base
    dc.l    wh59-base
    dc.l    wh60-base
    dc.l    wh61-base
    dc.l    wh62-base
    dc.l    wh63-base
    dc.l    wh64-base
    dc.l    wh65-base
    dc.l    wh66-base
    dc.l    wh67-base
    dc.l    wh68-base
    dc.l    wh69-base
    dc.l    wh70-base
    dc.l    wh71-base
    dc.l    wh72-base
    dc.l    wh73-base
    dc.l    wh74-base
    dc.l    wh75-base
    dc.l    wh76-base
    dc.l    wh77-base
    dc.l    wh78-base
    dc.l    wh79-base

wd  dc.l    0
    dc.l    wd1-base
    dc.l    wd2-base
    dc.l    wd3-base
    dc.l    wd4-base
    dc.l    wd5-base
    dc.l    wd6-base
    dc.l    wd7-base

wf  dc.l    0
    dc.l    wf1-base
    dc.l    wf2-base
    dc.l    wf3-base
    dc.l    wf4-base
    dc.l    wf5-base
    dc.l    wf6-base
    dc.l    wf7-base
    dc.l    wf8-base
    dc.l    wf9-base
    dc.l    wf10-base
    dc.l    wf11-base
    dc.l    wf12-base
    dc.l    wf13-base
    dc.l    wf14-base
    dc.l    wf15-base
    dc.l    wf16-base
    dc.l    wf17-base
    dc.l    wf18-base
    dc.l    wf19-base

erasers
    dc.l    erase0-base
    dc.l    erase1-base
    dc.l    erase2-base
    dc.l    erase3-base
    dc.l    erase4-base
    dc.l    erase5-base
    dc.l    erase6-base
    dc.l    erase7-base
    dc.l    erase8-base
    dc.l    erase9-base
    dc.l    erase10-base
    dc.l    erase11-base
    dc.l    erase12-base
    dc.l    erase13-base
    dc.l    erase14-base
    dc.l    erase15-base
    dc.l    erase16-base
    dc.l    erase17-base
    dc.l    erase18-base
    dc.l    erase19-base
    dc.l    erase20-base
    dc.l    erase21-base
    dc.l    erase22-base
    dc.l    erase23-base
    dc.l    erase24-base
    dc.l    erase25-base
    dc.l    erase26-base
    dc.l    erase27-base
    dc.l    erase28-base
    dc.l    erase29-base
    dc.l    erase30-base
    dc.l    erase31-base
    dc.l    erase32-base
    dc.l    erase33-base
    dc.l    erase34-base
    dc.l    erase35-base
    dc.l    erase36-base
    dc.l    erase37-base
    dc.l    erase38-base
    dc.l    erase39-base
    dc.l    erase40-base
    dc.l    erase41-base
    dc.l    erase42-base
    dc.l    erase43-base
    dc.l    erase44-base
    dc.l    erase45-base
    dc.l    erase46-base
    dc.l    erase47-base
    dc.l    erase48-base
    dc.l    erase49-base
    dc.l    erase50-base
    dc.l    erase51-base
    dc.l    erase52-base
    dc.l    erase53-base
    dc.l    erase54-base
    dc.l    erase55-base
    dc.l    erase56-base
    dc.l    erase57-base
    dc.l    erase58-base
    dc.l    erase59-base
    dc.l    erase60-base
    dc.l    erase61-base
    dc.l    erase62-base
    dc.l    erase63-base
    dc.l    erase64-base
    dc.l    erase65-base
    dc.l    erase66-base
    dc.l    erase67-base
    dc.l    erase68-base
    dc.l    erase69-base
    dc.l    erase70-base
punksend

*------ DRAW LINE -------------------------------------------------------------*

; Optimized Bresenham's line algorithm with
; pseudo depth buffer
; Based on http://www.edepot.com/linebresenham.html

drawline
    move.w  d2,d5                   ; d5 will become dx
    sub.w   d0,d5                   ; dx (d5) = x2 - x1
    
    cmp.w   d3,d1                   ; y2 (d3) >= y1 (d1)?
    bhi     .1                      ; ->  d1 > d3
    move.w  d3,d6                   ; y2 >= y1
    sub.w   d1,d6                   ; dy (d6) = y2 - y1
    moveq   #-pwidth,d0             ; incy = 1
    moveq   #4,d2                   ; offset to eraser
    bra     .2                      ;
.1  move.w  d1,d6                   ;
    sub.w   d3,d6                   ; dy (d6) = y1 - y2
    moveq   #pwidth,d0              ; incy = -1
    moveq   #-4,d2                  ; offset to eraser
.2  cmp.w   d5,d6                   ; dx >= dy?
    bhi     .ydir                   ; -> dx < dy (x direction wins vs bhs)

; x direction

    add.w   d6,d6                   ; dy *= 2
    move.w  d6,d1                   ; d1 will become balance
    sub.w   d5,d1                   ; balance = dy - dx
    move.w  d5,d7                   ; d7 = line len (dx)
    add.w   d5,d5                   ; dx *= 2
    subq.w  #1,d7                   ; because of dbf

    btst    #16,d7                  ;
    bne     .noerase                ; bit 16 is set -> no erase
.xloop
    move.b  d4,d3                   ;
    not.b   d3                      ;
    move.l  (a4),a3                 ;
    jsr     (a3)                    ; eraser (pseudo depth buffer)
.noerase
    or.b    d4,(a0)                 ; draw pixel
    ror.b   #1,d4                   ; x++
    bcc     .x1                     ;
    addq.w  #1,a0                   ; next byte
.x1 tst.w   d1                      ;
    bmi     .x2                     ; balance < 0
    add.w   d0,a0                   ; adjust y
    add.w   d2,a4                   ; adjust eraser size
    sub.w   d5,d1                   ; balance -= dx
.x2 add.w   d6,d1                   ; balance += dy
    dbf     d7,.xloop               ;
    moveq   #0,d7                   ; clear bit 16
    rts                             ;

; y direction

.ydir
    add.w   d5,d5                   ; dx *= 2
    move.w  d5,d1                   ; d1 is balance
    sub.w   d6,d1                   ; balance = dx - dy
    move.w  d6,d7                   ; d7 = line len (dy)
    add.w   d6,d6                   ; dy *= 2
    subq.w  #1,d7                   ; because of dbf
    tst.w   d0                      ;
    bgt     .yloopdown              ;

; y direction up

    btst    #16,d7                  ;
    bne     .yloopup                ; bit 16 is set -> no erase
    
    move.b  d4,d3                   ; clear pixels below line
    not.b   d3                      ;
    move.l  (a4),a3                 ;
    jsr     (a3)                    ; eraser (pseudo depth buffer)
.yloopup
    or.b    d4,(a0)                 ; draw pixel

    add.w   d0,a0                   ; adjust y
    add.w   d2,a4                   ; adjust eraser size (+4)
    tst.w   d1                      ;
    bmi     .y1                     ; balance < 0

    ror.b   #1,d4                   ; x++
    bcc     .y2                     ;
    addq.w  #1,a0                   ; next byte

.y2 move.b  d4,d3                   ;
    not.b   d3                      ;
    move.l  (a4),a3                 ;
    jsr     (a3)                    ; eraser (pseudo depth buffer)

    sub.w   d6,d1                   ; balance -= dy
.y1 add.w   d5,d1                   ; balance += dx
    dbf     d7,.yloopup             ;
    moveq   #-1,d7                  ; set bit 16 (no pseudo depth buffer)
    rts                             ;

*---------- Y DIRECTION DOWN -------*

.yloopdown
    or.b    d4,(a0)                 ; draw pixel
    
    add.w   d0,a0                   ; adjust y
    add.w   d2,a4                   ; adjust eraser size (-4)
    tst.w   d1                      ;
    bmi     .y4                     ; balance < 0

    move.b  d4,d3                   ;
    not.b   d3                      ;
    sub.w   d0,a0                   ; temporary adjustment
    move.l  4(a4),a3                ; -> 4 (temporary adjustment)
    jsr     (a3)                    ; eraser (pseudo depth buffer)
    add.w   d0,a0                   ;
    
    ror.b   #1,d4                   ; x++
    bcc     .y3                     ;
    addq.w  #1,a0                   ; next byte
.y3 sub.w   d6,d1                   ; balance -= dy
.y4 add.w   d5,d1                   ; balance += dx
    dbf     d7,.yloopdown           ;
    moveq   #0,d7                   ; clear bit 16
    rts                             ;

dynamicline
active
    ds.b    64*2,0
activeend
;   ds.b    100*2,0                 ; safety zone -> overwrite followed linedata instead

linedata
    dc.b    0,0,    190,0,  -1
    rept 10
    dc.b    0,0,    -1
    endr

    dc.b    0,0,    190,0,  -1
    rept 4
    dc.b    0,0,    -1
    endr
    dc.b    0,0,    190,0,  -1
    rept 10
    dc.b    0,0,    -1
    endr
    dc.b    0,0,    190,0,  -1
    rept 4
    dc.b    0,0,    -1
    endr
    dc.b    0,0,    190,0,  -1
    rept 4
    dc.b    0,0,    -1
    endr
    dc.b    0,0,    190,0,  -1
    rept 70
    dc.b    0,0,    -1
    endr

    rept 100
    dc.b    0,0,    190,0,  -1
    endr

linedatapulsar
    dc.b 0,0, 34,0, 37,1, 38,0, 42,1, 45,0, 48,1, 51,0, 52,0, 54,1, 58,2, 60,2, 65,1, 69,0, 74,2, 80,5, 82,6, 84,7, 86,8, 92,8, 95,6, 98,5, 105,5, 108,6, 111,7, 115,7, 117,8, 118,8, 121,5, 124,5, 128,3, 132,1, 138,0, 170,0, 171,1, 173,0, 187,0, 191,0, -1
    dc.b 0,0, 36,0, 39,1, 42,1, 51,1, 52,2, 56,2, 59,4, 60,6, 63,8, 68,9, 75,9, 81,7, 84,6, 90,6, 94,4, 97,3, 110,3, 113,4, 118,4, 122,5, 130,5, 132,3, 136,2, 138,1, 140,1, 142,0, 145,1, 148,0, 191,0, -1
    dc.b 0,0, 2,0, 4,1, 7,1, 9,0, 13,1, 15,0, 17,1, 19,1, 22,0, 40,0, 43,1, 47,1, 49,0, 51,1, 54,2, 59,3, 63,4, 67,4, 69,7, 71,10, 73,12, 74,14, 75,15, 76,15, 81,9, 85,7, 87,7, 88,8, 90,8, 93,10, 96,10, 98,11, 99,11, 101,9, 105,6, 109,4, 113,3, 125,3, 127,2, 129,3, 131,2, 135,2, 139,0, 186,0, 188,1, 190,0, 191,0, -1
    dc.b 0,0, 6,1, 10,0, 12,0, 14,1, 21,0, 42,0, 45,1, 53,1, 55,2, 56,2, 57,1, 59,3, 63,4, 66,4, 69,5, 72,7, 75,12, 76,13, 77,14, 78,16, 80,19, 81,16, 83,14, 86,10, 89,7, 92,6, 97,6, 98,7, 100,9, 101,10, 102,8, 103,6, 106,5, 111,5, 113,6, 115,7, 117,7, 120,4, 123,2, 134,2, 137,1, 139,0, 141,1, 144,0, 159,0, 161,1, 167,1, 170,0, 173,0, 177,1, 180,0, 182,0, 184,1, 186,0, 187,1, 188,1, 189,0, 191,0, -1
    dc.b 0,0, 38,0, 42,1, 45,0, 48,1, 53,1, 57,2, 58,3, 61,3, 62,4, 67,6, 69,7, 71,9, 74,9, 77,6, 82,4, 89,2, 92,4, 95,4, 101,6, 107,5, 110,4, 115,3, 122,3, 126,2, 132,2, 135,1, 138,1, 141,0, 146,1, 154,1, 155,0, 158,1, 177,1, 180,0, 182,1, 191,0, -1
    dc.b 0,0, 24,0, 28,1, 32,1, 34,0, 40,2, 49,2, 51,1, 56,2, 60,3, 62,4, 65,6, 74,6, 79,7, 81,9, 84,11, 89,10, 92,8, 96,6, 101,5, 103,6, 104,7, 107,7, 109,6, 113,8, 116,10, 118,12, 121,12, 124,8, 127,5, 128,3, 131,4, 133,3, 137,2, 141,1, 143,0, 146,1, 150,0, 153,1, 156,0, 159,1, 163,1, 166,0, 169,1, 183,1, 191,0, -1
    dc.b 0,1, 49,1, 51,2, 56,3, 60,5, 62,7, 63,10, 64,11, 66,10, 69,9, 73,6, 76,3, 81,1, 84,3, 87,6, 89,5, 93,3, 97,4, 100,2, 102,4, 103,3, 105,3, 108,4, 112,5, 114,6, 116,6, 118,7, 120,8, 123,9, 125,9, 128,5, 132,4, 136,3, 139,1, 146,1, 148,2, 152,0, 188,1, 190,0, 191,1, -1
    dc.b 0,0, 5,0, 9,1, 11,0, 13,1, 40,1, 42,2, 51,2, 53,3, 56,3, 59,4, 62,7, 64,10, 66,11, 68,13, 70,15, 72,19, 73,21, 75,22, 77,20, 79,16, 83,10, 85,9, 95,9, 99,7, 104,7, 108,6, 116,4, 121,4, 127,2, 137,2, 139,1, 163,1, 165,0, 167,1, 188,1, 189,0, 191,1, -1
    dc.b 0,1, 38,1, 39,2, 41,1, 45,1, 46,2, 58,2, 63,5, 66,7, 73,9, 77,10, 79,7, 82,4, 86,7, 89,9, 95,9, 97,8, 98,7, 102,5, 105,3, 112,0, 115,0, 118,2, 134,2, 137,1, 140,2, 142,1, 171,1, 174,2, 185,1, 186,1, 189,0, 191,1, -1
    dc.b 0,0, 5,0, 10,1, 20,1, 22,0, 26,1, 43,1, 45,2, 46,1, 48,2, 57,2, 60,4, 63,7, 66,9, 70,9, 72,8, 76,12, 77,14, 78,16, 81,16, 84,15, 87,15, 88,13, 91,11, 93,10, 99,10, 101,11, 103,12, 104,12, 106,13, 108,12, 110,11, 114,10, 117,10, 118,9, 121,7, 125,5, 128,5, 131,4, 133,2, 137,2, 139,1, 141,2, 143,1, 181,0, 182,2, 184,0, 185,1, 191,1, -1
    dc.b 0,1, 5,1, 8,0, 11,2, 21,1, 29,1, 31,2, 34,1, 42,2, 47,2, 53,3, 55,5, 58,9, 60,10, 62,14, 63,17, 65,18, 66,18, 68,16, 71,16, 72,20, 74,22, 75,19, 76,17, 77,16, 78,12, 81,11, 86,11, 88,12, 90,12, 94,10, 95,8, 103,8, 105,7, 107,5, 110,5, 115,8, 117,9, 118,10, 120,12, 123,11, 125,9, 126,9, 129,6, 132,5, 134,4, 137,2, 140,2, 143,1, 169,1, 171,2, 173,1, 184,1, 186,0, 187,1, 191,1, -1
    dc.b 0,1, 28,1, 30,2, 33,2, 36,1, 38,2, 40,1, 43,2, 50,2, 52,4, 56,4, 60,8, 63,12, 65,14, 70,14, 72,15, 74,16, 76,17, 80,17, 82,14, 84,12, 85,12, 87,10, 92,10, 95,11, 97,11, 99,10, 100,9, 103,7, 106,6, 111,7, 114,8, 119,8, 121,7, 127,7, 131,5, 135,5, 138,4, 142,2, 144,1, 191,1, -1
    dc.b 0,1, 39,1, 41,2, 44,1, 45,2, 47,1, 54,3, 61,3, 64,4, 66,5, 69,8, 70,11, 71,12, 74,12, 76,13, 77,15, 81,16, 83,15, 86,13, 89,10, 92,10, 96,8, 97,9, 101,11, 102,12, 104,14, 105,16, 106,18, 107,18, 109,17, 110,17, 112,15, 113,13, 116,10, 120,7, 124,5, 127,2, 145,2, 146,1, 151,1, 156,2, 158,0, 160,2, 163,1, 178,1, 181,0, 182,1, 191,1, -1
    dc.b 0,1, 4,2, 6,1, 37,1, 40,2, 42,3, 46,2, 49,3, 51,2, 52,2, 56,4, 62,5, 69,6, 74,6, 78,10, 80,11, 82,13, 84,14, 87,12, 90,10, 101,10, 105,9, 108,8, 110,10, 114,11, 117,12, 119,12, 125,6, 129,4, 131,4, 134,2, 137,3, 139,3, 142,2, 146,1, 148,2, 151,1, 170,1, 172,0, 174,1, 191,1, -1
    dc.b 0,1, 19,1, 21,2, 23,1, 32,2, 46,2, 48,3, 52,3, 55,5, 57,7, 59,10, 61,13, 63,14, 66,14, 69,13, 71,13, 75,12, 78,10, 84,11, 87,13, 89,13, 94,10, 96,10, 98,8, 99,8, 103,6, 110,6, 112,8, 115,9, 118,12, 119,13, 120,15, 121,15, 123,14, 125,12, 126,11, 128,10, 132,6, 135,5, 140,3, 145,2, 146,1, 148,2, 149,1, 168,2, 171,1, 174,1, 185,2, 187,1, 191,1, -1
    dc.b 0,1, 9,1, 14,2, 24,2, 30,1, 34,2, 38,2, 41,1, 44,2, 46,3, 48,2, 52,4, 55,5, 59,9, 63,9, 66,12, 68,13, 71,15, 73,19, 74,22, 76,17, 78,16, 83,13, 89,11, 96,10, 100,9, 106,8, 111,8, 113,6, 116,7, 120,7, 123,6, 126,7, 129,5, 136,5, 140,2, 144,1, 146,2, 178,1, 191,1, -1
    dc.b 0,1, 5,1, 7,2, 12,1, 35,1, 36,2, 39,1, 42,1, 44,3, 47,2, 51,2, 55,3, 57,4, 61,3, 64,4, 70,4, 74,7, 75,8, 77,11, 78,16, 83,16, 84,15, 85,14, 87,12, 92,11, 97,10, 98,11, 101,13, 103,14, 105,13, 114,13, 116,11, 118,8, 124,4, 126,4, 128,5, 132,3, 134,3, 138,2, 139,2, 142,1, 158,1, 162,2, 177,1, 191,1, -1
    dc.b 0,1, 14,2, 37,2, 41,1, 43,2, 47,2, 49,3, 56,3, 64,6, 68,10, 70,13, 71,14, 75,14, 81,12, 84,10, 86,9, 92,6, 98,8, 99,11, 103,11, 106,10, 108,10, 110,11, 112,12, 114,12, 115,11, 118,9, 122,10, 124,8, 125,6, 129,5, 132,3, 140,3, 142,2, 150,1, 185,1, 186,2, 191,1, -1
    dc.b 0,1, 11,1, 19,2, 25,1, 30,1, 35,2, 39,2, 43,3, 47,2, 52,3, 53,5, 55,8, 58,9, 60,11, 64,15, 66,17, 73,15, 78,15, 81,16, 84,18, 87,20, 89,21, 91,22, 92,23, 94,23, 95,19, 97,15, 100,12, 102,10, 104,8, 106,7, 108,7, 109,9, 110,8, 112,9, 114,11, 117,9, 119,8, 121,7, 122,7, 124,8, 125,9, 126,9, 128,8, 130,6, 132,3, 135,4, 139,2, 187,2, 188,1, 191,2, -1
    dc.b 0,1, 24,1, 28,2, 32,1, 35,1, 39,2, 43,2, 46,3, 49,2, 54,4, 57,4, 61,6, 64,8, 66,11, 68,16, 69,19, 70,20, 72,20, 73,19, 76,17, 80,12, 83,10, 86,7, 89,7, 92,8, 97,6, 100,6, 104,7, 105,7, 108,8, 116,8, 119,9, 122,10, 124,8, 129,7, 132,7, 133,6, 136,4, 139,3, 144,2, 161,2, 162,1, 165,2, 170,2, 171,1, 173,2, 178,2, 183,1, 188,2, 189,1, 191,2, -1
    dc.b 0,1, 14,1, 21,2, 26,2, 31,1, 35,1, 37,2, 39,2, 41,3, 51,3, 53,4, 55,3, 58,4, 60,5, 61,6, 66,8, 70,15, 72,19, 74,23, 75,24, 76,26, 78,28, 79,24, 81,20, 83,15, 86,11, 91,10, 95,7, 99,8, 104,5, 124,5, 128,2, 130,3, 135,2, 138,2, 141,3, 143,2, 146,3, 148,2, 159,2, 162,1, 164,2, 170,1, 176,2, 179,1, 185,2, 191,2, -1
    dc.b 0,1, 7,1, 9,2, 17,2, 20,1, 41,1, 42,2, 43,1, 44,1, 45,2, 48,2, 49,3, 57,3, 58,2, 73,2, 76,4, 81,7, 87,11, 88,12, 89,17, 91,17, 94,15, 97,14, 98,13, 99,14, 101,16, 103,16, 104,14, 106,12, 108,11, 110,10, 112,8, 116,7, 117,6, 119,5, 124,5, 128,4, 129,4, 133,3, 143,3, 146,2, 161,2, 163,1, 165,2, 177,1, 186,1, 191,2, -1
    dc.b 0,1, 23,1, 28,2, 31,2, 34,1, 37,3, 40,2, 48,3, 54,4, 56,5, 62,5, 69,7, 73,11, 76,16, 78,20, 79,20, 81,22, 84,25, 85,24, 86,25, 90,18, 94,12, 97,11, 101,10, 104,10, 107,8, 115,6, 127,5, 131,6, 136,4, 138,2, 147,2, 150,1, 154,2, 157,2, 158,1, 163,2, 165,3, 166,2, 174,1, 176,2, 177,1, 180,2, 182,1, 184,2, 186,2, 189,1, 191,1, -1
    dc.b 0,2, 3,2, 6,1, 11,1, 14,2, 16,2, 24,1, 38,1, 39,2, 40,3, 44,2, 49,4, 54,4, 57,6, 60,8, 62,12, 63,14, 69,16, 71,19, 74,19, 78,17, 79,15, 80,12, 85,13, 86,16, 87,18, 90,15, 92,13, 98,11, 103,9, 109,10, 115,11, 118,12, 121,12, 124,11, 128,9, 132,9, 135,7, 140,4, 145,3, 150,2, 153,1, 156,1, 160,2, 191,1, -1
    dc.b 0,1, 4,1, 8,2, 12,2, 19,1, 25,1, 34,2, 43,2, 45,3, 53,3, 57,4, 60,6, 63,7, 64,9, 70,10, 75,15, 77,16, 87,16, 88,17, 94,17, 96,15, 98,13, 104,13, 109,9, 120,7, 130,5, 134,3, 135,4, 137,3, 144,3, 147,1, 148,2, 168,2, 172,1, 173,1, 176,2, 191,2, -1
    dc.b 0,1, 9,2, 16,1, 33,1, 43,2, 45,3, 47,2, 48,3, 58,5, 64,11, 70,13, 74,15, 76,18, 78,21, 79,23, 80,24, 81,26, 83,27, 86,24, 87,20, 89,18, 91,16, 95,16, 97,15, 100,13, 104,12, 107,13, 110,18, 112,21, 114,20, 116,18, 117,14, 121,9, 124,7, 130,2, 145,2, 149,1, 152,2, 160,2, 171,1, 181,1, 183,2, 186,1, 187,1, 191,2, -1
    dc.b 0,1, 11,1, 13,2, 17,2, 20,1, 22,3, 25,2, 32,2, 35,3, 36,3, 39,2, 42,3, 45,3, 49,4, 53,6, 54,7, 56,8, 58,8, 59,10, 62,13, 65,17, 66,17, 69,16, 72,11, 75,9, 78,8, 83,10, 86,11, 91,11, 95,10, 98,9, 101,7, 114,7, 117,8, 118,8, 122,7, 124,9, 126,9, 129,8, 132,5, 134,4, 137,3, 140,3, 144,2, 168,2, 170,1, 174,1, 175,2, 191,2, -1
    dc.b 0,1, 7,1, 11,2, 28,2, 31,1, 34,1, 39,2, 42,3, 51,3, 54,5, 58,10, 61,18, 63,24, 65,32, 66,37, 67,41, 68,41, 69,40, 71,34, 72,30, 74,24, 76,17, 78,15, 79,12, 82,10, 86,11, 87,13, 89,15, 93,16, 94,14, 97,11, 100,9, 104,7, 114,7, 117,8, 120,10, 123,11, 126,11, 130,8, 133,6, 137,5, 139,3, 143,3, 146,2, 160,2, 162,1, 165,2, 191,2, -1
    dc.b 0,1, 10,1, 12,2, 20,2, 22,3, 25,2, 39,2, 41,3, 42,2, 46,2, 48,3, 51,3, 53,3, 55,4, 58,5, 61,7, 64,9, 67,12, 68,13, 70,15, 71,18, 72,19, 73,20, 74,21, 76,18, 80,14, 83,11, 89,11, 91,10, 94,9, 96,10, 100,9, 104,9, 106,10, 108,11, 109,12, 113,8, 117,7, 121,5, 124,6, 129,6, 131,4, 137,4, 141,3, 144,3, 147,2, 165,2, 168,1, 170,2, 191,2, -1
    dc.b 0,1, 9,1, 11,2, 40,2, 44,3, 52,3, 55,4, 60,5, 64,5, 68,7, 70,10, 72,13, 74,15, 76,16, 78,23, 79,27, 80,29, 82,27, 85,21, 89,13, 91,12, 93,11, 96,11, 99,10, 101,10, 104,9, 111,5, 116,5, 125,4, 127,4, 132,3, 135,3, 140,2, 191,2, -1
    dc.b 0,1, 5,1, 7,2, 11,1, 16,2, 24,1, 33,1, 40,2, 46,2, 50,3, 54,4, 58,4, 62,6, 69,8, 74,10, 80,11, 82,13, 85,14, 92,12, 96,10, 100,10, 103,11, 106,12, 109,14, 110,15, 112,14, 115,13, 116,14, 117,16, 120,16, 121,15, 122,14, 124,10, 125,7, 128,5, 129,5, 131,4, 133,3, 140,3, 142,2, 148,2, 155,1, 158,1, 160,2, 184,2, 185,1, 187,2, 191,2, -1
    dc.b 0,1, 6,2, 42,2, 44,3, 50,3, 52,4, 54,6, 56,8, 57,11, 59,13, 60,15, 61,18, 62,20, 64,22, 67,20, 69,16, 71,14, 73,13, 76,13, 78,14, 80,13, 81,13, 83,14, 84,15, 86,17, 93,17, 95,16, 97,13, 100,11, 102,11, 104,10, 106,9, 107,8, 109,9, 112,8, 116,12, 117,12, 119,13, 120,15, 122,14, 123,13, 127,9, 130,7, 132,5, 135,5, 137,4, 139,3, 144,3, 147,2, 169,2, 171,1, 172,2, 178,2, 179,1, 181,2, 191,2, -1
    dc.b 0,1, 8,2, 33,2, 36,1, 39,3, 48,3, 52,4, 54,6, 58,9, 59,11, 63,11, 65,13, 71,13, 75,15, 78,14, 85,13, 91,11, 95,11, 99,9, 101,9, 103,10, 104,9, 105,10, 108,8, 112,8, 115,7, 118,9, 123,9, 124,7, 127,7, 130,5, 134,6, 138,3, 140,4, 143,3, 145,3, 147,2, 158,2, 160,1, 164,2, 191,2, -1
    dc.b 0,2, 6,2, 8,1, 9,2, 14,2, 18,1, 23,1, 25,2, 27,2, 28,1, 29,2, 43,2, 45,3, 47,2, 50,3, 55,3, 58,5, 67,8, 70,9, 75,14, 77,16, 80,18, 82,18, 85,17, 88,13, 91,10, 95,10, 96,9, 99,9, 100,8, 102,7, 103,8, 107,8, 110,7, 121,7, 125,5, 130,3, 135,2, 139,2, 141,3, 143,2, 145,3, 147,2, 191,2, -1
    dc.b 0,1, 3,1, 6,2, 9,1, 13,1, 15,2, 22,2, 24,3, 26,2, 37,2, 41,1, 44,3, 51,3, 55,4, 58,6, 62,8, 64,10, 73,10, 76,9, 80,12, 82,13, 83,15, 84,17, 85,19, 87,19, 89,17, 91,16, 93,13, 94,11, 96,11, 99,9, 102,8, 106,8, 109,9, 117,9, 118,10, 121,9, 124,7, 129,3, 137,3, 139,4, 141,3, 147,2, 149,3, 153,2, 155,2, 158,1, 159,3, 163,2, 191,2, -1
    dc.b 0,1, 5,2, 16,2, 18,1, 21,2, 26,1, 29,2, 35,2, 36,1, 38,2, 44,2, 47,3, 53,3, 57,5, 60,7, 63,10, 65,11, 67,13, 68,11, 69,11, 71,12, 72,13, 73,14, 76,14, 78,12, 80,13, 81,13, 87,12, 91,7, 94,6, 102,5, 108,5, 110,7, 113,8, 115,8, 118,9, 121,11, 128,11, 129,9, 135,6, 141,3, 146,3, 148,2, 150,3, 152,2, 178,2, 180,1, 182,2, 191,2, -1
    dc.b 0,2, 17,2, 24,1, 28,1, 30,3, 32,2, 35,2, 36,3, 37,3, 39,2, 41,3, 54,3, 56,5, 59,6, 60,8, 62,11, 68,11, 71,12, 72,12, 76,11, 80,9, 88,10, 93,10, 96,9, 100,9, 103,7, 108,5, 110,6, 113,6, 114,7, 117,8, 126,8, 129,7, 134,5, 136,4, 140,3, 143,3, 147,2, 148,3, 149,2, 152,2, 153,3, 155,2, 173,2, 175,1, 176,2, 191,2, -1
    dc.b 0,2, 13,2, 14,1, 16,2, 25,2, 29,1, 33,2, 48,2, 49,3, 54,3, 56,4, 58,4, 67,8, 73,10, 76,13, 78,15, 80,20, 82,22, 84,23, 86,22, 88,17, 91,13, 93,11, 96,9, 97,9, 99,8, 103,6, 115,6, 124,4, 127,3, 131,3, 135,2, 137,2, 139,3, 143,2, 191,2, -1
    dc.b 0,2, 31,2, 34,3, 43,3, 44,4, 47,3, 49,3, 50,4, 57,4, 59,6, 62,7, 65,9, 66,9, 68,11, 72,10, 77,9, 78,9, 80,10, 83,11, 84,12, 86,13, 89,13, 91,12, 93,10, 97,9, 100,8, 103,7, 105,7, 108,8, 110,10, 112,10, 114,11, 116,11, 118,13, 119,14, 122,12, 125,9, 129,6, 131,4, 134,4, 138,3, 140,3, 142,2, 150,2, 153,3, 154,2, 191,2, -1
    dc.b 0,2, 25,2, 27,1, 28,2, 30,3, 33,3, 35,2, 45,2, 47,3, 49,3, 53,4, 57,4, 61,7, 63,9, 66,10, 69,11, 83,8, 84,6, 89,5, 93,7, 121,7, 122,6, 124,7, 125,7, 127,5, 129,4, 132,3, 138,3, 141,2, 143,2, 145,2, 172,2, 174,3, 181,2, 191,2, -1
    dc.b 0,1, 3,1, 6,2, 13,2, 16,1, 25,1, 30,2, 40,2, 42,3, 45,2, 48,3, 51,4, 56,6, 59,8, 66,10, 76,12, 84,12, 88,13, 91,14, 95,10, 100,8, 109,6, 124,6, 132,4, 135,4, 141,2, 143,3, 145,2, 147,3, 150,3, 151,2, 156,2, 157,1, 161,2, 168,2, 170,1, 171,1, 173,2, 175,1, 180,1, 182,2, 189,1, 191,1, -1
    dc.b 0,2, 14,1, 21,2, 40,2, 43,3, 48,3, 53,4, 55,4, 57,5, 59,7, 62,9, 65,12, 71,18, 74,21, 77,22, 81,20, 84,19, 88,15, 92,13, 95,10, 98,11, 99,10, 103,10, 106,9, 116,7, 120,6, 125,4, 132,4, 139,3, 143,2, 191,2, -1
    dc.b 0,2, 10,2, 13,3, 18,2, 20,1, 21,2, 30,2, 34,1, 36,3, 38,2, 40,3, 41,3, 42,2, 43,3, 46,2, 49,3, 53,3, 56,4, 57,5, 61,10, 67,14, 78,14, 81,15, 84,16, 89,14, 93,12, 96,11, 97,11, 99,10, 104,11, 106,12, 108,14, 110,15, 115,15, 117,14, 119,12, 121,9, 122,8, 125,7, 130,4, 133,3, 144,3, 146,2, 156,2, 158,1, 161,2, 170,2, 171,3, 173,2, 191,2, -1
    dc.b 0,2, 13,2, 16,1, 19,1, 22,2, 37,2, 39,3, 49,3, 50,4, 52,4, 55,5, 58,7, 61,12, 63,17, 65,20, 67,22, 71,24, 75,20, 78,16, 82,13, 87,10, 89,10, 93,12, 95,10, 99,8, 103,6, 106,5, 112,5, 115,7, 118,7, 122,9, 125,9, 129,8, 132,5, 137,3, 147,3, 149,2, 168,2, 170,1, 173,2, 183,2, 184,3, 185,2, 188,2, 191,1, -1
    dc.b 0,2, 33,2, 41,3, 45,4, 48,4, 52,5, 54,6, 56,7, 58,7, 61,8, 62,10, 66,19, 68,24, 70,35, 71,37, 73,42, 74,41, 77,30, 79,22, 82,17, 84,13, 94,7, 97,8, 104,4, 113,4, 116,8, 120,11, 122,11, 127,8, 130,7, 133,6, 137,5, 139,4, 142,3, 145,3, 146,2, 147,3, 151,2, 191,2, -1
    dc.b 0,2, 6,2, 10,1, 14,2, 40,2, 43,3, 48,4, 56,4, 58,6, 59,6, 63,9, 65,10, 66,11, 69,14, 77,16, 81,16, 91,10, 97,8, 101,9, 110,8, 113,8, 122,6, 130,4, 136,4, 142,3, 148,2, 151,3, 155,3, 159,2, 163,2, 164,1, 171,2, 191,2, -1
    dc.b 0,1, 5,2, 19,2, 21,3, 24,2, 41,2, 47,3, 52,4, 55,4, 57,5, 64,5, 70,7, 73,10, 75,10, 83,9, 86,11, 88,12, 91,13, 94,12, 96,11, 100,11, 104,14, 107,14, 108,16, 110,17, 112,17, 114,14, 119,11, 124,8, 128,4, 133,3, 139,3, 142,2, 150,2, 152,3, 153,2, 155,3, 156,3, 164,2, 165,1, 167,2, 178,1, 179,1, 180,2, 185,2, 186,1, 188,2, 191,2, -1
    dc.b 0,2, 9,1, 19,2, 31,2, 35,1, 38,2, 41,2, 42,3, 45,3, 46,2, 50,4, 54,4, 57,6, 60,8, 62,11, 68,8, 72,7, 76,9, 78,11, 80,12, 83,12, 94,7, 100,4, 107,2, 113,2, 118,3, 122,5, 132,4, 136,3, 141,4, 146,2, 191,2, -1
    dc.b 0,2, 5,1, 7,2, 9,1, 10,2, 23,1, 32,2, 35,2, 36,3, 45,4, 46,3, 48,3, 53,5, 56,7, 59,9, 62,11, 63,13, 66,17, 69,22, 71,25, 73,25, 75,24, 78,22, 80,19, 85,13, 91,11, 95,11, 99,12, 104,10, 109,9, 115,9, 119,10, 124,10, 126,8, 128,9, 136,5, 141,3, 144,3, 147,2, 191,2, -1
    dc.b 0,2, 5,2, 8,1, 11,2, 17,2, 19,1, 21,2, 28,1, 30,2, 33,2, 34,3, 37,2, 39,3, 42,2, 47,2, 48,3, 52,4, 55,4, 56,5, 58,5, 63,6, 65,7, 66,7, 70,8, 77,6, 80,5, 82,5, 85,7, 99,7, 102,6, 104,5, 107,3, 109,3, 112,6, 114,6, 118,7, 123,7, 129,5, 132,3, 144,3, 147,2, 160,2, 163,3, 166,2, 167,1, 169,2, 175,2, 177,1, 178,3, 180,2, 191,2, -1
    dc.b 0,2, 5,2, 8,1, 11,2, 17,2, 21,1, 35,1, 37,3, 40,2, 43,3, 53,3, 56,4, 57,6, 72,11, 75,11, 78,14, 79,17, 81,22, 83,28, 84,31, 86,35, 87,37, 89,31, 90,25, 92,23, 93,21, 95,16, 97,14, 99,13, 104,13, 107,12, 109,13, 115,13, 117,14, 119,15, 121,13, 123,11, 125,8, 126,6, 130,5, 131,4, 134,3, 139,3, 143,2, 191,2, -1
    dc.b 0,2, 15,2, 16,1, 18,2, 22,2, 27,1, 31,1, 34,2, 36,2, 39,1, 41,3, 52,3, 55,5, 56,7, 58,9, 60,11, 62,16, 63,17, 64,20, 65,27, 67,31, 69,34, 71,30, 73,27, 74,24, 76,20, 84,18, 92,16, 95,13, 101,12, 105,12, 109,10, 113,8, 116,8, 119,9, 124,9, 126,11, 127,11, 128,10, 130,9, 131,7, 132,6, 135,6, 139,3, 143,2, 191,2, -1
    dc.b 0,2, 4,2, 9,1, 12,1, 14,2, 18,1, 28,1, 32,2, 39,2, 40,3, 42,3, 43,2, 45,2, 47,3, 49,3, 52,4, 54,5, 57,6, 59,7, 60,8, 65,9, 66,11, 68,12, 70,13, 72,14, 74,17, 75,18, 76,19, 77,19, 80,18, 82,17, 84,16, 89,16, 91,14, 93,13, 96,11, 100,10, 110,10, 114,11, 117,11, 119,10, 121,9, 123,9, 124,8, 128,9, 130,7, 132,8, 136,5, 140,4, 147,2, 169,1, 171,1, 173,2, 191,2, -1
    dc.b 0,1, 20,1, 22,2, 24,1, 25,2, 28,2, 34,1, 38,2, 41,2, 43,3, 51,3, 54,4, 57,4, 61,6, 65,8, 68,10, 69,11, 70,13, 73,13, 76,16, 77,17, 78,20, 80,21, 81,23, 84,23, 85,20, 87,19, 89,15, 90,14, 93,12, 95,12, 97,11, 99,10, 101,9, 108,10, 113,11, 118,10, 124,5, 129,4, 131,3, 133,2, 135,3, 141,2, 151,2, 154,1, 156,2, 161,2, 164,1, 170,1, 176,2, 181,1, 184,2, 185,1, 187,2, 191,1, -1
    dc.b 0,2, 10,2, 15,1, 19,2, 25,2, 29,1, 33,1, 34,2, 40,2, 43,3, 47,4, 53,4, 55,5, 58,7, 61,11, 62,13, 63,16, 66,17, 67,17, 69,14, 70,13, 71,12, 73,10, 76,8, 79,8, 81,10, 83,12, 84,14, 86,15, 87,17, 89,16, 91,16, 92,17, 94,15, 95,15, 96,13, 97,12, 99,12, 101,13, 102,16, 104,18, 105,19, 106,20, 107,20, 109,19, 110,19, 111,17, 117,17, 119,16, 120,13, 123,9, 126,8, 129,6, 131,5, 134,5, 137,3, 139,2, 146,2, 148,3, 151,2, 158,2, 161,1, 163,2, 191,2, -1
    dc.b 0,1, 5,1, 10,2, 11,2, 13,1, 27,1, 29,2, 42,2, 44,3, 52,3, 53,4, 56,4, 58,6, 59,8, 62,10, 63,11, 64,13, 66,15, 68,15, 70,13, 72,12, 77,10, 92,10, 97,8, 102,7, 105,6, 106,6, 109,8, 110,8, 112,10, 115,11, 117,11, 119,12, 121,11, 128,11, 130,9, 131,7, 135,7, 137,6, 139,4, 142,3, 147,2, 149,2, 151,1, 153,2, 191,2, -1
    dc.b 0,1, 6,2, 9,2, 13,1, 14,1, 15,2, 22,2, 26,1, 29,2, 37,2, 40,3, 43,3, 45,2, 47,3, 54,3, 57,4, 60,5, 62,6, 63,8, 65,10, 66,11, 67,12, 69,12, 70,11, 71,11, 73,13, 75,14, 77,15, 78,16, 80,14, 87,13, 91,11, 94,10, 95,9, 98,8, 100,8, 102,7, 106,8, 114,8, 116,7, 118,6, 119,6, 121,7, 122,7, 124,8, 125,7, 128,8, 130,8, 131,7, 132,6, 134,6, 136,4, 138,3, 145,3, 148,2, 191,2, -1
    dc.b 0,1, 5,2, 9,1, 11,2, 13,2, 17,1, 20,1, 23,2, 27,1, 31,1, 33,2, 38,2, 41,3, 44,2, 47,2, 49,3, 56,3, 58,4, 63,4, 67,7, 71,9, 73,11, 74,12, 76,13, 78,14, 79,14, 81,17, 83,18, 87,20, 89,17, 91,16, 92,14, 95,9, 98,7, 110,7, 113,6, 116,6, 119,5, 124,5, 127,4, 132,3, 135,3, 143,2, 163,2, 167,1, 169,1, 171,2, 178,2, 180,1, 182,1, 183,2, 191,2, -1
    dc.b 0,1, 30,1, 34,2, 46,2, 48,3, 51,3, 55,4, 57,5, 60,6, 61,9, 63,10, 73,10, 74,9, 80,9, 84,13, 86,15, 91,17, 92,17, 94,14, 96,14, 98,12, 100,10, 101,9, 104,8, 109,8, 112,7, 114,7, 117,6, 119,5, 124,6, 127,6, 130,4, 134,5, 135,5, 139,3, 146,3, 149,2, 158,2, 161,1, 164,2, 191,2, -1
    dc.b 0,1, 8,1, 11,2, 13,1, 21,1, 25,2, 27,2, 29,1, 30,1, 33,2, 35,1, 38,1, 40,2, 43,3, 48,3, 52,2, 56,3, 57,5, 59,7, 61,9, 63,9, 65,12, 66,14, 68,16, 69,16, 71,14, 73,11, 76,9, 79,9, 83,10, 87,10, 88,12, 90,13, 91,14, 93,15, 96,11, 97,10, 98,9, 100,9, 101,7, 107,7, 108,6, 110,6, 113,5, 115,6, 117,7, 119,7, 122,8, 124,9, 126,9, 129,8, 132,6, 134,5, 137,5, 142,3, 147,2, 188,2, 191,1, -1
    dc.b 0,1, 3,1, 5,2, 7,1, 22,1, 25,2, 28,2, 34,1, 36,1, 39,2, 49,2, 50,1, 53,3, 56,4, 58,3, 59,4, 62,4, 65,6, 67,8, 70,11, 71,13, 73,14, 75,16, 77,17, 80,15, 83,13, 85,12, 90,9, 94,7, 104,7, 108,6, 112,5, 132,5, 135,3, 139,3, 144,2, 156,2, 161,1, 163,2, 170,2, 172,1, 174,2, 180,2, 182,1, 184,2, 186,1, 189,2, 191,2, -1
    dc.b 0,1, 7,1, 8,2, 10,2, 13,1, 16,1, 18,2, 20,1, 29,1, 32,2, 34,2, 36,1, 40,2, 45,2, 48,3, 49,3, 51,2, 52,3, 55,3, 57,4, 60,5, 62,6, 64,6, 73,7, 76,9, 78,11, 81,11, 82,12, 84,13, 86,13, 89,11, 94,11, 96,10, 97,9, 102,9, 105,8, 106,9, 108,8, 110,8, 113,6, 115,6, 117,5, 128,5, 131,3, 137,3, 138,2, 156,2, 163,1, 171,1, 172,2, 175,2, 177,1, 178,2, 184,2, 186,1, 188,2, 191,2, -1
    rem
    dc.b 0,1, 2,1, 4,2, 6,1, 10,1, 12,2, 13,2, 15,1, 17,2, 18,2, 22,1, 36,1, 38,2, 42,2, 45,1, 47,2, 51,3, 55,4, 59,4, 61,6, 62,7, 64,8, 66,10, 68,12, 71,13, 76,12, 80,11, 83,8, 87,5, 97,5, 101,3, 104,3, 110,5, 114,5, 116,6, 120,5, 124,6, 127,6, 132,5, 140,2, 142,2, 144,3, 146,3, 149,2, 151,2, 153,3, 156,3, 157,2, 181,2, 183,1, 187,2, 191,2, -1
    dc.b 0,1, 5,1, 7,2, 10,1, 26,1, 28,2, 30,1, 32,2, 34,1, 38,1, 40,2, 41,1, 43,2, 46,2, 48,1, 50,2, 54,3, 57,4, 60,5, 62,6, 63,7, 65,9, 66,10, 67,10, 69,12, 70,14, 71,14, 73,17, 74,18, 75,19, 76,20, 77,18, 80,15, 81,13, 85,11, 89,11, 93,10, 97,9, 99,7, 101,8, 104,6, 116,6, 118,7, 120,5, 123,5, 124,6, 126,6, 131,4, 141,2, 153,2, 155,1, 159,1, 161,2, 163,2, 168,1, 171,2, 180,2, 182,1, 184,2, 185,1, 187,1, 188,2, 189,2, 191,1, -1
    dc.b 0,1, 38,1, 39,2, 40,2, 41,1, 43,1, 44,2, 47,1, 49,1, 51,2, 61,2, 63,3, 67,5, 69,6, 71,7, 72,9, 74,9, 75,10, 77,12, 78,14, 82,14, 83,16, 84,17, 85,17, 87,16, 89,15, 90,14, 91,14, 93,12, 94,10, 96,9, 99,9, 101,8, 102,7, 105,7, 107,6, 110,6, 112,5, 120,5, 123,4, 125,4, 127,3, 132,3, 134,2, 139,2, 141,1, 159,1, 176,2, 191,2, -1
    dc.b 0,1, 12,1, 15,0, 17,1, 37,1, 39,2, 40,1, 43,2, 44,2, 45,1, 47,2, 52,2, 55,3, 57,5, 59,7, 60,9, 61,10, 62,10, 63,11, 66,12, 67,11, 68,10, 69,10, 73,9, 81,8, 85,6, 87,5, 92,5, 95,4, 96,4, 97,3, 106,3, 108,4, 114,4, 115,5, 119,5, 121,6, 124,6, 127,5, 130,4, 132,4, 134,3, 137,2, 140,2, 144,1, 152,1, 153,2, 154,2, 155,1, 157,1, 158,2, 185,2, 186,1, 187,1, 188,2, 191,2, -1
    dc.b 0,1, 2,1, 4,0, 5,0, 6,1, 10,1, 12,0, 14,0, 16,1, 43,1, 44,2, 53,2, 55,3, 57,4, 59,4, 62,5, 63,5, 64,6, 66,7, 68,7, 69,8, 70,8, 72,9, 74,9, 78,12, 82,11, 88,8, 90,6, 92,6, 94,5, 95,4, 96,5, 99,4, 103,4, 106,5, 109,5, 112,4, 114,4, 116,5, 119,6, 121,7, 124,7, 125,5, 129,5, 130,4, 132,3, 133,4, 134,3, 137,2, 149,1, 156,1, 157,0, 158,1, 176,1, 179,2, 181,2, 183,1, 191,1, -1
    dc.b 0,1, 13,1, 14,2, 16,1, 41,1, 42,2, 43,2, 46,1, 52,1, 53,2, 57,2, 61,3, 63,5, 65,5, 66,6, 67,7, 68,8, 70,9, 72,8, 74,9, 76,12, 78,13, 80,16, 82,18, 83,18, 85,16, 86,13, 88,11, 91,7, 94,6, 101,5, 106,4, 110,4, 111,5, 112,5, 114,4, 116,5, 117,4, 121,4, 123,5, 124,5, 128,4, 131,3, 132,3, 133,4, 134,3, 135,4, 136,3, 137,3, 140,2, 144,1, 188,1, 191,2, -1
    dc.b 0,1, 46,1, 48,2, 51,2, 52,3, 53,2, 55,3, 57,3, 59,4, 60,5, 61,5, 67,7, 73,7, 80,8, 86,6, 91,4, 95,3, 100,2, 111,1, 115,2, 131,1, 168,1, 169,2, 172,1, 191,1, -1
    dc.b 0,1, 5,1, 6,2, 9,1, 17,1, 21,0, 23,1, 40,1, 42,2, 52,2, 54,3, 55,2, 57,3, 58,4, 59,5, 60,6, 61,7, 62,9, 63,9, 66,10, 72,10, 77,12, 82,13, 84,14, 86,15, 87,14, 89,13, 90,12, 93,11, 97,11, 98,12, 101,11, 107,11, 109,10, 112,8, 124,8, 126,7, 128,7, 130,6, 133,6, 137,4, 140,3, 143,2, 146,2, 148,1, 149,2, 150,1, 191,1, -1
    dc.b 0,1, 39,1, 42,0, 43,1, 47,1, 48,2, 54,2, 55,3, 59,3, 60,4, 65,6, 68,9, 73,13, 74,14, 76,16, 78,16, 81,15, 83,13, 86,12, 88,13, 94,13, 96,12, 99,11, 101,9, 103,9, 104,8, 110,7, 119,5, 124,4, 127,5, 129,5, 132,4, 135,3, 138,3, 140,2, 146,2, 148,1, 151,1, 155,2, 159,1, 181,1, 191,2, -1
    dc.b 0,1, 39,1, 41,2, 43,1, 45,1, 46,2, 54,2, 56,3, 57,4, 58,4, 66,6, 72,6, 78,11, 80,12, 81,15, 83,17, 84,16, 87,15, 88,14, 90,13, 93,11, 95,9, 102,9, 105,10, 107,11, 109,12, 111,13, 118,13, 119,11, 122,8, 124,6, 128,4, 131,2, 134,2, 136,1, 139,1, 140,2, 142,1, 191,1, -1
    dc.b 0,1, 6,1, 8,2, 10,2, 13,0, 15,1, 17,2, 19,1, 50,1, 52,2, 56,4, 59,6, 61,8, 63,10, 64,13, 66,14, 68,15, 71,18, 76,13, 79,10, 84,8, 87,7, 91,6, 94,5, 97,5, 101,4, 104,3, 107,4, 110,4, 114,5, 127,5, 130,4, 132,3, 135,2, 140,2, 141,1, 152,1, 153,2, 154,1, 191,1, -1
    dc.b 0,1, 32,1, 33,0, 35,1, 44,1, 47,2, 48,1, 51,2, 54,3, 56,5, 59,8, 60,9, 62,9, 67,8, 69,9, 74,17, 75,18, 76,18, 78,16, 80,13, 84,10, 85,9, 89,7, 96,7, 99,6, 102,6, 105,5, 107,5, 109,6, 111,5, 114,5, 116,6, 118,7, 122,6, 124,5, 127,6, 130,6, 134,4, 137,4, 140,3, 142,2, 144,2, 146,1, 161,1, 162,2, 163,2, 164,1, 191,1, -1
    dc.b 0,1, 47,1, 49,2, 56,2, 63,4, 72,3, 82,8, 84,9, 85,9, 87,8, 89,7, 94,5, 119,5, 124,4, 129,3, 133,3, 137,2, 146,2, 149,1, 159,2, 165,1, 191,1, -1
    dc.b 0,1, 15,1, 19,0, 21,1, 45,1, 46,2, 48,1, 51,1, 54,2, 57,4, 58,5, 60,7, 61,10, 62,12, 63,14, 64,16, 67,18, 69,19, 74,17, 78,12, 82,9, 84,8, 89,8, 91,10, 93,11, 95,11, 96,9, 98,7, 99,7, 102,6, 109,6, 110,8, 113,8, 115,9, 117,10, 118,11, 120,13, 123,12, 124,9, 126,7, 128,6, 130,4, 133,3, 137,2, 139,2, 141,1, 191,1, -1
    dc.b 0,1, 9,1, 11,2, 12,1, 45,1, 46,2, 49,2, 51,2, 54,2, 55,3, 57,3, 60,5, 62,8, 64,12, 66,19, 68,23, 69,28, 70,30, 71,30, 72,29, 73,28, 74,26, 75,24, 77,20, 78,18, 79,15, 80,12, 83,10, 89,8, 91,8, 94,7, 97,5, 104,5, 107,4, 111,4, 112,5, 114,6, 118,6, 120,7, 122,6, 124,6, 126,5, 131,5, 134,4, 136,4, 138,3, 140,2, 143,2, 145,1, 150,1, 152,2, 154,1, 191,1, -1
    dc.b 0,1, 5,1, 6,2, 8,1, 30,1, 32,2, 35,2, 36,1, 38,2, 39,2, 40,1, 43,1, 45,2, 47,2, 48,3, 50,2, 54,3, 56,3, 57,4, 58,5, 61,5, 62,7, 64,8, 65,10, 68,10, 71,11, 73,12, 75,15, 77,15, 78,16, 79,16, 81,16, 83,15, 86,11, 89,10, 90,9, 92,8, 93,7, 94,8, 96,7, 97,6, 99,6, 100,5, 112,5, 113,6, 115,6, 116,5, 130,5, 134,3, 139,2, 141,1, 160,1, 162,0, 165,0, 167,1, 191,1, -1
    dc.b 0,1, 8,1, 10,0, 12,0, 13,1, 26,1, 28,2, 30,1, 37,1, 39,0, 43,1, 45,2, 46,2, 48,1, 52,1, 53,2, 58,2, 64,3, 67,5, 73,8, 76,9, 80,10, 90,10, 93,9, 96,7, 100,6, 102,5, 107,5, 114,4, 126,4, 133,3, 135,2, 140,2, 142,1, 191,1, -1
    dc.b 0,1, 6,1, 9,0, 19,0, 23,1, 42,1, 43,2, 55,2, 58,4, 61,6, 62,8, 64,10, 65,13, 67,14, 68,14, 69,13, 70,13, 72,12, 73,13, 74,14, 76,14, 77,13, 78,12, 81,12, 82,11, 88,11, 89,12, 90,12, 92,10, 93,10, 95,8, 98,7, 101,6, 104,7, 110,5, 113,5, 115,6, 119,7, 121,8, 122,9, 123,10, 125,11, 126,13, 128,12, 129,10, 130,8, 131,6, 136,3, 138,3, 140,2, 143,2, 145,1, 147,1, 159,1, 162,0, 163,1, 191,1, -1
    erem
linedataend

*------ ERASE PIXEL LINE ------------------------------------------------------*

    even

    if soliderase

erase70
    or.b    d4,68*pwidth(a0)
erase69
    or.b    d4,67*pwidth(a0)
erase68
    or.b    d4,66*pwidth(a0)
erase67
    or.b    d4,65*pwidth(a0)
erase66
    or.b    d4,64*pwidth(a0)
erase65
    or.b    d4,63*pwidth(a0)
erase64
    or.b    d4,62*pwidth(a0)
erase63
    or.b    d4,61*pwidth(a0)
erase62
    or.b    d4,60*pwidth(a0)
erase61
    or.b    d4,59*pwidth(a0)
erase60
    or.b    d4,58*pwidth(a0)
erase59
    or.b    d4,57*pwidth(a0)
erase58
    or.b    d4,56*pwidth(a0)
erase57
    or.b    d4,55*pwidth(a0)
erase56
    or.b    d4,54*pwidth(a0)
erase55
    or.b    d4,53*pwidth(a0)
erase54
    or.b    d4,52*pwidth(a0)
erase53
    or.b    d4,51*pwidth(a0)
erase52
    or.b    d4,50*pwidth(a0)
erase51
    or.b    d4,49*pwidth(a0)
erase50
    or.b    d4,48*pwidth(a0)
erase49
    or.b    d4,47*pwidth(a0)
erase48
    or.b    d4,46*pwidth(a0)
erase47
    or.b    d4,45*pwidth(a0)
erase46
    or.b    d4,44*pwidth(a0)
erase45
    or.b    d4,43*pwidth(a0)
erase44
    or.b    d4,42*pwidth(a0)
erase43
    or.b    d4,41*pwidth(a0)
erase42
    or.b    d4,40*pwidth(a0)
erase41
    or.b    d4,39*pwidth(a0)
erase40
    or.b    d4,38*pwidth(a0)
erase39
    or.b    d4,37*pwidth(a0)
erase38
    or.b    d4,36*pwidth(a0)
erase37
    or.b    d4,35*pwidth(a0)
erase36
    or.b    d4,34*pwidth(a0)
erase35
    or.b    d4,33*pwidth(a0)
erase34
    or.b    d4,32*pwidth(a0)
erase33
    or.b    d4,31*pwidth(a0)
erase32
    or.b    d4,30*pwidth(a0)
erase31
    or.b    d4,29*pwidth(a0)
erase30
    or.b    d4,28*pwidth(a0)
erase29
    or.b    d4,27*pwidth(a0)
erase28
    or.b    d4,26*pwidth(a0)
erase27
    or.b    d4,25*pwidth(a0)
erase26
    or.b    d4,24*pwidth(a0)
erase25
    or.b    d4,23*pwidth(a0)
erase24
    or.b    d4,22*pwidth(a0)
erase23
    or.b    d4,21*pwidth(a0)
erase22
    or.b    d4,20*pwidth(a0)
erase21
    or.b    d4,19*pwidth(a0)
erase20
    or.b    d4,18*pwidth(a0)
erase19
    or.b    d4,17*pwidth(a0)
erase18
    or.b    d4,16*pwidth(a0)
erase17
    or.b    d4,15*pwidth(a0)
erase16
    or.b    d4,14*pwidth(a0)
erase15
    or.b    d4,13*pwidth(a0)
erase14
    or.b    d4,12*pwidth(a0)
erase13
    or.b    d4,11*pwidth(a0)
erase12
    or.b    d4,10*pwidth(a0)
erase11
    or.b    d4,9*pwidth(a0)
erase10
    or.b    d4,8*pwidth(a0)
erase9
    or.b    d4,7*pwidth(a0)
erase8
    or.b    d4,6*pwidth(a0)
erase7
    or.b    d4,5*pwidth(a0)
erase6
    or.b    d4,4*pwidth(a0)
erase5
    or.b    d4,3*pwidth(a0)
erase4
    or.b    d4,2*pwidth(a0)
erase3
    or.b    d4,pwidth(a0)
erase2
erase1
erase0
    rts
    
    else

erase70
    and.b   d4,68*pwidth(a0)
erase69
    and.b   d4,67*pwidth(a0)
erase68
    and.b   d4,66*pwidth(a0)
erase67
    and.b   d4,65*pwidth(a0)
erase66
    and.b   d4,64*pwidth(a0)
erase65
    and.b   d4,63*pwidth(a0)
erase64
    and.b   d4,62*pwidth(a0)
erase63
    and.b   d4,61*pwidth(a0)
erase62
    and.b   d4,60*pwidth(a0)
erase61
    and.b   d4,59*pwidth(a0)
erase60
    and.b   d4,58*pwidth(a0)
erase59
    and.b   d4,57*pwidth(a0)
erase58
    and.b   d4,56*pwidth(a0)
erase57
    and.b   d4,55*pwidth(a0)
erase56
    and.b   d4,54*pwidth(a0)
erase55
    and.b   d4,53*pwidth(a0)
erase54
    and.b   d4,52*pwidth(a0)
erase53
    and.b   d4,51*pwidth(a0)
erase52
    and.b   d4,50*pwidth(a0)
erase51
    and.b   d4,49*pwidth(a0)
erase50
    and.b   d3,48*pwidth(a0)
erase49
    and.b   d3,47*pwidth(a0)
erase48
    and.b   d3,46*pwidth(a0)
erase47
    and.b   d3,45*pwidth(a0)
erase46
    and.b   d3,44*pwidth(a0)
erase45
    and.b   d3,43*pwidth(a0)
erase44
    and.b   d3,42*pwidth(a0)
erase43
    and.b   d3,41*pwidth(a0)
erase42
    and.b   d3,40*pwidth(a0)
erase41
    and.b   d3,39*pwidth(a0)
erase40
    and.b   d3,38*pwidth(a0)
erase39
    and.b   d3,37*pwidth(a0)
erase38
    and.b   d3,36*pwidth(a0)
erase37
    and.b   d3,35*pwidth(a0)
erase36
    and.b   d3,34*pwidth(a0)
erase35
    and.b   d3,33*pwidth(a0)
erase34
    and.b   d3,32*pwidth(a0)
erase33
    and.b   d3,31*pwidth(a0)
erase32
    and.b   d3,30*pwidth(a0)
erase31
    and.b   d3,29*pwidth(a0)
erase30
    and.b   d3,28*pwidth(a0)
erase29
    and.b   d3,27*pwidth(a0)
erase28
    and.b   d3,26*pwidth(a0)
erase27
    and.b   d3,25*pwidth(a0)
erase26
    and.b   d3,24*pwidth(a0)
erase25
    and.b   d3,23*pwidth(a0)
erase24
    and.b   d3,22*pwidth(a0)
erase23
    and.b   d3,21*pwidth(a0)
erase22
    and.b   d3,20*pwidth(a0)
erase21
    and.b   d3,19*pwidth(a0)
erase20
    and.b   d3,18*pwidth(a0)
erase19
    and.b   d3,17*pwidth(a0)
erase18
    and.b   d3,16*pwidth(a0)
erase17
    and.b   d3,15*pwidth(a0)
erase16
    and.b   d3,14*pwidth(a0)
erase15
    and.b   d3,13*pwidth(a0)
erase14
    and.b   d3,12*pwidth(a0)
erase13
    and.b   d3,11*pwidth(a0)
erase12
    and.b   d3,10*pwidth(a0)
erase11
    and.b   d3,9*pwidth(a0)
erase10
    and.b   d3,8*pwidth(a0)
erase9
    and.b   d3,7*pwidth(a0)
erase8
    and.b   d3,6*pwidth(a0)
erase7
    and.b   d3,5*pwidth(a0)
erase6
    and.b   d3,4*pwidth(a0)
erase5
    and.b   d3,3*pwidth(a0)
erase4
    and.b   d3,2*pwidth(a0)
erase3
    and.b   d3,pwidth(a0)
erase2
erase1
erase0
    rts
    endif

*------ PLAYER ----------------------------------------------------------------*

play
    tst.b   vwait(a5)               ;
    beq     .donotwait              ;
    subq.b  #1,vwait(a5)            ;
    rts                             ;

.donotwait
    move.l  vcmdspointer(a5),a0     ;
.loop
    move.b  (a0)+,d0                ; cmd_eof?
    beq     .eof                    ;
    
    subq.b  #1,d0                   ; cmd_scroll?
    bne     .2                      ;
    move.b  (a0)+,vscroll(a5)       ;
    bra     .loop                   ;

.2  subq.b  #1,d0                   ; cmd_wait?
    bne     .3                      ;
    move.b  (a0)+,vwait(a5)         ;
    bra     .loop                   ;

.3  subq.b  #1,d0                   ; cmd_quit?
    bne     .4                      ;
    st      vquit(a5)               ;
    bra     .loop                   ;

.4  subq.b  #1,d0                   ; cmd_fullscreen?
    bne     .5                      ;
    move.b  (a0)+,vfullscreen(a5)   ;
    move.l  clistbase(pc),a1        ;
    move.w  #$ffff,gradient-clist(a1)   ; go fullscreen
    bra     .loop                   ;

.5  subq.b  #1,d0                   ; cmd_logo?
    bne     .6                      ;
    st      vinitlogo(a5)           ;
    move.l  clistbase(pc),a1        ; screen "off"
    move.w  #$0200,d1               ;
    move.w  d1,bplcon0-clist(a1)    ;
    move.w  d1,bplcon0text-clist(a1);

    lea     ease(pc),a1             ; init animation data
    move.l  a1,veasep(a5)           ;
    bra     .loop                   ;

.6  subq.b  #1,d0                   ; cmd_draw?
    bne     .7                      ;
    move.b  (a0)+,vdraw(a5)         ;
    bra     .loop                   ;

.7  subq.b  #1,d0                   ; cmd_autotext?
    bne     .8                      ;
    move.b  (a0)+,vautotext(a5)     ;
    bra     .loop                   ;

.8  subq.b  #1,d0                   ; cmd_give?
    bne     .9                      ;
    st      vgive(a5)               ;
    st      vwaiting(a5)            ; semaphore
    bra     .loop                   ;

.9  subq.b  #1,d0                   ; cmd_dynamic?
    bne     .10                     ;
    move.b  (a0)+,vdynamic(a5)      ;
    bne     .notpulsar              ;
    lea     linedatapulsar(pc),a1   ;
    move.l  a1,vlinedatapointer(a5) ;
.notpulsar
    bra     .loop                   ;

.10 subq.b  #1,d0                   ; cmd_bplen?
    bne     .11                     ;
    move.w  #SET+BPLEN,$96(a6)      ; -> no flickering (?) on startup
    bra     .loop                   ;

.11 subq.b  #1,d0                   ; cmd_color?
    bne     .12                     ;
    moveq   #0,d1                   ;
    move.b  (a0)+,d1                ;
    movem.w colordata(pc,d1.w),d2/d3;
    move.l  clistbase(pc),a1        ;
    add.w   #colors-clist,a1        ;
    move.w  d2,(a1)                 ;
    move.w  d3,4(a1)                ;
    move.w  d3,8(a1)                ;
    bra     .loop                   ;
    
.12 subq.b  #1,d0                   ; cmd_clearpeaks?
    bne     .loop                   ;
    moveq   #(138+192)/6-1,d7       ;
    lea     peaksff76+1(pc),a1      ;
.clearpeaks
    clr.b   (a1)                    ;
    addq.w  #6,a1                   ;
    dbf     d7,.clearpeaks          ;
    bra     .loop                   ;
        
.eof
    lea     playcmdsend(pc),a1      ;
    cmp.l   a1,a0                   ; end of play data?
    bne     .norestart              ;
    lea     playcmds(pc),a0         ; restart
.norestart
    move.l  a0,vcmdspointer(a5)     ;
    rts                             ;

cmd_eof                     equ 0
cmd_scroll                  equ 1
cmd_wait                    equ 2
cmd_quit                    equ 3
cmd_fullscreen              equ 4
cmd_logo                    equ 5
cmd_draw                    equ 6
cmd_autotext                equ 7
cmd_give                    equ 8
cmd_dynamic                 equ 9
cmd_bplen                   equ 10
cmd_color                   equ 11
cmd_clearpeaks              equ 12

colorsize   equ 4
colordata ; 14 fade out colors
    dc.w    $0000,$0000
    dc.w    $0111,$0111
    dc.w    $0111,$0222
    dc.w    $0111,$0333
    dc.w    $0222,$0444
    dc.w    $0222,$0555
    dc.w    $0222,$0666
    dc.w    $0333,$0777
    dc.w    $0333,$0888
    dc.w    $0333,$0999
    dc.w    $0333,$0aaa
    dc.w    $0444,$0bbb
    dc.w    $0444,$0ccc
    dc.w    $0444,$0ddd

playcmds
    dc.b    cmd_wait,75, cmd_eof
    dc.b    cmd_bplen, cmd_wait,200, cmd_eof
    
    dc.b    cmd_wait,25,    cmd_eof
    dc.b    cmd_scroll,1,   cmd_wait,200,   cmd_eof
    dc.b    cmd_give,   cmd_wait,80,    cmd_eof ; when routines
    dc.b    cmd_give,   cmd_wait,80,    cmd_eof ; bite hard
    dc.b    cmd_give,   cmd_wait,20,    cmd_eof ; empty

    dc.b    cmd_logo
    
    dc.b    cmd_scroll,0, cmd_wait,50,  cmd_eof
    
    dc.b    cmd_give, cmd_wait,80,  cmd_eof ; presents
    dc.b    cmd_give, cmd_wait,20,  cmd_eof ; monochrome
    
    dc.b    cmd_scroll,1,   cmd_wait,229,   cmd_eof
    dc.b    cmd_clearpeaks, cmd_wait,1, cmd_eof
    dc.b    cmd_dynamic,1,  cmd_scroll,1,   cmd_wait,30,    cmd_eof

    rept 21
    dc.b    cmd_autotext,1, cmd_wait,250,   cmd_eof
    endr

    dc.b    cmd_draw, 0, cmd_wait,250, cmd_eof
    dc.b    cmd_logo
    dc.b    cmd_scroll,0, cmd_draw,0, cmd_fullscreen,1,  cmd_dynamic,0,  cmd_wait,2,    cmd_eof
    dc.b    cmd_scroll,1, cmd_draw,1,  cmd_dynamic,0,  cmd_wait,200,    cmd_eof

    dc.b    cmd_autotext,0, cmd_wait,100,   cmd_eof

    dc.b    cmd_autotext,0, cmd_wait,250,   cmd_eof
    dc.b    cmd_autotext,0, cmd_wait,250,   cmd_eof
    dc.b    cmd_autotext,0, cmd_wait,250,   cmd_eof
    dc.b    cmd_autotext,0, cmd_wait,60,    cmd_eof

    dc.b    cmd_color,13*colorsize,cmd_eof
    dc.b    cmd_color,12*colorsize,cmd_eof
    dc.b    cmd_color,11*colorsize,cmd_eof
    dc.b    cmd_color,10*colorsize,cmd_eof
    dc.b    cmd_color,9*colorsize,cmd_eof
    dc.b    cmd_color,8*colorsize,cmd_eof
    dc.b    cmd_color,7*colorsize,cmd_eof
    dc.b    cmd_color,6*colorsize,cmd_eof
    dc.b    cmd_color,5*colorsize,cmd_eof
    dc.b    cmd_color,4*colorsize,cmd_eof
    dc.b    cmd_color,3*colorsize,cmd_eof
    dc.b    cmd_color,2*colorsize,cmd_eof
    dc.b    cmd_color,1*colorsize,cmd_eof
    dc.b    cmd_color,0*colorsize,cmd_eof

    dc.b    cmd_autotext,0, cmd_wait,25,    cmd_eof

    dc.b    cmd_quit, cmd_eof
    dc.b    cmd_wait,100, cmd_eof ; never reached
playcmdsend

*------ INIT SPRITE POINTERS --------------------------------------------------*

    even
initspritepointers1
    move.l  clistbase(pc),a1        ;
    move.l  spritedatabase(pc),d0   ; sprite data
    add.l   #spriteblankdata-spritedata,d0  ;
    lea     spritepointers1-clist(a1),a0    ;
    
    move.l  d0,d1                   ;
    swap    d1                      ;
    moveq   #8-1,d7                 ; init the 8 sprites
.insp
    move.w  d1,(a0)                 ;
    move.w  d0,4(a0)                ;
    addq.w  #8,a0                   ;
    dbf     d7,.insp                ;
    rts                             ;

initspritepointers2
    move.l  clist2base(pc),a1               ;
    move.l  spritedatabase(pc),d0           ; sprite data
    lea     spritepointers2-clist2(a1),a0   ;
    swap    d0                              ; sprite0
    move.w  d0,(a0)
    swap    d0
    move.w  d0,4(a0)
    add.l   #sprite1data-sprite0data,d0     ; sprite1
    swap    d0
    move.w  d0,8(a0)
    swap    d0
    move.w  d0,12(a0)
    add.l   #sprite2data-sprite1data,d0     ; sprite2
    swap    d0
    move.w  d0,16(a0)
    swap    d0
    move.w  d0,20(a0)
    add.l   #sprite3data-sprite2data,d0     ; sprite3
    swap    d0
    move.w  d0,24(a0)
    swap    d0
    move.w  d0,28(a0)
    add.l   #sprite4data-sprite3data,d0     ; sprite4
    swap    d0
    move.w  d0,32(a0)
    swap    d0
    move.w  d0,36(a0)
    add.l   #sprite5data-sprite4data,d0     ; sprite5
    swap    d0
    move.w  d0,40(a0)
    swap    d0
    move.w  d0,44(a0)
    add.l   #sprite6data-sprite5data,d0     ; sprite6
    swap    d0
    move.w  d0,48(a0)
    swap    d0
    move.w  d0,52(a0)
    add.l   #sprite7data-sprite6data,d0     ; sprite7
    swap    d0
    move.w  d0,56(a0)
    swap    d0
    move.w  d0,60(a0)
    rts

*------ SCROLL ----------------------------------------------------------------*

scroll
    move.l  vbitplane2(a5),a0       ;
    addq.w  #8,a0                   ;
    lea     1*pwidth(a0),a1         ; scroll 1 line
    moveq   #-1,d1                  ;
    
    move.w  #(239<<6)+((pwidth-16)/2),d2; bltsize and start  16 = modulo
    tst.b   vfullscreen(a5)         ;
    beq     .no                     ;
    move.w  #(257<<6)+((pwidth-16)/2),d2; 257 (pheight+1) is ok (there's padding)
.no
    bsr     waitblitter             ;
    move.l  a1,$50(a6)              ; source A
    move.l  a0,$54(a6)              ; destination D
    move.l  #$00100010,$64(a6)      ; modulo A (16) and D (16)
    move.l  d1,$44(a6)              ; first and last word mask for source A
    move.l  #$09f00000,$40(a6)      ; 0: No A shift, 9: USEA and USED, f0: D = A
    move.w  d2,$58(a6)              ; bltsize and start
    rts                             ;

*------ COPY ------------------------------------------------------------------*

copy
    movem.l vbitplane1(a5),a0/a1    ;
    addq.w  #8,a0                   ;
    addq.w  #8,a1                   ;
    moveq   #-1,d1                  ;
    
    move.w  #(239<<6)+((pwidth-16)/2),d2; bltsize and start  16 = modulo
    tst.b   vfullscreen(a5)         ;
    beq     .no                     ;
    move.w  #(256<<6)+((pwidth-16)/2),d2; bltsize and start  16 = modulo  full screen pulsar
.no
    bsr     waitblitter             ;
    move.l  a0,$50(a6)              ; source A
    move.l  a1,$54(a6)              ; destination D
    move.l  #$00100010,$64(a6)      ; modulo A (16) and D (16)
    move.l  d1,$44(a6)              ; first and last word mask for source A
    move.l  #$09f00000,$40(a6)      ; 0: No A shift, 9: USEA and USED, f0: D = A
    move.w  d2,$58(a6)              ; bltsize and start
    rts                             ;

*------ COPPER INSTRUCTION LIST 1919 ------------------------------------------*

clist
    dc.w    $008e,$2c81 ; DIWSTRT
    dc.w    $0090,$2cc1 ; DIWSTOP
    dc.w    $0092,$0038 ;
    dc.w    $0094,$00d0 ;
    dc.w    $0100
bplcon0
    dc.w    $2200       ; 2 bitplanes
    dc.w    $0102,$0010 ; "anti-aliasing": shift uneven bitplanes 1 pixel
    dc.w    $0104,$0000 ; sprites behind bitplanes
    dc.w    $0108,$0000
    dc.w    $010a,$0000
    dc.w    $0180,$0000,$0182
colors
    dc.w    $0444,$0184,$0eee,$0186,$0eee

    dc.w    $0120
spritepointers1
    dc.w    0
    dc.w    $0122,0
    dc.w    $0124,0
    dc.w    $0126,0
    dc.w    $0128,0
    dc.w    $012a,0
    dc.w    $012c,0
    dc.w    $012e,0
    dc.w    $0130,0
    dc.w    $0132,0
    dc.w    $0134,0
    dc.w    $0136,0
    dc.w    $0138,0
    dc.w    $013a,0
    dc.w    $013c,0
    dc.w    $013e,0

    dc.w    $01a0,$0000,$01a2,$0000,$01a4,$0000,$01a6,$0000 ; sprite 0 and 1 color
    dc.w    $01a8,$0000,$01aa,$0000,$01ac,$0000,$01ae,$0000 ; sprite 2 and 3 color
    dc.w    $01b0,$0000,$01b2,$0000,$01b4,$0000,$01b6,$0000 ; sprite 4 and 5 color
    dc.w    $01b8,$0000,$01ba,$0000,$01bc,$0000,$01be,$0000 ; sprite 6 and 7 color

    dc.w    $10<<8+$07,$fffe
    dc.w    $009c,$8010 ; set coper interrupt bit
    dc.w    ($10+11)<<8+$07,$fffe
lspdmacon
    dc.w    $0096,$8000 ; be sure DMACon word is $8000

gradient
    dc.w    $2c07,$fffe,$0182,$0111,$0184,$0111,$0186,$0111
    dc.w    $2d07,$fffe,$0182,$0111,$0184,$0222,$0186,$0222
    dc.w    $2e07,$fffe,$0182,$0111,$0184,$0333,$0186,$0333
    dc.w    $2f07,$fffe,$0182,$0222,$0184,$0444,$0186,$0444
    dc.w    $3007,$fffe,$0182,$0222,$0184,$0555,$0186,$0555
    dc.w    $3107,$fffe,$0182,$0222,$0184,$0666,$0186,$0666
    dc.w    $3207,$fffe,$0182,$0333,$0184,$0777,$0186,$0777
    dc.w    $3307,$fffe,$0182,$0333,$0184,$0888,$0186,$0888
    dc.w    $3407,$fffe,$0182,$0333,$0184,$0999,$0186,$0999
    dc.w    $3507,$fffe,$0182,$0333,$0184,$0aaa,$0186,$0aaa
    dc.w    $3607,$fffe,$0182,$0444,$0184,$0bbb,$0186,$0bbb
    dc.w    $3707,$fffe,$0182,$0444,$0184,$0ccc,$0186,$0ccc
    dc.w    $3807,$fffe,$0182,$0444,$0184,$0ddd,$0186,$0ddd
    dc.w    $3907,$fffe,$0182,$0444,$0184,$0eee,$0186,$0eee

    dc.w    $ffe1,$fffe
    
    dc.w    $1c07,$fffe
    dc.w    $0100
bplcon0text
    dc.w    $1200   ; 1 bitplane (text)
    dc.w    $0182,$0fff
    dc.w    $00e0
textplanep
    dc.w    0,$00e2,0

    dc.w    $ffff,$fffe
clistend

*------ SPRITE DATA -----------------------------------------------------------*

spritedata
sprite0data ; s
    dc.w $7090,$7f00 ; height 15
    rept 15
    dc.w %1111110000000000,$0000
    endr
    dc.w $0000,$0000

sprite1data ; p
    dc.w $7098,$8b00 ; height 27
    rept 27
    dc.w %1111110000000000,$0000
    endr
    dc.w $0000,$0000

sprite2data ; e
    dc.w $70a0,$7f00 ; height 15
    rept 15
    dc.w %1111110000000000,$0000
    endr
    dc.w $0000,$0000

sprite3data ; a
    dc.w $70a8,$8b00 ; height 27
    rept 27
    dc.w %1111110000000000,$0000
    endr
    dc.w $0000,$0000

sprite4data ; d
    dc.w $70b0,$9700 ; height 39
    rept 39
    dc.w %1111110000000000,$0000
    endr
    dc.w $0000,$0000

sprite5data ; p
    dc.w $b090,$cb00 ; height 27
    rept 27
    dc.w %1111110000000000,$0000
    endr
    dc.w $0000,$0000

sprite6data ; o
    dc.w $b098,$cb00 ; height 27
    rept 27
    dc.w %1111110000000000,$0000
    endr
    dc.w $0000,$0000

sprite7data ; n
    dc.w $b0a0,$cb00 ; height 27
    rept 27
    dc.w %1111110000000000,$0000
    endr
    dc.w $0000,$0000

spriteblankdata
    dc.w $1905,$1a00    ; 1px high sprite in the top-leftmost valid position
    dc.w $0000,$0000    ; blank pixel data
    dc.w $0000,$0000    ; end of sprite
spritedataend

*------ COPPER INSTRUCTION LIST LOGO ------------------------------------------*

clist2
    dc.w    $008e,$2c81 ; DIWSTRT
    dc.w    $0090,$2cc1 ; DIWSTOP
    dc.w    $0092,$0038 ;
    dc.w    $0094,$00d0 ;
    
    dc.w    $0100,$3200 ; 3 bitplanes

; http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0159.html
    dc.w    $0104,$0000 ; sprites behind bitplanes

    dc.w    $0108,$0000 ; modulo odd bitplanes
    dc.w    $010a,$0028 ; modulo even bitplanes (west)

    dc.w    $0120
spritepointers2
    dc.w    0
    dc.w    $0122,0
    dc.w    $0124,0
    dc.w    $0126,0
    dc.w    $0128,0
    dc.w    $012a,0
    dc.w    $012c,0
    dc.w    $012e,0
    dc.w    $0130,0
    dc.w    $0132,0
    dc.w    $0134,0
    dc.w    $0136,0
    dc.w    $0138,0
    dc.w    $013a,0
    dc.w    $013c,0
    dc.w    $013e,0

    dc.w    $0180,$0000

    dc.w    $01a0,$0000,$01a4,$0000,$01a6,$0000 ; some sprite 0 and 1 colors
    dc.w    $01a8,$0000,$01ac,$0000,$01ae,$0000 ; some sprite 2 and 3 colors
    dc.w    $01b0,$0000,$01b4,$0000,$01b6,$0000 ; some sprite 4 and 5 colors
    dc.w    $01b8,$0000,$01bc,$0000,$01be,$0000 ; some sprite 6 and 7 colors

    dc.w    $10<<8+$07,$fffe
    dc.w    $009c,$8010 ; set coper interrupt bit
    dc.w    ($10+11)<<8+$07,$fffe
lspdmacon2
    dc.w    $0096,$8000 ; be sure DMACon word is $8000
    
    dc.w    $ffff,$fffe
clist2end

*------ UPDATE SPRITES --------------------------------------------------------*

; d0: x, d1: y
updatesprites
    move.l  spritedatabase(pc),a0   ; = sprite0data
    move.l  a0,a1                   ; s
    moveq   #5*3,d2                 ; height
    bsr     setspritepos            ;

    lea     sprite1data-sprite0data(a0),a1  ; p
    add.w   #16*3,d0
    sub.w   #4*3,d1
    moveq   #9*3,d2
    bsr     setspritepos
    
    lea     sprite2data-sprite0data(a0),a1  ; e
    add.w   #2*16*3,d0
    moveq   #5*3,d2
    bsr     setspritepos

    lea     sprite3data-sprite0data(a0),a1  ; a
    add.w   #16*3,d0
    moveq   #9*3,d2
    bsr     setspritepos

    lea     sprite4data-sprite0data(a0),a1  ; d
    add.w   #16*3,d0
    sub.w   #4*3,d1
    moveq   #13*3,d2
    bsr     setspritepos

    ; line 2
    lea     sprite5data-sprite0data(a0),a1  ; p
    sub.w   #4*16*3+6,d0
    add.w   #22*3,d1
    moveq   #9*3,d2
    bsr     setspritepos

    lea     sprite6data-sprite0data(a0),a1  ; o
    add.w   #16*3,d0
    bsr     setspritepos

    lea     sprite7data-sprite0data(a0),a1  ; n
    add.w   #20*3,d0
    bsr     setspritepos
    rts

; VSTART    HSTART
; xxxxxxxx  xxxxxxxx
; VSTOP     MISC/CTRL
; xxxxxxxx  A----xxx
;                |||
;                VSTART high bit
;                 ||
;                 VSTOP high bit
;                  |
;                  HSTART low bit

; must not alter d0 and d1
setspritepos
    move.l  d0,d3               ; process x
    move.l  #450,d5             ;
    cmp.l   d5,d3               ; outside of visible area?
    bcs     .lower              ;
    move.l  d5,d3               ;
.lower
    moveq   #0,d5               ; init CTRL
    asr.w   #1,d3               ;
    bvc     .2                  ;
    moveq   #1,d5               ; set HSTART low bit
.2
    move.b  d3,1(a1)            ; write HSTART
    move.l  d1,d3               ; process y
    move.b  d3,(a1)             ; write VSTART
    clr.b   d3                  ;
    tst.w   d3                  ;
    beq     .3                  ;
    addq.b  #4,d5               ; set VSTART high bit
.3
    move.l  d1,d3               ; process height VSTOP
    add.w   d2,d3               ;
    move.b  d3,2(a1)            ; write VSTOP
    clr.b   d3                  ;
    tst.w   d3                  ;
    beq     .4                  ;
    addq.b  #2,d5               ; set VSTOP high bit
.4
    move.b  d5,3(a1)            ; write CTRL
    rts

*------ MEMORY MANAGEMENT -----------------------------------------------------*

BESTMEMORY          equ 0
MEMF_CHIP           equ 1<<1
MEMF_CLEAR          equ 1<<16

clistsize           equ clistend-clist
clist2size          equ clist2end-clist2
lspbanksize         equ lspbankend-lspbank
spritedatasize      equ spritedataend-spritedata

memtable
clistbase           dc.l    0,MEMF_CHIP,clistsize
clist2base          dc.l    0,MEMF_CHIP,clist2size
lspbankbase         dc.l    0,MEMF_CHIP,lspbanksize
spritedatabase      dc.l    0,MEMF_CHIP,spritedatasize

memtable2
bitplane1base       dc.l    0,MEMF_CHIP+MEMF_CLEAR,2*psize
bitplane2base       dc.l    0,MEMF_CHIP+MEMF_CLEAR,2*psize
textplanebase       dc.l    0,MEMF_CHIP+MEMF_CLEAR,numtbuffers*tsize
logowestbase        dc.l    0,MEMF_CHIP+MEMF_CLEAR,logowestsize
memtableend

entrysize   equ 12 ; one entry in the memtable is 12 bytes large (3 longwords)
entries     equ (memtableend-memtable)/entrysize
entrieschip equ (memtable2-memtable)/entrysize

alloc
    lea     clist(pc),a1            ;
    move.l  AbsExecBase.w,a6        ;
    jsr     TypeOfMem(a6)           ;
    btst    #1,d0                   ; chipmem?
    beq     .notchipmem             ;

    lea     clist(pc),a0            ; mark data that is in chipmen already
    lea     clistbase(pc),a1        ;
    move.l  a0,(a1)                 ;
        
    lea     clist2(pc),a0           ;
    lea     clist2base(pc),a1       ;
    move.l  a0,(a1)                 ;

    lea     base(pc),a0             ;
    add.l   #lspbank-base,a0        ;
    lea     lspbankbase(pc),a1      ;
    move.l  a0,(a1)                 ;

    lea     spritedata(pc),a0       ;
    lea     spritedatabase(pc),a1   ;
    move.l  a0,(a1)                 ;

.notchipmem
    lea     memtable(pc),a5         ;
    moveq   #entries-1,d7           ;
.2
    tst.l   (a5)                    ; not to be allocated?
    bne     .3                      ;
    move.l  8(a5),d0                ; bytesize
    move.l  4(a5),d1                ; requirements
    move.l  AbsExecBase.w,a6        ;
    jsr     AllocMem(a6)            ; allocmem
    tst.l   d0                      ; out of memory?
    beq     .printerrorandfreemem   ;
    move.l  d0,(a5)                 ;
.3
    add.w   #entrysize,a5           ; next entry
    dbf     d7,.2                   ;
    bsr     initmemory              ;
    moveq   #0,d0                   ; ok, all entries allocated
    rts                             ;

.printerrorandfreemem
    bsr     printoutofmemory        ;
dealloc
    move.l  AbsExecBase.w,a6        ;
    jsr     TypeOfMem(a6)           ;
    lea     memtable(pc),a5         ;
    moveq   #entries-1,d7           ;
    btst    #1,d0                   ; chipmem?
    beq     .free2                  ; we are not in chipmem so free all entries
    lea     memtable2(pc),a5        ;
    moveq   #entries-entrieschip-1,d7;
.free2
    tst.l   (a5)                    ; end of memtable?
    beq     .free3                  ;
    move.l  (a5),a1                 ; address of memory block
    move.l  8(a5),d0                ; bytesize
    move.l  AbsExecBase.w,a6        ;
    jsr     FreeMem(a6)             ;
    add.w   #entrysize,a5           ;
    dbf     d7,.free2               ;
.free3
    moveq   #-1,d0                  ; alloc error
    rts                             ;

initmemory
; copy copper list to chip memory
    lea     clist(pc),a0            ;
    move.l  clistbase(pc),a1        ;
    move.w  #clistsize-1,d0         ;
.copyclist
    move.b  (a0)+,(a1)+             ;
    dbf     d0,.copyclist           ;

; copy copper list 2 to chip memory
    lea     clist2(pc),a0           ;
    move.l  clist2base(pc),a1       ;
    move.w  #clist2size-1,d0        ;
.copyclist2
    move.b  (a0)+,(a1)+             ;
    dbf     d0,.copyclist2          ;

; copy sprite data
    lea     spritedata(pc),a0       ;
    move.l  spritedatabase(pc),a1   ;
    move.w  #spritedatasize-1,d0    ;
.copyspr
    move.b  (a0)+,(a1)+             ;
    dbf     d0,.copyspr             ;

; init sprites
    bsr     initspritepointers1     ;
    bsr     initspritepointers2     ;

; textplane
    move.l  textplanebase(pc),d0    ;
    move.l  clistbase(pc),a1        ;
    add.w   #textplanep-clist,a1    ;
    move.w  d0,4(a1)                ;
    swap    d0                      ;
    move.w  d0,(a1)                 ;

; bitplane test pattern
    if testpattern
    
    move.l  bitplane1base(pc),a1    ;
    move.w  #2*psize-1,d0           ;
.tp1
    move.b  #%10101010,(a1)+        ;
    dbf     d0,.tp1                 ;
    move.w  #pwidth-1,d0            ; special bottom pattern
.tp11
    move.b  #%11111101,-(a1)        ;
    dbf     d0,.tp11                ;

    move.l  bitplane2base(pc),a1    ;
    move.w  #2*psize-1,d0           ;
.tp2
    move.b  #%10101010,(a1)+        ;
    dbf     d0,.tp2                 ;
    move.w  #pwidth-1,d0            ; special bottom pattern
.tp22
    move.b  #%11111101,-(a1)        ;
    dbf     d0,.tp22                ;

    move.l  logowestbase(pc),a1     ;
    move.w  #logowestsize-1,d0      ;
.tp3
    move.b  #%11101010,(a1)+        ;
    dbf     d0,.tp3                 ;
    move.w  #lwidth-1,d0            ; special bottom pattern
.tp33
    move.b  #%11001100,-(a1)        ;
    dbf     d0,.tp33                ;
    endif

; init punks
    lea     base(pc),a1             ;
    move.l  a1,d1                   ;
    lea     punks(pc),a0            ;
    move.w  #(punksend-punks)/4-1,d0        ;
.ploop
    add.l   d1,(a0)+                ;
    dbf     d0,.ploop               ;

; init dynamic line
    lea     active(pc),a0           ;
    moveq   #31,d0                  ; x
    moveq   #64-1,d7                ;
.idl
    move.b  d0,(a0)+                ; x
    clr.b   (a0)+                   ; y
    addq.b  #2,d0                   ; x += 2 (1 gets too slow)
    dbf     d7,.idl                 ;

; init lsp bank
    lea     base(pc),a0             ; copy lspbank
    add.l   #lspbank-base,a0        ;
    move.l  lspbankbase(pc),a1      ;
    move.l  #lspbanksize,d0         ;
.copylspbank
    move.b  (a0)+,(a1)+             ;
    subq.l  #1,d0                   ;
    bne     .copylspbank            ;
    rts                             ;

printoutofmemory
    moveq   #0,d0                   ;
    lea     dos(pc),a1              ;
    move.l  AbsExecBase.w,a6        ;
    jsr     OpenLibrary(a6)         ;
    move.l  d0,a6                   ;
    beq     .error                  ;
    jsr     Output(a6)              ;
    move.l  d0,d1                   ;
    beq     .error                  ;
    move.l  #errmsgend-errmsg,d3    ; length
    lea     errmsg(pc),a1           ;
    move.l  a1,d2                   ;
    jsr     Write(a6)               ;
    tst.l   d0                      ;
    beq     .error                  ;
    move.l  a6,a1                   ;
    move.l  AbsExecBase.w,a6        ;
    jsr     CloseLibrary(a6)        ;
.error
    moveq   #0,d0                   ;
    rts                             ;

dos dc.b    "dos.library",0

errmsg
    dc.b    "Error: Could not allocate enough memory",10
errmsgend

;*****************************************************************
;
;   Light Speed Player v1.05 (modified by Depeche)
;   Fastest Amiga MOD player ever :)
;   Written By Arnaud Carré (aka Leonard / OXYGENE)
;   https://github.com/arnaud-carre/LSPlayer
;   twitter: @leonard_coder
;
;   --------How to use---------
;
;   bsr LSP_MusicDriver+0 : Init LSP player code
;       In: a0: LSP music data(any memory)
;           a1: LSP sound bank(chip memory)
;           a2: DMACON 8bits byte address (should be odd address!)
;      Out: a0: music BPM pointer (16bits)
;           d0: music len in tick count
;
;   bsr LSP_MusicDriver+4 : LSP player tick (call once per frame)
;       In: a6: must be $dff000
;           Scratched regs: d0/d1/d2/a0/a1/a2/a3/a4/a5
;      Out: None
;
;*****************************************************************

    even

lspplay     lea     LSPVars(pc),a1
            move.l  (a1),a0                 ; byte stream
process     moveq   #0,d0
cloop       move.b  (a0)+,d0
            bne     swCode
            addi.w  #$0100,d0
            bra     cloop
swCode      add.w   d0,d0
            move.l  m_codeTableAddr(a1),a2  ; code table
            move.w  (a2,d0.w),d0            ; code
            beq     noInst
            cmp.w   m_escCodeRewind(a1),d0
            beq     r_rewind
            cmp.w   m_escCodeSetBpm(a1),d0
            beq     r_chgbpm

            add.b   d0,d0                   ; volume
            bcc     noVd
            move.b  (a0)+,$d9(a6)
noVd        add.b   d0,d0
            bcc     noVc
            move.b  (a0)+,$c9(a6)
noVc        add.b   d0,d0
            bcc     noVb
            move.b  (a0)+,$b9(a6)
noVb        add.b   d0,d0
            bcc     noVa
            move.b  (a0)+,$a9(a6)
noVa
            move.l  a0,(a1)+                ; store byte stream ptr
            move.l  (a1),a0                 ; word stream

            tst.b   d0                      ; period (rate)
            beq     noPa
            add.b   d0,d0
            bcc     noPd
            move.w  (a0)+,$d6(a6)
noPd        add.b   d0,d0
            bcc     noPc
            move.w  (a0)+,$c6(a6)
noPc        add.b   d0,d0
            bcc     noPb
            move.w  (a0)+,$b6(a6)
noPb        add.b   d0,d0
            bcc     noPa
            move.w  (a0)+,$a6(a6)
noPa
            tst.w   d0
            beq     noInst

            moveq   #0,d1
            move.l  m_lspInstruments-4(a1),a2   ; instrument table
            lea     resetv+12(pc),a4

            lea     $d0(a6),a5
            moveq   #4-1,d2                 ; 4 channels
vloop       add.w   d0,d0
            bcs     setIns
            add.w   d0,d0
            bcc     skip
            move.l  (a4),a3
            move.l  (a3)+,(a5)
            move.w  (a3)+,4(a5)
            bra     skip
setIns      move.w  (a0)+,d3                ; d3 is unused here
            add.w   d3,a2                   ;
            lea     vars(pc),a3             ; a3 is unused here

            if instruments
            cmp.w   #3,d2                   ;
            bne     .i2                     ;
            move.w  d3,vinst3(a3)           ;
            bra     .done                   ;
.i2         cmp.w   #2,d2                   ;
            bne     .i1                     ;
            move.w  d3,vinst2(a3)           ;
            bra     .done                   ;
.i1         cmp.w   #1,d2                   ;
            bne     .i0                     ;
            move.w  d3,vinst1(a3)           ;
            bra     .done                   ;
.i0         tst.w   d2                      ;
            bne     .done                   ;
            move.w  d3,vinst0(a3)           ;
.done
            cmp.w   vinstlow(a3),d3         ;
            bgt     .greater                ;
            move.w  d3,vinstlow(a3)         ;
.greater    cmp.w   vinsthigh(a3),d3        ;
            blt     .less                   ;
            move.w  d3,vinsthigh(a3)        ;
.less
            endif                           ;

            cmp.w   #$0018,d3               ;
            beq     .beat                   ;
            cmp.w   #$001e,d3               ;
            beq     .beat                   ;
            cmp.w   #$ffb2,d3               ;
            beq     .beat                   ;
            cmp.w   #$0030,d3               ;
            bne     .nobeat                 ;
.beat       st      vtriggertext(a3)        ;
.nobeat
            lea     peaks0000(pc),a3        ;
            add.w   d3,a3                   ; d3 is in range $ff76 - $00c0
            move.b  (a3)+,(a3)              ; set peak

            add.w   d0,d0
            bcc     noReset
            bset    d2,d1
            move.w  d1,$96(a6)
noReset     move.l  (a2)+,(a5)
            move.w  (a2)+,4(a5)
            move.l  a2,(a4)
skip        subq.w  #4,a4
            sub.w   #$10,a5
            dbf     d2,vloop

            move.l  m_dmaconPatch-4(a1),a3      ; dmacon patch clist 1
            move.b  d1,(a3)                     ; dmacon
            move.l  m_dmaconPatch2-4(a1),a3     ; dmacon patch clist 2
            move.b  d1,(a3)                     ; dmacon

noInst      move.l  a0,(a1)         ; store word stream (or byte stream if coming from early out)
            rts

r_rewind    move.l  m_byteStreamLoop(a1),a0
            move.l  m_wordStreamLoop(a1),m_wordStream(a1)
            bra     process

r_chgbpm    move.b  (a0)+,m_currentBpm+1(a1)    ; BPM
            bra     process

lspinit     lea     base(pc),a0             ; a0: music data (any mem) + 10
            add.l   #lspmusic-base,a0
            move.l  lspbankbase(pc),a1      ; a1: sound bank data (chip mem)
            move.l  clistbase(pc),a2        ; a2: 16bit DMACON word address
            lea     lspdmacon+3-clist(a2),a2

            lea     LSPVars(pc),a3
            move.w  (a0)+,m_currentBpm(a3)  ; default BPM
            move.w  (a0)+,m_escCodeRewind(a3)
            move.w  (a0)+,m_escCodeSetBpm(a3)
            move.l  (a0)+,-(a7)             ; who cares? replace with addq.w #4,a0?
            move.l  a2,m_dmaconPatch(a3)

            move.l  clist2base(pc),a2       ; a2: 16bit DMACON word address
            lea     lspdmacon2+3-clist2(a2),a2
            move.l  a2,m_dmaconPatch2(a3)

            move.w  (a0)+,d0                ; instrument count
            lea     -12(a0),a2              ; LSP data has -12 offset on instrument tab (to win 2 cycles in fast player)
            move.l  a2,m_lspInstruments(a3) ; instrument tab addr ( minus 4 )
            subq.w  #1,d0
            move.l  a1,d1
relocLoop   bset.b  #0,3(a0)                ; bit0 is relocation done flag
            bne     relocated
            add.l   d1,(a0)
            add.l   d1,6(a0)
relocated   lea     12(a0),a0
            dbf     d0,relocLoop
            move.w  (a0)+,d0                ; codes count (+2)
            move.l  a0,m_codeTableAddr(a3)  ; code table
            add.w   d0,d0
            add.w   d0,a0
            move.l  (a0)+,d0                ; word stream size
            move.l  (a0)+,d1                ; byte stream loop point
            move.l  (a0)+,d2                ; word stream loop point
            move.l  a0,m_wordStream(a3)
            lea     (a0,d0.l),a1            ; byte stream
            move.l  a1,m_byteStream(a3)
            add.l   d2,a0
            add.l   d1,a1
            move.l  a0,m_wordStreamLoop(a3)
            move.l  a1,m_byteStreamLoop(a3)
            lea     m_currentBpm(a3),a0
            move.l  (a7)+,d0                ; music len in frame ticks? who cares? REMOVE?
            rts

    rsreset

m_byteStream        rs.l    1   ;  0 byte stream
m_wordStream        rs.l    1   ;  4 word stream
m_dmaconPatch       rs.l    1   ;  8 m_lfmDmaConPatch
m_codeTableAddr     rs.l    1   ; 12 code table addr
m_escCodeRewind     rs.w    1   ; 16 rewind special escape code
m_escCodeSetBpm     rs.w    1   ; 18 set BPM escape code
m_lspInstruments    rs.l    1   ; 20 LSP instruments table addr
m_relocDone         rs.w    1   ; 24 reloc done flag
m_currentBpm        rs.w    1   ; 26 current BPM
m_byteStreamLoop    rs.l    1   ; 28 byte stream loop point
m_wordStreamLoop    rs.l    1   ; 32 word stream loop point
m_dmaconPatch2      rs.l    1   ; 40 m_lfmDmaConPatch2, added
sizeof_LSPVars      rs.w    0

LSPVars     ds.b    sizeof_LSPVars,0
            
resetv      dc.l    0,0,0,0

*------ TEXT WIDTH ------------------------------------------------------------*

; input a0: text
; output d0, d4
; must not trash d6, d7 (see hiddenmessage)
textwidth
    lea     prop(pc),a1             ;
    moveq   #0,d2                   ; sum up width
.loop
    moveq   #0,d0                   ;
    move.b  (a0)+,d0                ;
    beq     .done                   ;
    sub.b   #" ",d0                 ;
    moveq   #0,d1                   ;
    move.b  (a1,d0.w),d1            ;
    addq.b  #2,d1                   ; padding
    add.l   d1,d2                   ;
    bra     .loop                   ;
.done

    if testing
    cmp.w   #322,d2                 ; too large?
    ble     .inrange                ;
    st      vquit(a5)               ; -> signal quit
.inrange
    endif

    move.l  #322,d0                 ; 320 + center adj (last padding)
    sub.l   d2,d0                   ;
    asr.l   #1,d0                   ; /2 (half)
    move.l  d0,d2                   ;
    asr.l   #3,d0                   ; /8 (byte)
    and.l   #%111,d2                ;
    moveq   #0,d4                   ;
    move.b  startmasks(pc,d2.w),d4  ;
    rts                             ;

startmasks
    dc.b    %10000000
    dc.b    %01000000
    dc.b    %00100000
    dc.b    %00010000
    dc.b    %00001000
    dc.b    %00000100
    dc.b    %00000010
    dc.b    %00000001

*------ PRINT TEXT ------------------------------------------------------------*

; param a1, d4
printtext
.char
    lea     text(pc),a0             ;
    add.w   vtextpointer(a5),a0     ;

    moveq   #0,d0                   ;
    move.b  (a0),d0                 ;
    beq     .done                   ;
    
    sub.b   #" ",d0                 ;
    moveq   #0,d1                   ;
    move.b  prop(pc,d0.w),d1        ;
    subq.w  #1,d1                   ;
    
    asl.w   #5,d0                   ; *32 (data of 1 char)
    lea     font(pc),a2             ;
    add.w   d0,a2                   ;

    moveq   #15,d3                  ; bit 15
.col
    movem.l a1-a2,-(a7)             ;
    moveq   #fheight-1,d7           ;
.row
    move.w  (a2),d0                 ;
    btst.l  d3,d0                   ;
    beq     .no
    or.b    d4,(a1)                 ; draw pixel
.no
    addq.w  #2,a2                   ; next row of char
    add.w   #pwidth,a1              ;
    dbf     d7,.row                 ;

    movem.l (a7)+,a1-a2             ;
    subq.w  #1,d3                   ; next bit to test in char col

    ror.b   #1,d4                   ; x++
    bcc     .x1                     ;
    addq.w  #1,a1                   ; next byte
.x1
    dbf     d1,.col                 ;
    
    ror.b   #1,d4                   ; x++
    bcc     .space1                 ;
    addq.w  #1,a1                   ; next byte
.space1
    ror.b   #1,d4                   ; x++
    bcc     .space2                 ;
    addq.w  #1,a1                   ; next byte
.space2
    
    addq.w  #1,vtextpointer(a5)     ;
    bra     .char                   ;
.done
    addq.w  #1,vtextpointer(a5)     ; skip 0
    rts                             ;

prop
    ; net values, 0 = unused char
    dc.b    14 ; space
    dc.b    2  ; !
    dc.b    5  ; "
    dc.b    0  ; #
    dc.b    0  ; $
    dc.b    0  ; %
    dc.b    0  ; &
    dc.b    4  ; '
    dc.b    6  ; (
    dc.b    6  ; )
    dc.b    0  ; *
    dc.b    0  ; +
    dc.b    4  ; ,
    dc.b    14 ; -
    dc.b    2  ; .
    dc.b    6  ; /
    dc.b    14,3,14,14,14,14,14,14,14,14 ; 0-9
    dc.b    2  ; :
    dc.b    0  ; ;
    dc.b    0  ; <
    dc.b    0  ; =
    dc.b    0  ; >
    dc.b    0  ; ?
    dc.b    14 ; @
    dc.b    14,14,14,14 ; ABCD
    dc.b    14,14,14,14 ; EFGH
    dc.b    2,14,13,2   ; IJKL
    dc.b    16,14,14,14 ; MNOP
    dc.b    14,14,14,14 ; QRST
    dc.b    14,14,16,14 ; UVWX
    dc.b    14,14       ; YZ

*------ PRINT LARGE TEXT ------------------------------------------------------*

; param a4: font a3: text  d4: x a1: bitplane  d2: pwidth
    even

printtextlarge
    movem.l a1/a3/d4,-(a7)          ;
.char
    moveq   #0,d0                   ;
    move.b  (a3)+,d0                ;
    bmi     .done                   ;
    
    moveq   #0,d1                   ;
    move.b  propsp(pc,d0.w),d1      ;
        
    move.w  d1,d3                   ; *3
    add.w   d1,d1                   ;
    add.w   d3,d1                   ;
    subq.w  #1,d1                   ; prepare dbf
    
    asl.w   #5,d0                   ; *32 (data of 1 char)
    move.l  a4,a2                   ;
    add.w   d0,a2                   ;

    moveq   #15,d3                  ; bit 15
    moveq   #3,d5                   ; 3 px wide
.col
    movem.l a1-a2,-(a7)             ;
    moveq   #fheight-1,d7           ;
.row
    move.w  (a2),d0                 ;
    btst.l  d3,d0                   ;
    beq     .notset                 ;
    or.b    d4,(a1)                 ; 3 px high
    add.w   d2,a1                   ;
    or.b    d4,(a1)                 ;
    add.w   d2,a1                   ;
    or.b    d4,(a1)                 ;
    add.w   d2,a1                   ;
    bra     .set                    ;
.notset
    add.w   d2,a1                   ;
    add.w   d2,a1                   ;
    add.w   d2,a1                   ;
.set
    addq.w  #2,a2                   ; next row of char
    dbf     d7,.row                 ;
    movem.l (a7)+,a1-a2             ;
    
    subq.w  #1,d5                   ;
    bne     .w4                     ;
    moveq   #3,d5                   ;
    subq.w  #1,d3                   ; next bit to test in char col
.w4
    ror.b   #1,d4                   ; x++
    bcc     .x1                     ;
    addq.w  #1,a1                   ; next byte
.x1
    dbf     d1,.col                 ;
    
    moveq   #6-1,d0                 ; space between chars
.space                              ;
    ror.b   #1,d4                   ; x++
    bcc     .space1                 ;
    addq.w  #1,a1                   ; next byte
.space1
    dbf     d0,.space               ;
    bra     .char                   ; next char
.done
    movem.l (a7)+,a1/a3/d4          ;
    rts                             ;

propsp
    dc.b    14,14,14,14 ; SPRE
    dc.b    14,14,14,2  ; ADOI
    dc.b    14,14,13    ; NT*

; SPRE ADOI NT*
; 0123 4567 891
;             0
textspread
    dc.b    0,1,2,3,4,5,-1
textpoint
    dc.b    1,6,7,8,9,-1

*------ TEXT ------------------------------------------------------------------*

text
    dc.b    "WHEN ROUTINES",0
    dc.b    "BITE HARD...",0
    dc.b    "",0
    
    dc.b    "PRESENTS",0
    dc.b    "MONOCHROME",0
    dc.b    "",0
    dc.b    "",0
    dc.b    "CODE: DEPECHE",0
    dc.b    "MUSIC: LORD",0
    dc.b    "ASCII: SKOPE",0
    dc.b    "FONT: LARGE 9 BY DCTRL",0
    rept 10
    dc.b    "",0
    endr

    dc.b    "RELEASED AT",0
    dc.b    "RELEASED AT",0
    dc.b    "DEMONIGHTS 016",0
    dc.b    "DEMONIGHTS 016",0
    dc.b    "ON 26 NOVEMBER 2022",0
    dc.b    "ON 26 NOVEMBER 2022",0
    rept 8
    dc.b    "",0
    endr
    
    dc.b    "LORD DID USE",0
    dc.b    "THE ORIGINAL",0
    dc.b    "SYNARE DRUM MACHINE",0
    dc.b    "WHICH",0
    dc.b    "JOY DIVISION",0
    dc.b    "USED FOR",0
    dc.b    "UNKNOWN PLEASURES",0
    rept 6
    dc.b    "",0
    endr

    dc.b    "LORD'S AMIGA37",0
    dc.b    "GREETINGS GO TO:",0
    dc.b    "MIKE",0
    dc.b    "MNEMOTRON",0
    dc.b    "MARKUS TILLMANN",0
    dc.b    "WILLI BAECKER",0
    dc.b    "PATRICK NEVIAN",0
    dc.b    "CHRIS HUELSBECK",0
    dc.b    "FASTLOADERS",0
    dc.b    "RAVI ABBOTT",0
    dc.b    "HOFFMAN",0
    dc.b    "PROJECT 42",0
    rept 6
    dc.b    "",0
    endr
        
    dc.b    "DEPECHE SAYS HI TO:",0
    dc.b    "DEPECHE SAYS HI TO:",0
    dc.b    "0B5VR",0
    dc.b    "4MAT",0
    dc.b    "AMIGA BILL",0
    dc.b    "BIFAT",0
    dc.b    "BLUEBERRY",0
    dc.b    "CCE",0
    dc.b    "DIPSWITCH",0
    dc.b    "FREQUENT",0
    dc.b    "GARGAJ",0
    dc.b    "GASMAN",0
    dc.b    "HAM",0
    dc.b    "HITCHHIKR",0
    dc.b    "HOFFMAN",0
    dc.b    "KONEY",0
    dc.b    "LASERBEAM",0
    dc.b    "LEMMY",0
    dc.b    "LEONARD",0
    dc.b    "MCCOY",0
    dc.b    "MENACE",0
    dc.b    "MOTION",0
    dc.b    "NE7",0
    dc.b    "PHOTON",0
    dc.b    "PRIMAX",0
    dc.b    "PS",0
    dc.b    "ROG",0
    dc.b    "REZ",0
    dc.b    "SACHY",0
    dc.b    "SENSENSTAHL",0
    dc.b    "SERPENT",0
    dc.b    "SKOPE",0
    dc.b    "STI",0
    dc.b    "STINGRAY",0
    dc.b    "TITUS",0
    dc.b    "UNLOCK",0
    dc.b    "VIRGILL",0
    dc.b    "XXX",0
    dc.b    "ZODIAC",0
    rept 6
    dc.b    "",0
    endr

    dc.b    "WE SEND",0
    dc.b    "WE SEND",0
    dc.b    "GREETINGS TO:",0
    dc.b    "GREETINGS TO:",0
    dc.b    "ABYSS",0
    dc.b    "ABYSS CONNECTION",0
    dc.b    "ADEPT",0
    dc.b    "ALCATRAZ",0
    dc.b    "ALTAIR",0
    dc.b    "APEX",0
    dc.b    "ARTSTATE",0
    dc.b    "ATTENTIONWHORE",0
    dc.b    "BATMAN GROUP",0
    dc.b    "BRAINSTORM",0
    dc.b    "COCOON",0
    dc.b    "DEFJAM",0
    dc.b    "DEMOZOO",0
    dc.b    "DESIRE",0
    dc.b    "DREAMWEB",0
    dc.b    "DRIFTERS",0
    dc.b    "FAIRLIGHT",0
    dc.b    "HAUJOBB",0
    dc.b    "HIGH QUALITY CRACKINGS",0
    dc.b    "KESTRA BITWORLD",0
    dc.b    "LOGICOMA",0
    dc.b    "LOONIES",0
    dc.b    "MELON",0
    dc.b    "POUET",0
    dc.b    "QUARTEX",0
    dc.b    "RADWAR ENTERPRISES",0
    dc.b    "RAZOR 1911",0
    dc.b    "REBELS",0
    dc.b    "RESISTANCE",0
    dc.b    "SCA",0
    dc.b    "SCOOPEX",0
    dc.b    "SECTION 8",0
    dc.b    "SPACEBALLS",0
    dc.b    "SPACEPIGS",0
    dc.b    "THE BLACK LOTUS",0
    dc.b    "THE ELECTRONIC KNIGHTS",0
    dc.b    "THE LIGHT CIRCLE",0
    dc.b    "THE SILENTS",0
    dc.b    "THE STAR FRONTIERS",0
    dc.b    "TRSI",0
    dc.b    "UNIT A",0
    dc.b    "UP ROUGH",0
    dc.b    "VISION FACTORY",0
    dc.b    "VOID",0
    rept 6
    dc.b    "",0
    endr

    dc.b    "SPECIAL GREETINGS TO:",0
    dc.b    "SPECIAL GREETINGS TO:",0
    dc.b    "JOCELYN BELL BURNELL",0
    dc.b    "JEREMIAH P. OSTRIKER",0
    dc.b    "JOY DIVISION",0
    dc.b    "PETER SAVILLE",0
    dc.b    "MIKEFC/COOLBUTUSELESS",0
    rept 6
    dc.b    "",0
    endr

    dc.b    "SPREADPOINT",0
    dc.b    "SPREADPOINT",0
    dc.b    "JOIN THE POWER",0
    dc.b    "JOIN THE POWER",0
    dc.b    "WE ARE INTERGALACTIC",0
    dc.b    "WE ARE INTERGALACTIC",0

    rept 16
    dc.b    "",0
    endr

textend
texthidden
    dc.b    "SORRY",0
    dc.b    "LORD...",0
    dc.b    "",0
    dc.b    "NO",0
    dc.b    "SNOWFALL",0
    dc.b    "EFFECT.",0

*------ LOGO EASE IN OUT DATA -------------------------------------------------*

    even
ease
    dc.w    0, 0, 400, 12, 800, 25, 1160, 37, 1560, 49, 1920, 60, 2280, 72, 2640, 83, 3000, 94, 3320, 104, 3680, 115, 4000, 125, 4320, 135, 4600, 144, 4920, 154, 5200, 163, 5480, 172, 5760, 180, 6040, 188, 6280, 196, 6520, 204, 6760, 212, 7000, 219, 7240, 226, 7440, 233, 7680, 240, 7880, 246, 8040, 252, 8240, 258, 8400, 263, 8600, 268, 8760, 273, 8880, 278, 9040, 283, 9160, 287, 9280, 291, 9400, 294, 9520, 298, 9640, 301, 9720, 304, 9800, 307, 9880, 309, 9960, 311, 10000, 313, 10080, 315, 10120, 316, 10160, 317, 10200, 318, 10200, 319, 10200, 319, 10240, 320
    
    rept 64
    dc.w    10240, 320
    endr
    
    dc.w    10240, 320, 10200, 319, 10200, 319, 10200, 318, 10160, 317, 10120, 316, 10080, 315, 10000, 313, 9960, 311, 9880, 309, 9800, 307, 9720, 304, 9640, 301, 9520, 298, 9400, 294, 9280, 291, 9160, 287, 9040, 283, 8880, 278, 8760, 273, 8600, 268, 8400, 263, 8240, 258, 8040, 252, 7880, 246, 7640, 239, 7440, 233, 7240, 226, 7000, 219, 6760, 212, 6520, 204, 6280, 196, 6040, 188, 5760, 180, 5480, 172, 5200, 163, 4920, 154, 4600, 144, 4320, 135, 4000, 125, 3680, 115, 3320, 104, 3000, 94, 2640, 83, 2280, 72, 1920, 60, 1560, 49, 1160, 37, 800, 25, 400, 12, 0, 0
easeend

*------ FONT ------------------------------------------------------------------*

; Large 9 by dCTRL (Andreas A. Lorenz, dctrl.ch)
; https://www.dafont.com/large9.font

fheight equ 16
    even

font
; space
    ds.w    16,0

; !
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; "
    dc.w    %1101100000000000
    dc.w    %1101100000000000
    dc.w    %1101100000000000
    dc.w    %1101100000000000
    dc.w    %1101100000000000
    dc.w    %1101100000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; #
    ds.w    16,0

; $
    ds.w    16,0
    
; %
    ds.w    16,0
    
; &
    ds.w    16,0
    
; '
    dc.w    %0001100000000000
    dc.w    %0001100000000000
    dc.w    %0001100000000000
    dc.w    %0110000000000000
    dc.w    %0110000000000000
    dc.w    %0110000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
; (
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111110000000000
    dc.w    %1111110000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111110000000000
    dc.w    %1111110000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; )
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111110000000000
    dc.w    %1111110000000000
    dc.w    %0000110000000000
    dc.w    %0000110000000000
    dc.w    %0000110000000000
    dc.w    %0000110000000000
    dc.w    %0000110000000000
    dc.w    %1111110000000000
    dc.w    %1111110000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; *
    ds.w    16,0

; +
    ds.w    16,0

; ,
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0011000000000000
    dc.w    %0011000000000000
    dc.w    %0011000000000000
    dc.w    %0011000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000

; -
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; .
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; /
    dc.w    %0000110000000000
    dc.w    %0000110000000000
    dc.w    %0000110000000000
    dc.w    %0000110000000000
    dc.w    %0011000000000000
    dc.w    %0011000000000000
    dc.w    %0011000000000000
    dc.w    %0011000000000000
    dc.w    %0011000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
; 0 = o
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; note extra space at beginning
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0110000000000000
    dc.w    %0110000000000000
    dc.w    %0110000000000000
    dc.w    %0110000000000000
    dc.w    %0110000000000000
    dc.w    %0110000000000000
    dc.w    %0110000000000000
    dc.w    %0110000000000000
    dc.w    %0110000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; 2 = z
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; :
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
; ;
    ds.w    16,0

; <
    ds.w    16,0

; =
    ds.w    16,0

; >
    ds.w    16,0

; ?
    ds.w    16,0

; @
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100111111101100
    dc.w    %1100110001101100
    dc.w    %1100110001101100
    dc.w    %1100111111111100
    dc.w    %1100111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100

    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100

    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000111111000
    dc.w    %1100000111111000
    dc.w    %1100000110000000
    dc.w    %1111111110000000
    dc.w    %1111111110000000
    dc.w    %1100000110000000
    dc.w    %1100000110000000
    dc.w    %1100000111111000
    dc.w    %1100000111111000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111111
    dc.w    %1111111111111111
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000110000
    dc.w    %1100000000110000
    dc.w    %1111111111110000
    dc.w    %1111111111110000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1100000110000011
    dc.w    %1111111111111111
    dc.w    %1111111111111111
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %0011111111110000
    dc.w    %0011111111110000
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1100000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %1100000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

fontwest
; s
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
; p
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000

; r
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; e
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; a
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
; d
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
; o
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; i
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; n
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; t
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %1100000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
; * (snowflake)
    dc.w    %0000001000000000
    dc.w    %0001011101000000
    dc.w    %0010001000100000
    dc.w    %0101001001010000
    dc.w    %0000101010000000
    dc.w    %0100011100010000
    dc.w    %1111111111111000
    dc.w    %0100011100010000
    dc.w    %0000101010000000
    dc.w    %0101001001010000
    dc.w    %0010001000100000
    dc.w    %0001011101000000
    dc.w    %0000001000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

fontnorth
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    rem
fonteast
; s
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
; p
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; r
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; e
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; a
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
; d
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
; o
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; i
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; n
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000001100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

; t
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    erem

fontsouth
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000


    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %1111111111111100
    dc.w    %1111111111111100
    dc.w    %0000000000000000
    dc.w    %0000000000000000
    dc.w    %0000000000000000

*------ MUSIC -----------------------------------------------------------------*

    even
lspbank
    incbin  "lord-lost_control.lsbank"
lspbankend

    even
lspmusic
    incbin  "lord-lost_control.lsmusic",10  ; skip header (10 bytes)
