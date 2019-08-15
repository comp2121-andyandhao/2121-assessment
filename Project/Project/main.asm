/* **************************************************
 *           COMP2121 19T2 Assessment               *
 *           ** Monorail Emulator **                *
 *           Created: 16 August 2019                *
 *    Author : Andy Lu(z       ), Hao Sun(z5158176) *
 * **************************************************
*/
.include "m2560def.inc"


/*===========REGISTER DECLARATIONS===========*/

; Constant Registers
.def one = r15
.def zero = r14

; Main Registers
.def tempMain = r25
.def tempSec =r24
.def stationPointer = r23
.def cursorPos = r22
.def storagePos = r21
.def row = r20
.def col = r19
.def mask = r18
.def displayChar = r17

; Custom Flag Registers
.def invExpression = r2
.def OVFOccured = r3

; Global Registers
.def mulRes_l = r0
.def mulRes_h = r1


/*===========VALUE DECLARATIONS===========*/
.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4
.equ PORTLDIR = 0b11110000
.equ INITCOLMASK = 0b11101111
.equ INITROWMASK = 0b00000001
.equ ROWMASK = 0b00001111

/*===========DSEG===========*/

.dseg
.org 0x200
maxStationNum: .byte 1
stationNames: .byte 100
travelTimes: .byte 10
stopTime: .byte 1

ASCIIStorage: .byte 10


/*===========CSEG===========*/

.cseg
.org 0
jmp RESET

.org INT0addr
;jmp buttonRight

.org INT1addr
;jmp buttonLeft

.org INT3addr
;jmp laserReceiver

.org OVF0addr
;jmp Timer0OVF

/*===========DELAY MACRO===========*/

.macro delay
    push tempMain
    push tempSec

    clr zero
    clr one
    inc one

    clr tempMain
    clr tempSec

    ldi tempMain, high(@0)
    ldi tempSec, low(@0)

    delayLoop:
        sub tempSec, one
        sbc tempMain,zero

        push tempMain
        push tempSec

        ldi tempMain, 0x0A
        ldi tempSec, 0x6B

        subDelayLoop:
            sub tempSec, one
            sbc tempMain,zero
            cp tempSec, zero
            cpc tempMain, zero
            brne subDelayLoop

        pop tempSec
        pop tempMain

        cp tempMain, zero
        cpc tempSec, zero
        brne delayLoop

    pop tempSec
    pop tempMain
.endmacro

/*===========MULTIPLICATION MACRO===========*/

.macro multMacro ; max result - 2 bytes
    push r15
    push r14

    ldi tempMain, @2
    mul @1, tempMain
    mov r14, r1
    mov r15, r0
    mul @0, tempMain
    add r14, r0
    mov @1, r15
    mov @0, r14

    pop r14
    pop r15
.endmacro

/*===========BLINK MACRO===========*/

blink:
    push tempMain
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 250
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 250
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 250
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 250
    pop tempMain
ret

/*===========QUICK BLINK MACRO===========*/

quickBlink:
    push tempMain
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 100
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 100
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 100
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 100
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 100
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 100
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 100
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 100
    pop tempMain
ret

/*===========SPECIAL BLINK MACRO===========*/

specialBlink:
    push tempMain
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0 ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    ldi tempMain, 0xff ;;;;;;;;;;;;;
    out PORTC, tempMain
    delay 30
    pop tempMain
ret

/*===========PRESET LCD COMMANDS===========*/

.macro do_lcd_command
    ldi tempMain, @0
    call lcd_command
    call lcd_wait
.endmacro

.macro do_lcd_data
    mov tempMain, @0
    call lcd_data
    call lcd_wait
.endmacro

.macro lcd_set
    sbi PORTA, @0
.endmacro

.macro lcd_clr
    cbi PORTA, @0
.endmacro

lcd_command:
    out PORTF, tempMain
    nop
    lcd_set LCD_E
    nop
    nop
    nop
    lcd_clr LCD_E
    nop
    nop
    nop
ret

lcd_data:
    out PORTF, tempMain
    lcd_set LCD_RS
    nop
    nop
    nop
    lcd_set LCD_E
    nop
    nop
    nop
    lcd_clr LCD_E
    nop
    nop
    nop
    lcd_clr LCD_RS
ret

lcd_wait:
    push tempMain
    clr tempMain
    out DDRF, tempMain
    out PORTF, tempMain
    lcd_set LCD_RW

lcd_wait_loop:
    nop
    lcd_set LCD_E
    nop
    nop
    nop
    in tempMain, PINF
    lcd_clr LCD_E
    sbrc tempMain, 7
    jmp lcd_wait_loop
    lcd_clr LCD_RW
    ser tempMain
    out DDRF, tempMain
    pop tempMain
ret

/*===========CLEAR DISPLAY MACRO===========*/

.macro clearDisplay
    do_lcd_command 0b00000001 ; clear display
    clr cursorPos
.endmacro

/*===========DISPLAY UPDATE MACRO===========*/

.macro updateDisplay
    push r16
    mov r16, @0
    push tempMain
    ldi tempMain, 16
    cp cursorPos, tempMain
    pop tempMain
    brne skipClear
    clearDisplay
    skipClear:

    do_lcd_data r16
    inc cursorPos
    pop r16
.endmacro

/*===========DISPLAY UPDATE WITH ASCII MACRO===========*/

.macro updateDisplayWithASCII
    ldi tempMain, 16
    cp cursorPos, tempMain
    brne skipClear
    clearDisplay
    skipClear:

    ldi tempMain, @0
    do_lcd_data tempMain
    inc cursorPos
.endmacro

/*===========CLEAR ASCII STORAGE MACRO===========*/

.macro clearASCIIStorage
    ldi xl, low(ASCIIStorage)
    ldi xh, high(ASCIIStorage)

    st x+, zero
    st x+, zero
    st x+, zero
    st x+, zero
    st x+, zero
    st x+, zero
    st x+, zero
    st x+, zero
    st x+, zero
    st x+, zero

    clr storagePos
.endmacro

/*===========DISPLAY STRING SUBROUTINE===========*/

displayString:
    push r16
    clr r16
    mov r16, tempMain

    clearDisplay

    displayStringLoop:

    lpm tempSec, z+
    updateDisplay tempSec

    delay 25 // TO BE MODIFIED

    dec r16
    out PORTC, r16 ;;;;;;;;;;;;
    cpi r16, 0

    breq skipDisplayStringLoop
    jmp displayStringLoop
    skipDisplayStringLoop:
    pop r16
ret

/*===========MASTER RESET SUBROUTINE===========*/

RESET:
    ldi tempMain, low(RAMEND)
    out spl, tempMain
    ldi tempMain, high(RAMEND)
    out sph, tempMain

//********************************
    ser tempMain
    out DDRF, tempMain
    out DDRA, tempMain
    clr tempMain
    out PORTF, tempMain
    out PORTA, tempMain

    do_lcd_command 0b00111000 ; display setting
    delay 5
    do_lcd_command 0b00111000 ; display setting
    delay 1
    do_lcd_command 0b00111000 ; display setting
    do_lcd_command 0b00111000 ; display setting
    do_lcd_command 0b00001000 ; display on
    do_lcd_command 0b00000001 ; clear display
    do_lcd_command 0b00000110 ; increment, no display shift
    do_lcd_command 0b00001110 ; cursor setting

//********************************

    ldi tempMain, PORTLDIR ; columns are outputs, rows are inputs
    STS DDRL, tempMain     ; cannot use out

    ldi tempMain, (0 << CS50) 
    sts TCCR5B, tempMain 
    ldi tempMain, (0 << WGM50)|(0 << COM5A1)
    sts TCCR5A, tempMain

//********************************
    ser tempMain
    out DDRC, tempMain ; PORTC is all ouputs
;    ldi tempMain, 0b11111111
;    out PORTC, tempMain
;    delay 50
;    ldi tempMain, 0b11100111
;    out PORTC, tempMain
;    delay 50
;    ldi tempMain, 0b11000011
;    out PORTC, tempMain
;    delay 50
;    ldi tempMain, 0b10000001
;    out PORTC, tempMain
;    delay 50
;    ldi tempMain, 0b00000000
;    out PORTC, tempMain

//********************************
    testData: .db "1234567890", 0, 0 ; 10 char
    errorOcc: .db "Error occured, please try again.", 0, 0 ; 32 char
    // Phase 1
    iniSNum: .db "Please type the maximum number of stations", 0, 0 ; 42 char
    // Phase 2
    iniSName: .db "Pleas type the name of station", 0, 0 ; 30 char
    // Phase 3
    iniSTravel1: .db "The time from Station", 0 ; 21 char
    iniSTravel2: .db "to Station", 0, 0 ; 10 char
    // Phase 4
    iniSStop: .db "The stop time of the monorail at any station is", 0 ; 47 char
    iniFin: .db "Now the configuration is complete. Please wait 5 seconds", 0, 0 ; 56 char
    iniCharOVF: .db "Name cannot be more than 10 characters, please try again", 0, 0 ; 56 char

//********************************
    clr zero
    clr one
    inc one
    clr cursorPos
    clr storagePos
    clr invExpression
    clr OVFOccured
    clr stationPointer
    clearASCIIStorage
    ;call specialBlink
jmp main


/*===========MAIN FUNCTION===========*/

main:

    call phase1
    phase1Completed:

    clearASCIIStorage

    call phase2
    phase2Completed:

    clearDisplay

    ldi yl, low(stationNames)
    ldi yh, high(stationNames)

    ldi tempMain, 0
    multMacro zero, tempMain, 10
    add yl, tempMain

    ld tempMain, y+
    updateDisplay tempMain
    delay 100
    ld tempMain, y+
    updateDisplay tempMain
    delay 100
    ld tempMain, y+
    updateDisplay tempMain
    delay 100
    ld tempMain, y+
    updateDisplay tempMain
    delay 100
    ld tempMain, y+
    updateDisplay tempMain
    delay 100
    ld tempMain, y+
    updateDisplay tempMain
    delay 100
    ld tempMain, y+
    updateDisplay tempMain
    delay 100
    ld tempMain, y+
    updateDisplay tempMain
    delay 100
    ld tempMain, y+
    updateDisplay tempMain
    delay 100
    ld tempMain, y+
    updateDisplay tempMain
    delay 100

    endLoop:

        ldi tempMain, 0xff ;;;;;;;;;;;;;
        out PORTC, tempMain
        delay 500
;        ldi tempMain, 0 ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0xff ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0 ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0xff ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0 ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;
;        ldi tempMain, 0xff ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 1500
;        ldi tempMain, 0 ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0xff ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 1500
;        ldi tempMain, 0 ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0xff ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 1500
;        ldi tempMain, 0 ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;
;        ldi tempMain, 0xff ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0 ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0xff ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0 ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0xff ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
;        ldi tempMain, 0 ;;;;;;;;;;;;;
;        out PORTC, tempMain
;        delay 500
    rjmp endLoop

/*===========PHASE 1 SUBROUTINE===========*/

phase1:
    ldi zl, low(iniSNum<<1)
    ldi zh, high(iniSNum<<1)
    ldi tempMain, 42
    call displayString
    updateDisplayWithASCII ':'
    updateDisplayWithASCII 32

    call valueEnter
ret

/*===========PHASE 2 SUBROUTINE===========*/

phase2:

    clr stationPointer

    nameEnterLoop:

        ldi xl, low(maxStationNum) 
        ldi xh, high(maxStationNum)
        ld tempMain, x

        cp stationPointer, tempMain
        brne nameEnterNotComplete
        jmp nameEnterComplete

        nameEnterNotComplete:
        ldi zl, low(iniSName<<1)
        ldi zh, high(iniSName<<1)
        ldi tempMain, 30
        call displayString
        updateDisplayWithASCII 32
        delay 25
        ldi tempMain, '1'
        mov tempSec, stationPointer
        add tempSec, tempMain
        updateDisplay tempSec
        delay 25
        updateDisplayWithASCII ':'
        delay 25
        updateDisplayWithASCII 32
        
        call characterEnter

    nameEnterComplete:

    call quickBlink
    call quickBlink
jmp phase2Completed

/*===========NUMBER-BASE VALUE ENTERING FUNCTION===========*/

valueEnter:
    ldi mask, INITCOLMASK
    clr col

    colloopVal:
        STS PORTL, mask
        ldi tempMain, 255

        delayMainVal:
            dec tempMain
            brne delayMainVal

    LDS tempMain, PINL 
    andi tempMain, ROWMASK 
    cpi tempMain, 0b00001111
    brne rowNotPushedVal 
    jmp nextcolVal
    rowNotPushedVal:

    ldi mask, INITROWMASK
    clr row 

    rowloopVal:
        mov tempSec, tempMain
        and tempSec, mask
        breq isEqualVal
        jmp skipconvVal
        isEqualVal:
        call convertVal 

    delay 250
    jmp valueEnter
ret

/*===========NUMBER-BASE KEYPAD COMMANDS===========*/

skipconvVal:
    inc row
    lsl mask 
jmp rowloopVal

nextcolVal:     
    cpi col, 3
    brne colNotPushedVal
    jmp valueEnter
    colNotPushedVal:
    sec
    rol mask
    inc col
jmp colloopVal

convertVal:
    cpi col, 3
    breq lettersVal
    cpi row, 3
    breq symbolsVal
    mov displayChar, row 
    lsl displayChar
    add displayChar, row
    add displayChar, col
    ldi tempSec, 49
    add displayChar, tempSec

jmp convert_endVal

lettersVal:
    mov tempMain, row
    cpi tempMain, 0
    breq ACharVal
    cpi tempMain, 1
    breq BCharVal
    cpi tempMain, 2
    breq CChar
    ldi displayChar, 'D'
    clr invExpression
    inc invExpression
jmp convert_endVal

ACharVal:
    ldi displayChar, 'A'
    clr invExpression
    inc invExpression
jmp convert_endVal

BCharVal:
    ldi displayChar, 'B'
    clr invExpression
    inc invExpression
jmp convert_endVal

CChar:
jmp phase1End

symbolsVal:
    cpi col, 0
    breq starVal
    cpi col, 1
    breq zeroCharVal
    ldi displayChar, '#'
    clr invExpression
    inc invExpression
jmp convert_endVal

starVal:
    ldi displayChar, '*'
    clr invExpression
    inc invExpression
jmp convert_endVal

zeroCharVal:
    ldi displayChar, '0'

convert_endVal:
    updateDisplay displayChar
    call storeKeyPadVal
ret

/*===========NUMBER-BASE STORE KEYPAD VAL===========*/

storeKeyPadVal:
    ldi xl, low(ASCIIStorage)
    ldi xh, high(ASCIIStorage)

    out PORTC, storagePos

    add xl, storagePos

    st x+, displayChar

    inc storagePos
    cpi storagePos, 3
    brsh charOVFVal
    jmp noCharOVFVal
    charOVFVal:
    clr OVFOccured
    inc OVFOccured
    noCharOVFVal:
ret

/*===========NUMBER-BASE END===========*/

phase1End:
    push r16
    push r17
    cp OVFOccured, zero
    brne overflowed
    jmp notOverflowed

    overflowed:
    jmp displayError

    notOverflowed:
    cp invExpression, zero
    brne notValid
    jmp notInvalid

    notValid:
    jmp displayError

    notInvalid:
    ldi xl, low(ASCIIStorage) 
    ldi xh, high(ASCIIStorage)

    cp storagePos, one
    breq sinInt
    jmp notSinInt

    sinInt:
    push r16
    ld r16, x+
    out PORTC, r16
    subi r16, '0'
    out PORTC, r16
    cpi r16, 0
    brne notZero
    pop r16
    jmp displayError

    notZero:
    out PORTC, r16
    ldi xl, low(maxStationNum) 
    ldi xh, high(maxStationNum)
    st x, r16
    delay 1000 ;;;;;;;;;;;;
    pop r16
    jmp phase1Completed

    notSinInt:
    cp storagePos, zero
    breq nothingInSt
    jmp somethingInSt

    nothingInSt:
    jmp displayError

    somethingInSt:
    push r16
    push r17
    ld r16, x+
    ld r17, x+
    cpi r16, '1'
    breq testFor10
    pop r17
    pop r16
    jmp displayError

    testFor10:
    cpi r17, '0'
    breq isTen
    pop r17
    pop r16
    jmp displayError

    isTen:
    ldi xl, low(maxStationNum) 
    ldi xh, high(maxStationNum)
    ldi r16, 10
    st x, r16
    out PORTC, r16
    pop r17
    pop r16
    delay 1000 ;;;;;;;;;;;;;;;;;;;
    jmp phase1Completed

/*===========CHARACTER ENTERING FUNCTION===========*/

characterEnter:
    out PORTC, stationPointer
    ldi mask, INITCOLMASK
    clr col

    colloopChar:
        STS PORTL, mask
        ldi tempMain, 255

        delayMainChar:
            dec tempMain
            brne delayMainChar

    LDS tempMain, PINL 
    andi tempMain, ROWMASK 
    cpi tempMain, 0b00001111
    brne rowNotPushedChar 
    jmp nextcolChar
    rowNotPushedChar:

    ldi mask, INITROWMASK
    clr row 

    rowloopChar:
        mov tempSec, tempMain
        and tempSec, mask
        breq isEqualChar
        jmp skipconvChar
        isEqualChar:
        call convertChar 

    delay 250
    jmp characterEnter
ret

/*===========CHARACTER KEYPAD COMMANDS===========*/

skipconvChar:
    inc row
    lsl mask 
jmp rowloopChar

nextcolChar:     
    cpi col, 3
    brne colNotPushedChar
    jmp characterEnter
    colNotPushedChar:
    sec
    rol mask
    inc col
jmp colloopChar

convertChar:
    cpi col, 3
    breq lettersChar
    cpi row, 3
    breq symbolsChar
    mov displayChar, row 
    lsl displayChar
    add displayChar, row
    add displayChar, col
    ldi tempSec, 49
    add displayChar, tempSec

jmp convert_endChar

lettersChar:
    mov tempMain, row
    cpi tempMain, 0
    breq ACharChar
    cpi tempMain, 1
    breq BCharChar
    cpi tempMain, 2
    breq CCharChar
    ldi displayChar, 'D'
    clr invExpression
    inc invExpression
jmp convert_endChar

ACharChar:
    call charSwitch
jmp convert_endChar

BCharChar:
    ldi displayChar, 32
jmp convert_endChar

CCharChar:
    jmp phase2End

symbolsChar:
    cpi col, 0
    breq starChar
    cpi col, 1
    breq zeroCharChar
    ldi displayChar, '#'
    clr invExpression
    inc invExpression
jmp convert_endChar

starChar:
    ldi displayChar, 'Q'
jmp convert_endChar

zeroCharChar:
    ldi displayChar, 'Z'

convert_endChar:
    call numToChar
    updateDisplay displayChar
    call storeKeyPadChar
ret

;/*===========SRAM DISPLAY===========*/
;
;displaySRAM:
;    ldi xl, low(ASCIIStorage)
;    ldi xh, high(ASCIIStorage)
;    ld tempMain, x+
;    updateDisplay tempMain
;    ld tempMain, x+
;    updateDisplay tempMain
;    ld tempMain, x+
;    updateDisplay tempMain
;    ld tempMain, x+
;    updateDisplay tempMain
;    ld tempMain, x+
;    updateDisplay tempMain
;    ld tempMain, x+
;    updateDisplay tempMain
;    ld tempMain, x+
;    updateDisplay tempMain
;    temploop: jmp temploop

/*===========NUMBER TO CHARACTER CONVERTER===========*/

numToChar:
    cpi displayChar, '1'
    brne not1Conv
    clr invExpression
    inc invExpression
    jmp numToCharEnd

    not1Conv:
    cpi displayChar, '2'
    brne not2Conv
    ldi displayChar, 'A'
    jmp numToCharEnd

    not2Conv:
    cpi displayChar, '3'
    brne not3Conv
    ldi displayChar, 'D'
    jmp numToCharEnd

    not3Conv:
    cpi displayChar, '4'
    brne not4Conv
    ldi displayChar, 'G'
    jmp numToCharEnd

    not4Conv:
    cpi displayChar, '5'
    brne not5Conv
    ldi displayChar, 'J'
    jmp numToCharEnd

    not5Conv:
    cpi displayChar, '6'
    brne not6Conv
    ldi displayChar, 'M'
    jmp numToCharEnd

    not6Conv:
    cpi displayChar, '7'
    brne not7Conv
    ldi displayChar, 'P'
    jmp numToCharEnd

    not7Conv:
    cpi displayChar, '8'
    brne not8Conv
    ldi displayChar, 'T'
    jmp numToCharEnd

    not8Conv:
    cpi displayChar, '9'
    brne not9Conv
    ldi displayChar, 'W'
    jmp numToCharEnd

    not9Conv:

    numToCharEnd:
ret

/*===========CHARACTER SWITCHER===========*/

charSwitch:
    ldi xl, low(ASCIIStorage)
    ldi xh, high(ASCIIStorage)
    dec storagePos
    add xl, storagePos

    ld displayChar, x
    cpi displayChar, 'Q'
    brne conSwitch
    jmp charConvEnd
    conSwitch:
    ;updateDisplay displayChar
    ;call quickBlink
    inc displayChar

    cpi displayChar, 'D'
    brne char1Conv
    ldi displayChar, 'A'
    jmp charConvEnd

    char1Conv:
    cpi displayChar, 'G'
    brne char2Conv
    ldi displayChar, 'D'
    jmp charConvEnd

    char2Conv:
    cpi displayChar, 'J'
    brne char3Conv
    ldi displayChar, 'G'
    jmp charConvEnd

    char3Conv:
    cpi displayChar, 'M'
    brne char4Conv
    ldi displayChar, 'J'
    jmp charConvEnd

    char4Conv:
    cpi displayChar, 'P'
    brne char5Conv
    ldi displayChar, 'M'
    jmp charConvEnd

    char5Conv:
    cpi displayChar, 'T'
    brne char6Conv
    ldi displayChar, 'P'
    jmp charConvEnd

    char6Conv:
    cpi displayChar, 'W'
    brne char7Conv
    ldi displayChar, 'T'
    jmp charConvEnd

    char7Conv:
    cpi displayChar, 'Z'
    brne char8Conv
    ldi displayChar, 'W'
    jmp charConvEnd

    char8Conv:
    cpi displayChar, '['
    brne char9Conv
    ldi displayChar, 'Z'
    jmp charConvEnd

    char9Conv:
    cpi displayChar, 'Q'
    brne char10Conv
    ldi displayChar, 'R'
    jmp charConvEnd

    char10Conv:

    charConvEnd:
    st x+, displayChar
    dec cursorPos
    do_lcd_command 0b00010000
ret

/*===========CHARACTER STORE KEYPAD VAL===========*/

storeKeyPadChar:
    ldi xl, low(ASCIIStorage)
    ldi xh, high(ASCIIStorage)

    inc storagePos
    cpi storagePos, 11
    dec storagePos
    brsh charOVFChar
    jmp noCharOVFChar

    charOVFChar:
    clr OVFOccured
    inc OVFOccured
    jmp charOVFError

    noCharOVFChar:
    add xl, storagePos
    st x+, tempMain
    inc storagePos

    ;OVFNoActChar:
ret

/*===========PHASE 2 END===========*/

phase2End:
    ldi xl, low(ASCIIStorage)
    ldi xh, high(ASCIIStorage)

    ldi yl, low(stationNames)
    ldi yh, high(stationNames)

    mov tempMain, stationPointer
    multMacro zero, tempMain, 10
    add yl, tempMain

    ld tempMain, x+
    st y+, tempMain
    updateDisplay tempMain
    delay 100
    ld tempMain, x+
    st y+, tempMain
    updateDisplay tempMain
    delay 100
    ld tempMain, x+
    st y+, tempMain
    updateDisplay tempMain
    delay 100
    ld tempMain, x+
    st y+, tempMain
    updateDisplay tempMain
    delay 100
    ld tempMain, x+
    st y+, tempMain
    updateDisplay tempMain
    delay 100
    ld tempMain, x+
    st y+, tempMain
    updateDisplay tempMain
    delay 100
    ld tempMain, x+
    st y+, tempMain
    updateDisplay tempMain
    delay 100
    ld tempMain, x+
    st y+, tempMain
    updateDisplay tempMain
    delay 100
    ld tempMain, x+
    st y+, tempMain
    updateDisplay tempMain
    delay 100
    ld tempMain, x+
    st y+, tempMain
    updateDisplay tempMain
    delay 100

    clearASCIIStorage
    inc stationPointer
jmp nameEnterLoop

/*===========NUMBER-BASE ERROR SUBROUTINE===========*/

displayError:
    ldi zl, low(errorOcc<<1)
    ldi zh, high(errorOcc<<1)
    ldi tempMain, 32
    call displayString
    clr OVFOccured
    clr invExpression
    clearASCIIStorage
    call quickBlink
jmp phase1

/*===========NUMBER-BASE ERROR SUBROUTINE===========*/

charOVFError:
    ldi zl, low(iniCharOVF<<1)
    ldi zh, high(iniCharOVF<<1)
    ldi tempMain, 56
    call displayString
    clr OVFOccured
    clr invExpression
    clearASCIIStorage
    call quickBlink
jmp nameEnterLoop