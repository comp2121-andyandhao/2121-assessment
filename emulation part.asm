/* one loop only
 * register used here:
 * temp temp2 counter_1 counter_2 tempMain mask step
 */
/* ========== EMULATION PART ========== */
ldi temp2, (1<<PE4)                     ; turn on the motor
out PORTE, temp2
out DDRE, temp2

ldi temp2, 0b10000000
out PORTC, temp2
ldi temp2, 0b00000000
out DDRC, temp2

ldi xh, high(NEXT_STATION_LENGTH)
ldi xl, low(NEXT_STATION_LENGTH)
ld counter_1, x                         ; here counter_1 stores the length of the next station
ldi yh, high(NEXT_STATION_NAME)
ldi yl, low(NEXT_STATION_NAME)
ldi zh, high(NEXT_STATION_TIME)
ldi zl, low(NEXT_STATION_TIME)
ld counter_2, z                         ; counter_2 stores the travel time
clearDisplay

// display station's name
display_name_loop:
    cpi counter_1, 0                    ; counter_1 will decrease until it reach 0
    breq travel_loop
    ld temp, y+
    do_lcd_data temp                    ; to display the character "temp"
    dec counter_1
    jmp display_name_loop

// spend $counter_2$ on traveling to next station
travel_loop:
    cpi counter_2, 0
    breq end_travel_loop
    ldi tempMain, 0
    dec counter_2

    // this loop is to count for 1 sec
    travel_loop_one_sec:
        sleep 100
        sbic PINC, 0                    ; skip "not_stop" if PB0 or PB1 is pressed
        jmp not_stop
        jmp stop

    stop:
        ldi temp2, 1                    ; 1 is load so the monorail will stop at next station
    not_stop:
        cpi tempMain, 10
        breq travel_loop
        inc tempMain
        jmp travel_loop_one_sec


// after $counter_2$, monorail should reach the next station
end_travel_loop:
    cpi temp, 0
    breq /* next loop */

    clearDisplay
    do_lcd_data 'S'
    do_lcd_data 'T'
    do_lcd_data 'O'
    do_lcd_data 'P'

    ldi temp2 (0<<PE4)                   ; turn off the motor
    out PORTE, temp2
    out DDRE, temp2

    clr tempMain

stop:
    cp tempMain, mask
    breq /* next loop */
    inc tempMain

    rcall blink
    rcall blink
    rcall blink
    sleep 100

    jmp stop

/* ========== EXTRA STEP FOR HASH PRESSING ========== */
hash_handle:
    sleep 500
    ldi temp2, 1
    cpi currstatus, temp2               ; if currstatus is 1 (running), then it need to stop
    jmp stop__
    ldi temp2, 0
    cpi currstatus, 0                   ; if currstatus is 0 (stop), then it need to run
    jmp

stop__:

TimerOVF:
    in timerTemp, SREG
    push temp                           ; prologue starts
    push YH                             ; save all conflicting registers in the prologue
    push YL
    push r25
    push r24                            ; prologue ends

    ; load the value of the temporary counter
    lds r24, TempCounter
    lds r25, TempCounter+1
    adiw r25:r24, 1
    cpi r24, low()
