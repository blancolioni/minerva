    .var r0 = $80
    .var r1 = $82
    .var r2 = $84
    .var r3 = $86
    .var r4 = $88
    .var r5 = $8A
    .var r6 = $8C
    .var r7 = $8E

* = $c000
    lda #23
    sta 53272
    ldx #$ff
    txs
    jsr main
    jmp *

main:
