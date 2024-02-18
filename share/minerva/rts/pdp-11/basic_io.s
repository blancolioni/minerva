basic_io__put_char:
basic_io__put_ascii:
        bit #200, @#177564
        beq basic_io__put_ascii
        movb 2(sp), @#177566
        rts pc
