        IDENTIFICATION DIVISION.
        PROGRAM-ID.  Numbers1.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  UNSIGN-AN.
            05  UNSIGNED-DIGIT        PIC 9 VALUE ZERO.
        01  NEGATIVE-AN.
            05  NEGATIVE-DIGIT        PIC S9.
        01  POSITIVE-AN.
            05  POSITIVE-DIGIT        PIC S9.
        PROCEDURE DIVISION.
        0000-MAIN.
            DISPLAY 'DIGIT NEGATIVE POSITIVE'.
            PERFORM 0100-LOOP 10 TIMES.
            STOP RUN.
        0100-LOOP.
            MULTIPLY UNSIGNED-DIGIT BY -1 GIVING NEGATIVE-DIGIT.
            MULTIPLY NEGATIVE-DIGIT BY -1 GIVING POSITIVE-DIGIT.
            DISPLAY '  ' UNSIGN-AN '      ' NEGATIVE-AN
                '         ' POSITIVE-AN.
            ADD 1 TO UNSIGNED-DIGIT.
