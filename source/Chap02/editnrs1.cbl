        IDENTIFICATION DIVISION.
        PROGRAM-ID.  EditNrs1.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  CALC-NUMBER             PIC 9(5)V99 VALUE 3245.12.
        01  EDIT-GRP1.
            05  EDIT-NR1            PIC 9(5).99.
        01  EDIT-GRP2.
            05  EDIT-NR2            PIC Z(5).ZZ.
        01  EDIT-GRP3.
            05  EDIT-NR3            PIC Z(4)9.99.
        01  EDIT-GRP4.
            05  EDIT-NR4            PIC ZZ,ZZ9.99.

        PROCEDURE DIVISION.
        0000-MAIN.
            PERFORM 0100-DISPLAY.
            MOVE ZERO TO CALC-NUMBER.
            PERFORM 0100-DISPLAY.
            MOVE 10.54 TO CALC-NUMBER.
            PERFORM 0100-DISPLAY.
            MOVE .2 TO CALC-NUMBER
            PERFORM 0100-DISPLAY.
            STOP RUN.
        0100-DISPLAY.
            MOVE CALC-NUMBER TO EDIT-NR1.
            MOVE CALC-NUMBER TO EDIT-NR2.
            MOVE CALC-NUMBER TO EDIT-NR3.
            MOVE CALC-NUMBER TO EDIT-NR4.
            DISPLAY '  9(5).99: ' EDIT-GRP1.
            DISPLAY '  Z(5).ZZ: ' EDIT-GRP2.
            DISPLAY ' Z(4)9.99: ' EDIT-GRP3.
            DISPLAY 'ZZ,ZZ9.99: ' EDIT-GRP4.

