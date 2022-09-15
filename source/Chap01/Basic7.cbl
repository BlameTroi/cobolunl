        IDENTIFICATiON DIVISION.
        PROGRAM-ID.  Basic7.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  TOTAL-VALUE               PIC 99999 VALUE ZERO.
        77  INPUT-VALUE               PIC 99999 VALUE ZERO.
        PROCEDURE DIVISION.
        0000-MAIN.
           PERFORM 0100-LOOP
                UNTIL INPUT-VALUE = 99999.
           DISPLAY 'The Total is: ' TOTAL-VALUE.
           STOP RUN.
        0100-LOOP.
           DISPLAY 'Enter an Amount to Add (or 99999 if Done): '.
           ACCEPT INPUT-VALUE.
           IF INPUT-VALUE NOT NUMERIC
               DISPLAY 'Amount is NOT a Number, Re-enter'
               MOVE ZERO TO INPUT-VALUE.
           IF INPUT-VALUE NOT = 99999
               ADD INPUT-VALUE TO TOTAL-VALUE
               MOVE ZERO TO INPUT-VALUE.

