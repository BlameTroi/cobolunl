       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Calculat.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  LOOP-CONTROL                 PIC X VALUE 'N'.
           88  USER-IS-DONE             VALUE 'Y'.
       01  COMPUTE-INFO.
           05  FIELD-1                  PIC 9(18).
           05  FILLER                   PIC X.
           05  OPERATION                PIC X.
           05  FILLER                   PIC X.
           05  FIELD-2                  PIC 9(18).
       01  RESULT                       PIC 9(18).
       01  SHOW-RESULT     PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0100-MAIN.
           PERFORM 0200-CALCULATOR-CONTROL
               UNTIL USER-IS-DONE.
           STOP RUN.
       0200-CALCULATOR-CONTROL.
           DISPLAY 'ENTER 18-DIGIT NUMBERS SEPARATED BY OPERATION AS'.
           DISPLAY '123456789012345678 X 123456789012345678'.
           ACCEPT COMPUTE-INFO.
           MOVE ZERO TO RESULT.
           IF OPERATION = '+'
               ADD FIELD-1 FIELD-2 GIVING RESULT
           ELSE
           IF OPERATION = '-'
               SUBTRACT FIELD-2 FROM FIELD-1 GIVING RESULT
           ELSE
           IF OPERATION = '*'
               MULTIPLY FIELD-1 BY FIELD-2 GIVING RESULT
           ELSE
           IF OPERATION = '/'
               DIVIDE FIELD-1 BY FIELD-2 GIVING RESULT
           ELSE
           IF OPERATION = '^'
               MOVE 1 TO RESULT
               PERFORM 0210-EXPONENT FIELD-2 TIMES.
           MOVE RESULT TO SHOW-RESULT.
           DISPLAY ' THE ANSWER IS: ' SHOW-RESULT.
           DISPLAY 'ARE YOU FINISHED WITH YOUR CALCULATIONS? (Y/N): '.
           ACCEPT LOOP-CONTROL.
       0210-EXPONENT.
           MULTIPLY RESULT BY FIELD-1 GIVING RESULT.

