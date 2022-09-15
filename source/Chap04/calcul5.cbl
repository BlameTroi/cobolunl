       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Calcul5.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  LOOP-CONTROL                 PIC X VALUE 'N'.
           88  USER-IS-DONE             VALUE 'Y'.
       01  DATA-VALID-INDICATOR         PIC X VALUE 'Y'.
           88  VALID-DATA               VALUE 'Y'.
       01  COMPUTE-INFO.
           05  FIELD-1                  PIC 9(18).
           05  FILLER                   PIC X.
           05  OPERATION                PIC X.
               88  VALID-OPERATION      VALUE '+' '-' '*' '/' '^'.
           05  FILLER                   PIC X.
           05  FIELD-2                  PIC 9(18).
       01  RESULT                       PIC 9(18).
       01  POWER-VALUE                  PIC 9(18).
       01  SHOW-RESULT     PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
       01  OVERFLOW-CONTROL             PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       0100-MAIN.
           DISPLAY 'ENTER 18-DIGIT NUMBERS SEPARATED BY OPERATION AS'.
           DISPLAY '123456789012345678 X 123456789012345678'.
           ACCEPT COMPUTE-INFO.
           MOVE ZERO TO RESULT.
           MOVE 'Y' TO DATA-VALID-INDICATOR.
           IF FIELD-1 NOT NUMERIC
               DISPLAY 'FIRST FIELD NOT A VALID NUMBER'
               MOVE 'N' TO DATA-VALID-INDICATOR.
           IF FIELD-2 NOT NUMERIC
               DISPLAY 'SECOND FIELD NOT A VALID NUMBER'
               MOVE 'N' TO DATA-VALID-INDICATOR.
           IF NOT VALID-OPERATION
               DISPLAY 'UNKNOWN CALCULATION'
               MOVE 'N' TO DATA-VALID-INDICATOR.
           IF NOT VALID-DATA
               DISPLAY '  RE-ENTER IF YOU WISH'
               GO TO 0500-CHECK-MORE-DAYS.

           IF OPERATION = '/' AND FIELD-2 = ZERO
              DISPLAY 'DIVIDE BY ZERO NOT ALLOWED, RE-ENTER IF YOU WISH'
              GO TO 0500-CHECK-MORE-DAYS.

      *    EDITING COMPLETE, BEGIN CALCULATIONS

           IF OPERATION = '+'
               ADD FIELD-1 FIELD-2 GIVING RESULT
               GO TO 0400-SHOW-RESULT.
           IF OPERATION = '-'
               SUBTRACT FIELD-2 FROM FIELD-1 GIVING RESULT
               GO TO 0400-SHOW-RESULT.
           IF OPERATION NOT = '*'
               GO TO 0200-CHECK-DIVIDE.
           MULTIPLY FIELD-1 BY FIELD-2 GIVING RESULT
               ON SIZE ERROR
                 DISPLAY 'MULTIPLY OVERFLOW, SIGNIFICANT DIGITS LOST'
                 MULTIPLY FIELD-1 BY FIELD-2 GIVING RESULT.
           GO TO 0400-SHOW-RESULT.

       0200-CHECK-DIVIDE.
           IF OPERATION NOT = '/'
               GO TO 0300-CHECK-POWER.
           DIVIDE FIELD-1 BY FIELD-2 GIVING RESULT ROUNDED
               ON SIZE ERROR
                   DISPLAY 'DIVIDE OVERFLOW, UNSPECIFIED RESULTS'.
           GO TO 0400-SHOW-RESULT.

       0300-CHECK-POWER.
           IF OPERATION NOT = '^'
               GO TO 0400-SHOW-RESULT.
           MOVE 1 TO RESULT.
           MOVE 1 TO POWER-VALUE.

       0350-POWER-LOOP.
           IF POWER-VALUE > FIELD-2
               GO TO 0400-SHOW-RESULT.
           MULTIPLY RESULT BY FIELD-1 GIVING RESULT
               ON SIZE ERROR
                   DISPLAY 'OVERFLOW AT POWER = ' POWER-VALUE
                   SUBTRACT 1 FROM POWER-VALUE
                   DISPLAY 'VALUE AT ' POWER-VALUE ' WAS: '
                   GO TO 0400-SHOW-RESULT.
           ADD 1 TO POWER-VALUE.
           GO TO 0350-POWER-LOOP.

       0400-SHOW-RESULT.
           MOVE RESULT TO SHOW-RESULT.
           DISPLAY ' THE ANSWER IS: ' SHOW-RESULT.

       0500-CHECK-MORE-DAYS.
           DISPLAY 'ARE YOU FINISHED WITH YOUR CALCULATIONS? (Y/N): '.
           ACCEPT LOOP-CONTROL.
           IF USER-IS-DONE
               STOP RUN.
           GO TO 0100-MAIN.

