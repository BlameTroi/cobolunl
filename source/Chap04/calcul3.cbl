       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Calcul3.
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
           PERFORM 0200-CALCULATOR-CONTROL
               TEST AFTER
               UNTIL USER-IS-DONE.
           STOP RUN.
       0200-CALCULATOR-CONTROL.
           DISPLAY 'ENTER 18-DIGIT NUMBERS SEPARATED BY OPERATION AS'.
           DISPLAY '123456789012345678 X 123456789012345678'.
           ACCEPT COMPUTE-INFO.
           MOVE ZERO TO RESULT.
           MOVE 'Y' TO DATA-VALID-INDICATOR.
           PERFORM 0300-EDIT-INPUT THRU 0300-EXIT.
           IF VALID-DATA
               PERFORM 0400-CALCULATE THRU 0400-EXIT
               MOVE RESULT TO SHOW-RESULT
               DISPLAY ' THE ANSWER IS: ' SHOW-RESULT.
           DISPLAY 'ARE YOU FINISHED WITH YOUR CALCULATIONS? (Y/N): '.
           ACCEPT LOOP-CONTROL.

       0300-EDIT-INPUT.
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
               GO TO 0300-EXIT.
           IF OPERATION = '/' AND FIELD-2 = ZERO
              DISPLAY 'DIVIDE BY ZERO NOT ALLOWED, RE-ENTER IF YOU WISH'
              MOVE 'N' TO DATA-VALID-INDICATOR.
       0300-EXIT.
           EXIT.

       0400-CALCULATE.
           IF OPERATION = '+'
               ADD FIELD-1 FIELD-2 GIVING RESULT
               GO TO 0400-EXIT.
           IF OPERATION = '-'
               SUBTRACT FIELD-2 FROM FIELD-1 GIVING RESULT
               GO TO 0400-EXIT.
           IF OPERATION = '*'
               MULTIPLY FIELD-1 BY FIELD-2 GIVING RESULT
                 ON SIZE ERROR
                   DISPLAY 'MULTIPLY OVERFLOW, SIGNIFICANT DIGITS LOST'
                   MULTIPLY FIELD-1 BY FIELD-2 GIVING RESULT
                   END-MULTIPLY
               END-MULTIPLY
               GO TO 0400-EXIT.
           IF OPERATION = '/'
               DIVIDE FIELD-1 BY FIELD-2 GIVING RESULT ROUNDED
                   ON SIZE ERROR
                     DISPLAY 'DIVIDE OVERFLOW, UNSPECIFIED RESULTS'
               END-DIVIDE
               GO TO 0400-EXIT.
           IF OPERATION = '^'
               MOVE 1 TO RESULT
               MOVE 'N' TO OVERFLOW-CONTROL
               PERFORM
                   VARYING POWER-VALUE FROM 1 BY 1
                   UNTIL POWER-VALUE > FIELD-2
                     MULTIPLY RESULT BY FIELD-1 GIVING RESULT
                       ON SIZE ERROR
                         DISPLAY 'OVERFLOW AT POWER = ' POWER-VALUE
                         SUBTRACT 1 FROM POWER-VALUE
                         DISPLAY 'VALUE AT ' POWER-VALUE ' WAS: '
                         MOVE FIELD-2 TO POWER-VALUE
                     END-MULTIPLY
               END-PERFORM
           END-IF.
       0400-EXIT.
           EXIT.
