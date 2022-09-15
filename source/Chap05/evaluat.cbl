       IDENTIFICATION DIVISION.
       PROGRAM-ID. Evaluat.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CHOICE                  PIC X.
       01  TAXABLE-WAGES           PIC 9(5)V99.
       01  FEDERAL-TAXES           PIC 9(5)V99.
       01  TRANSACTION-FUNCTION    PIC X(8).
       01  USER-PERMISSION         PIC X(8).
       01  FIELD-LIST.
           05  FIELD-A             PIC X.
           05  FIELD-B             PIC X.
           05  FIELD-C             PIC X.
           05  FIELD-D             PIC X.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0000-LOOP
               TEST AFTER
               UNTIL CHOICE = 'Q'.
           STOP RUN.
       0000-LOOP.
           DISPLAY 'Enter Choice (Q to quit)'.
           ACCEPT CHOICE.
           EVALUATE CHOICE
               WHEN '1'
                 DISPLAY 'Enter Wages-9(5)99'
                 ACCEPT TAXABLE-WAGES
                 PERFORM 0100-EVALUATE
                 DISPLAY 'TAX IS: ' FEDERAL-TAXES
               WHEN '2'
                 DISPLAY 'Enter Function:'
                 ACCEPT TRANSACTION-FUNCTION
                 PERFORM 0200-EVALUATE
               WHEN '3'
               WHEN '5'
                 DISPLAY 'Enter Function:'
                 ACCEPT TRANSACTION-FUNCTION
                 DISPLAY 'Enter User Permission:'
                 ACCEPT USER-PERMISSION
                 IF CHOICE = '3'
                   PERFORM 0300-EVALUATE
                 ELSE
                   PERFORM 0500-EVALUATE
                 END-IF
               WHEN '4'
                 DISPLAY 'Enter Fields A,B,C,D (1 char each):'
                 ACCEPT FIELD-LIST
                 PERFORM 0400-EVALUATE
               WHEN 'Q'
                 CONTINUE
               WHEN OTHER
                 DISPLAY 'Bad Choice, Try Again.'
           END-EVALUATE.
       0100-EVALUATE.
           EVALUATE TAXABLE-WAGES
               WHEN ZERO THRU 200
                   MULTIPLY TAXABLE-WAGES BY .15 GIVING FEDERAL-TAXES
               WHEN 200 THRU 300
                   MULTIPLY TAXABLE-WAGES BY .22 GIVING FEDERAL-TAXES
               WHEN 300 THRU 400
                   MULTIPLY TAXABLE-WAGES BY .28 GIVING FEDERAL-TAXES
               WHEN 400 THRU 600
                   MULTIPLY TAXABLE-WAGES BY .32 GIVING FEDERAL-TAXES
               WHEN OTHER
                   MULTIPLY TAXABLE-WAGES BY .36 GIVING FEDERAL-TAXES
           END-EVALUATE.
       0200-EVALUATE.
           EVALUATE TRANSACTION-FUNCTION
               WHEN 'DISPLAY'
                 DISPLAY '0100-SHOW-RECORD THRU 0100-EXIT'
               WHEN 'INSERT'
                 DISPLAY '0200-INSERT-NEW-RECORD THRU 0200-EXIT'
               WHEN 'MODIFY'
               WHEN 'UPDATE'
                DISPLAY '0300-UPDATE-CONTROL THRU 0300-EXIT'
               WHEN 'DELETE'
                DISPLAY '0400-DELETE-RECORD THRU 0400-EXIT'
               WHEN OTHER
                DISPLAY 'UNKNOWN FUNCTION' 
           END-EVALUATE.
       0300-EVALUATE.
           EVALUATE TRUE
               WHEN TRANSACTION-FUNCTION = 'DISPLAY'
                   DISPLAY '0100-SHOW-RECORD THRU 0100-EXIT'
               WHEN TRANSACTION-FUNCTION = 'INSERT'
                 AND USER-PERMISSION = 'UPDATE'
                   DISPLAY '0200-INSERT-NEW-RECORD THRU 0200-EXIT'
               WHEN (TRANSACTION-FUNCTION = 'MODIFY'
                     OR TRANSACTION-FUNCTION = 'UPDATE')
                 AND USER-PERMISSION = 'UPDATE'
                   DISPLAY '0300-UPDATE-CONTROL THRU 0300-EXIT'
               WHEN TRANSACTION-FUNCTION = 'DELETE'
                 AND USER-PERMISSION = 'SUPER'
                   DISPLAY '0400-DELETE-RECORD THRU 0400-EXIT'
               WHEN OTHER
                   DISPLAY 'INVALID FUNCTION' 
           END-EVALUATE.
       0400-EVALUATE.
           EVALUATE FIELD-A = FIELD-B
               WHEN TRUE
                 DISPLAY '0100-PROCESS THRU 0100-EXIT'
                 EVALUATE FIELD-C = FIELD-D
                   WHEN TRUE
                     DISPLAY '0150-PROCESS THRU 0150-EXIT'
                 END-EVALUATE
               WHEN FALSE
                 DISPLAY '0200-PROCESS THRU 0200-EXIT'
               WHEN OTHER
                 DISPLAY 'IF YOU SEE THIS, COBOL IS BROKEN'
           END-EVALUATE.
       0500-EVALUATE.
           EVALUATE TRANSACTION-FUNCTION ALSO USER-PERMISSION
               WHEN 'DISPLAY' ALSO ANY
                 DISPLAY '0100-SHOW-RECORD THRU 0100-EXIT'
               WHEN 'INSERT'  ALSO 'UPDATE'
                 DISPLAY '0200-INSERT-NEW-RECORD THRU 0200-EXIT'
               WHEN 'MODIFY'  ALSO 'UPDATE'
               WHEN 'UPDATE'  ALSO 'UPDATE'
                 DISPLAY '0300-UPDATE-CONTROL THRU 0300-EXIT'
               WHEN 'DELETE'  ALSO 'SUPER'
                 DISPLAY '0400-DELETE-RECORD THRU 0400-EXIT'
               WHEN OTHER
                 DISPLAY 'INVALID FUNCTION' 
           END-EVALUATE.

