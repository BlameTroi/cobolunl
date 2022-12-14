       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Sort2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAILING-INFO         ASSIGN TO "INDATA1B.DAT"
                                       LINE SEQUENTIAL.
           SELECT MAILING-OUTPUT       ASSIGN TO "OUTDATA.DAT"
                                       LINE SEQUENTIAL.
           SELECT SORT-WORK            ASSIGN TO SORTWORK.

       DATA DIVISION.
       FILE SECTION.
       FD  MAILING-INFO.
       01  MAILING-INFO-RECORD.
           05  FILLER                    PIC X(7).
           05  MAILING-ADDRESS           PIC X(60).
           05  FILLER                    PIC X(13).
       FD  MAILING-OUTPUT.
       01  MAILING-OUTPUT-RECORD.
           05  MAIL-CONTROL-BREAK-CODE    PIC XXX.
           05  MAIL-BREAK-ZIP-CODE        PIC X(5).
           05  MAIL-BREAK-COUNT           PIC 9(9).
           05  FILLER                     PIC X(63).
       01  MAILING-PRINT-RECORD           PIC X(80).

       SD  SORT-WORK.
       01  SORT-RECORD.
           05  SORT-FULL-ZIP-CODE         PIC X(9).
           05  SORT-MAILING-INFO          PIC X(71).
       WORKING-STORAGE SECTION.
       01  END-INPUT-STATUS               PIC XXX VALUE 'NO '.
           88  END-INPUT                          VALUE 'YES'.
       01  END-SORT-STATUS                PIC XXX VALUE 'NO '.
           88  END-SORT                           VALUE 'YES'.
       01  PREVIOUS-ZIP-CODE              PIC X(5) VALUE ZERO.
       01  ZIP-CODE-COUNT                 PIC 9(9) VALUE ZERO.
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           SORT SORT-WORK
               ASCENDING KEY SORT-FULL-ZIP-CODE
               INPUT PROCEDURE 1000-RELEASE-CONTROL
               OUTPUT PROCEDURE 2000-RETURN-CONTROL.
           STOP RUN.
       1000-RELEASE-CONTROL.
           OPEN INPUT MAILING-INFO.
           PERFORM 1200-READ-MAIL-INPUT.
           PERFORM 1100-READ-RELEASE-LOOP
               UNTIL END-INPUT.
           CLOSE MAILING-INFO.
       1100-READ-RELEASE-LOOP.
           CALL 'FIND-ZIP' USING MAILING-ADDRESS SORT-FULL-ZIP-CODE.
           MOVE MAILING-INFO-RECORD TO SORT-MAILING-INFO.
           RELEASE SORT-RECORD.
           PERFORM 1200-READ-MAIL-INPUT.
       1200-READ-MAIL-INPUT.
           READ MAILING-INFO
               AT END
                 MOVE 'YES' TO END-INPUT-STATUS.
       2000-RETURN-CONTROL.
           OPEN OUTPUT MAILING-OUTPUT.
           PERFORM 2200-RETURN-SORT-RECORD.
           MOVE SORT-FULL-ZIP-CODE (1:5) TO PREVIOUS-ZIP-CODE.
           PERFORM 2100-PROCESS-OUTPUT-LOOP
               UNTIL END-SORT.
           MOVE 'XXX' TO MAIL-CONTROL-BREAK-code.
           MOVE PREVIOUS-ZIP-CODE TO MAIL-BREAK-ZIP-CODE.
           MOVE ZIP-CODE-COUNT TO MAIL-BREAK-COUNT.
           WRITE MAILING-OUTPUT-RECORD.
           CLOSE MAILING-OUTPUT.
       2100-PROCESS-OUTPUT-LOOP.
           IF SORT-FULL-ZIP-CODE (1:5) NOT = PREVIOUS-ZIP-CODE
               MOVE 'XXX' TO MAIL-CONTROL-BREAK-code
               MOVE PREVIOUS-ZIP-CODE TO MAIL-BREAK-ZIP-CODE
               MOVE ZIP-CODE-COUNT TO MAIL-BREAK-COUNT
               WRITE MAILING-OUTPUT-RECORD
               MOVE ZERO TO ZIP-CODE-COUNT
               MOVE SORT-FULL-ZIP-CODE  (1:5) TO PREVIOUS-ZIP-CODE.
           PERFORM 2500-FORMAT-PRINT-OUTPUT.
           ADD 1 TO ZIP-CODE-COUNT.
           PERFORM 2200-RETURN-SORT-RECORD.
       2200-RETURN-SORT-RECORD.
           RETURN SORT-WORK
               AT END
                 MOVE 'YES' TO END-SORT-STATUS.
       2500-FORMAT-PRINT-OUTPUT.
      *    Formatting and printing mailing data will be done here.
           EXIT.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-ZIP.
       DATA DIVISION.
       LINKAGE SECTION.
       01  MAIL-ADDRESS                PIC X(60).
       01  FULL-ZIP.
           05  NUMERIC-FIRST5          PIC 9(5).
           05  NUMERIC-ZIP4            PIC V9(4).
       PROCEDURE DIVISION USING MAIL-ADDRESS FULL-ZIP.
           IF MAIL-ADDRESS > 'P'
              MOVE 70000 TO NUMERIC-FIRST5
           ELSE
           IF MAIL-ADDRESS > 'G'
              MOVE 30000 TO NUMERIC-FIRST5
           ELSE
              MOVE 00010 TO NUMERIC-FIRST5.
           COMPUTE NUMERIC-ZIP4 = FUNCTION RANDOM.
           EXIT PROGRAM.
       END PROGRAM FIND-ZIP.
       END PROGRAM Sort2.
