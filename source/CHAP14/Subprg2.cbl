       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SUBPRG2.                                             (7)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AGENT-INFO-FILE      ASSIGN TO "AGTINFO"
                                       ORGANIZATION INDEXED
                                       ACCESS MODE DYNAMIC
                                       RECORD KEY AGENT-KEY
                                       FILE STATUS AGENT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  AGENT-INFO-FILE.
       01  AGENT-RECORD.
           05  AGENT-KEY.
               10  AGENT-STATE          PIC XX.
               10  AGENT-TERRITORY      PIC X(20).
           05  FILLER                   PIC X(58).
       WORKING-STORAGE SECTION.
       01  AGENT-STATUS                 PIC XX VALUE '00'.
       01  FIRST-TIME-STATUS            PIC X  VALUE 'Y'.                (8)
       LINKAGE SECTION.                                                  (9)
       01  CALL-FUNCTION                PIC X.
       01  CALL-RETURN                  PIC XX.
       01  CALL-INFO                    PIC X(80).
       PROCEDURE DIVISION USING CALL-FUNCTION CALL-RETURN CALL-INFO.      (10)
       0100-MAIN.
           IF FIRST-TIME-STATUS = 'N'
               PERFORM 0200-PROCESS-REQUEST
           ELSE
               MOVE 'N' TO FIRST-TIME-STATUS
               OPEN INPUT AGENT-INFO-FILE
               IF AGENT-STATUS  = '00'
                  PERFORM 0200-PROCESS-REQUEST
               ELSE
                  DISPLAY 'UNABLE TO OPEN AGENT FILE, STATUS: '
                      AGENT-STATUS
                  DISPLAY '**PLEASE CALL HELP DESK**'
                  MOVE '99' TO CALL-RETURN.
           EXIT PROGRAM.                                                 (11)
       0200-PROCESS-REQUEST.
           EVALUATE CALL-FUNCTION
             WHEN 'C'
               CLOSE AGENT-INFO-FILE
               MOVE '00' TO CALL-RETURN
             WHEN 'R'
               PERFORM 0300-DIRECT-READ
             WHEN OTHER
               DISPLAY 'UNKNOWN REQUEST FUNCTION: ' CALL-FUNCTION
               DISPLAY '**PLEASE CALL HELP DESK**'
               MOVE '99' TO CALL-RETURN.
       0300-DIRECT-READ.
           MOVE CALL-INFO TO AGENT-RECORD.
           READ AGENT-INFO-FILE INTO CALL-INFO.
           IF AGENT-STATUS = '00'
               MOVE '00' TO CALL-RETURN
           ELSE
           IF AGENT-STATUS = '23'
               MOVE '01' TO CALL-RETURN
           ELSE
               DISPLAY 'AGENT FILE SYSTEM ERROR, STATUS: ' AGENT-STATUS
               DISPLAY '**PLEASE CALL HELP DESK**'
               MOVE '99' TO CALL-RETURN.
