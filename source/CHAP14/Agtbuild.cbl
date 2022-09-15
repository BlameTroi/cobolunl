       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SUBPRG2.                                             (7)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AGENT-DATA-FILE      ASSIGN TO "AGTDATA.DAT"
                                       LINE SEQUENTIAL.
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
       FD  AGENT-DATA-FILE.
       01  AGENT-DATA-RECORD            PIC X(80).
       WORKING-STORAGE SECTION.
       01  AGENT-STATUS                 PIC XX VALUE '00'.
       01  AGENT-DATA-EOF-STATUS        PIC X  VALUE 'N'.
           88  AGENT-EOF                VALUE 'Y'.
       PROCEDURE DIVISION.                                                (10)
       0100-MAIN.
           OPEN OUTPUT AGENT-INFO-FILE
                 INPUT AGENT-DATA-FILE.
           READ AGENT-DATA-FILE
               AT END
                  MOVE 'Y' TO AGENT-DATA-EOF-STATUS.
           PERFORM UNTIL AGENT-EOF
               MOVE AGENT-DATA-RECORD TO AGENT-RECORD
               WRITE AGENT-RECORD INVALID KEY
                   DISPLAY 'BAD WRITE, STATUS: ' AGENT-STATUS
                     ', KEY: ' AGENT-KEY
               END-WRITE
               READ AGENT-DATA-FILE
                   AT END
                      MOVE 'Y' TO AGENT-DATA-EOF-STATUS
               END-READ
           END-PERFORM.
           CLOSE AGENT-INFO-FILE
                 AGENT-DATA-FILE.
           STOP RUN.                                                     (11)
