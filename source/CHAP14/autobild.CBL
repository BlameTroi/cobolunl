       IDENTIFICATION DIVISION.
       PROGRAM-ID.  AUTOBILD.                                            (7)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUTO-DATA-FILE       ASSIGN TO "AUTODATA.DAT"
                                       LINE SEQUENTIAL.
           SELECT AUTO-INFO-FILE       ASSIGN TO "AUTOINFO"
                                       ORGANIZATION INDEXED
                                       ACCESS MODE DYNAMIC
                                       RECORD KEY AUTO-KEY
                                       FILE STATUS AUTO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  AUTO-INFO-FILE.
       01  AUTO-RECORD.
           05  AUTO-KEY.
               10  AUTO-YEAR            PIC XX.
               10  AUTO-BRAND           PIC X(10).
           05  FILLER                   PIC X(68).
       FD  AUTO-DATA-FILE.
       01  AUTO-DATA-RECORD             PIC X(80).
       WORKING-STORAGE SECTION.
       01  AUTO-STATUS                  PIC XX VALUE '00'.
       01  AUTO-DATA-EOF-STATUS         PIC X  VALUE 'N'.
           88  AUTO-EOF                 VALUE 'Y'.
       PROCEDURE DIVISION.                                                (10)
       0100-MAIN.
           OPEN OUTPUT AUTO-INFO-FILE
                 INPUT AUTO-DATA-FILE.
           READ AUTO-DATA-FILE
               AT END
                  MOVE 'Y' TO AUTO-DATA-EOF-STATUS.
           PERFORM UNTIL AUTO-EOF
               MOVE AUTO-DATA-RECORD TO AUTO-RECORD
               WRITE AUTO-RECORD INVALID KEY
                   DISPLAY 'BAD WRITE, STATUS: ' AUTO-STATUS
                     ', KEY: ' AUTO-KEY
               END-WRITE
               READ AUTO-DATA-FILE
                   AT END
                      MOVE 'Y' TO AUTO-DATA-EOF-STATUS
               END-READ
           END-PERFORM.
           CLOSE AUTO-INFO-FILE
                 AUTO-DATA-FILE.
           STOP RUN.                                                     (11)
