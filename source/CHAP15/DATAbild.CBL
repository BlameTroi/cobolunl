       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DATABILD.                                            (7)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORDER-DATA-FILE      ASSIGN TO "INDATA2S.DAT"
                                       LINE SEQUENTIAL.
           SELECT ORDER-INFO-FILE      ASSIGN TO "INDATA2"
                                       ORGANIZATION INDEXED
                                       ACCESS MODE DYNAMIC
                                       RECORD KEY ORDER-KEY
                                       FILE STATUS ORDER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  ORDER-INFO-FILE.
       01  ORDER-RECORD.
           05  ORDER-KEY.
               10  ORDER-DATE           PIC 9(8).
               10  ORDER-NUMBER         PIC 9(6).
           05  FILLER                   PIC X(66).
       FD  ORDER-DATA-FILE.
       01  ORDER-DATA-RECORD            PIC X(80).
       WORKING-STORAGE SECTION.
       01  ORDER-STATUS                 PIC XX VALUE '00'.
       01  ORDER-DATA-EOF-STATUS        PIC X  VALUE 'N'.
           88  ORDER-EOF                VALUE 'Y'.
       PROCEDURE DIVISION.                                                (10)
       0100-MAIN.
           OPEN OUTPUT ORDER-INFO-FILE
                 INPUT ORDER-DATA-FILE.
           READ ORDER-DATA-FILE
               AT END
                  MOVE 'Y' TO ORDER-DATA-EOF-STATUS.
           PERFORM UNTIL ORDER-EOF
               MOVE ORDER-DATA-RECORD TO ORDER-RECORD
               WRITE ORDER-RECORD INVALID KEY
                   DISPLAY 'BAD WRITE, STATUS: ' ORDER-STATUS
                     ', KEY: ' ORDER-KEY
               END-WRITE
               READ ORDER-DATA-FILE
                   AT END
                      MOVE 'Y' TO ORDER-DATA-EOF-STATUS
               END-READ
           END-PERFORM.
           CLOSE ORDER-INFO-FILE
                 ORDER-DATA-FILE.
           STOP RUN.                                                     (11)
