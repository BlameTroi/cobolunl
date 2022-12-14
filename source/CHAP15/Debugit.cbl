       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DEBUGIT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  BRANDXY
                         WITH DEBUGGING MODE
                         .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE           ASSIGN TO "INDATA1B.DAT"
                                       LINE SEQUENTIAL.
           SELECT OUTPUT-FILE          ASSIGN TO "OUTDATA.DAT".
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD              PIC X(80).
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD             PIC X(80).

       WORKING-STORAGE SECTION.
       01  INPUT-RECORD-COUNT        PIC 9(6) VALUE ZERO.
       01  OUTPUT-RECORD-COUNT       PIC 9(6) VALUE ZERO.
       01  SEQUENCE-KEY              PIC X(20) VALUE LOW-VALUES.
       01  DEBUG-PREV-KEY            PIC X(20) VALUE LOW-VALUES.
       01  INPUT-EOF-STATUS          PIC X VALUE 'N'.
           88  INPUT-EOF             VALUE 'Y'.
       PROCEDURE DIVISION.
       DECLARATIVES.
       0000A-INPUT-FILE-DECL SECTION.
            USE FOR DEBUGGING ON INPUT-FILE.
       0000A-INPUT-FILE-PROC.
            ADD 1 TO INPUT-RECORD-COUNT.
            IF INPUT-RECORD-COUNT > 4
                DISPLAY '**RECORD: ' INPUT-RECORD-COUNT
                DISPLAY  ' ' DEBUG-ITEM.
       0000B-PROCEDURE-DECL SECTION.
            USE FOR DEBUGGING ON ALL PROCEDURES.
       0000B-PROCEDURE-PROC.
            IF INPUT-RECORD-COUNT > 4
                DISPLAY  ' ' DEBUG-ITEM.
       0000C-DATA-DECL SECTION.
            USE FOR DEBUGGING ON SEQUENCE-KEY ALL OUTPUT-RECORD
                     OUTPUT-RECORD-COUNT.
       0000C-DATA-PROC.
            IF DEBUG-PREV-KEY > SEQUENCE-KEY
                DISPLAY '***SOMETHING IS REALLY SCREWY***'
                     ', PREV KEY: ' DEBUG-PREV-KEY
                DISPLAY '  SEQUENCE KEY: ' SEQUENCE-KEY
                DISPLAY DEBUG-ITEM.
            MOVE SEQUENCE-KEY TO DEBUG-PREV-KEY.
            PERFORM 0000B-PROCEDURE-PROC.
       END DECLARATIVES.
       0100-MAIN-PROGRAM SECTION.
       0100-MAIN-CONTROL.
            OPEN INPUT INPUT-FILE
                 OUTPUT OUTPUT-FILE.
            PERFORM 0200-READ-INPUT.
            PERFORM 0300-LOOP-IT
                UNTIL INPUT-EOF.
            CLOSE INPUT-FILE
                  OUTPUT-FILE.
            STOP RUN.
       0200-READ-INPUT.
            READ INPUT-FILE
                AT END
                   MOVE 'Y' TO INPUT-EOF-STATUS.
       0300-LOOP-IT.
            MOVE INPUT-RECORD TO OUTPUT-RECORD.
            PERFORM 0400-WRITE-IT.
            PERFORM 0200-READ-INPUT.
       0400-WRITE-IT.
            WRITE OUTPUT-RECORD.

