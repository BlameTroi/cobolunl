       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Segments.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       OBJECT-COMPUTER. BRANDXY
           SEGMENT-LIMIT 30.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAIN-INPUT           ASSIGN TO 'MAININP.DAT'
                                       LINE SEQUENTIAL.
           SELECT EDIT-TABLE-FILE      ASSIGN TO 'EDITTBL.DAT'.
           SELECT EDIT-OUTPUT          ASSIGN TO 'EDITOUT.DAT'.
           SELECT ERROR-REPORT         ASSIGN TO 'ERRLIST.DAT'.
       DATA DIVISION.
       FILE SECTION.
       FD  MAIN-INPUT.
       01  INPUT-RECORD.
           05  INPUT-TYPE              PIC X(4).
           05  AUTO-RENEWAL-DATE       PIC X(8).
           05  POLICY-ACTIVATION-DATE  PIC X(8).
           05  INPUT-OTHER-DATA        PIC X(60).
       FD  EDIT-TABLE-FILE.
       01  EDIT-RECORD                 PIC X(80).
       FD  EDIT-OUTPUT.
       01  EDITED-RECORD               PIC X(80).
       FD  ERROR-REPORT.
       01  ERROR-RECORD                PIC X(80).

       WORKING-STORAGE SECTION.
       01  INPUT-EOF-STATUS            PIC X VALUE 'N'.
           88  INPUT-EOF               VALUE 'Y'.
       01  ERROR-CODE                  PIC 9(5) VALUE ZERO.
       01  DATE-FIELD                  PIC X(8).
       01  DATE-STATUS                 PIC 99   VALUE ZERO.
      *
       PROCEDURE DIVISION.
       0000-MAIN SECTION 00.
       0000-MAIN-PARA.
           PERFORM 0010-INITIAL.
           PERFORM 1000-MAIN-LOOP
               UNTIL INPUT-EOF.
           PERFORM 9010-FINAL.
           STOP RUN.

       0010-INITIAL SECTION 90.
       0010-INITIAL-PARA.
           OPEN INPUT MAIN-INPUT
                      EDIT-TABLE-FILE
               OUTPUT EDIT-OUTPUT
                      ERROR-REPORT.
           PERFORM 0100-READ-MAIN-INPUT.

       0100-READ-MAIN-INPUT SECTION 00.
       0100-READ-MAIN-INPUT-PARA.
           READ MAIN-INPUT
               AT END
                  MOVE 'Y' TO INPUT-EOF-STATUS.

       1000-MAIN-LOOP SECTION 00.
       1000-MAIN-LOOP-PARA.
            EVALUATE INPUT-TYPE
               WHEN 'AUTO'
                 PERFORM 2000-AUTO-POLICY-EDIT
               WHEN 'LIFE'
                 PERFORM 3000-LIFE-POLICY-EDIT
               WHEN OTHER
                 MOVE 1234 TO ERROR-CODE
                 PERFORM 1500-HANDLE-ERROR
             END-EVALUATE.
             PERFORM 0100-READ-MAIN-INPUT.

       1200-EDIT-DATES SECTION 22.
       1200-EDIT-DATE-PARA.
           EXIT.
      *   Date editing routine

       1500-HANDLE-ERROR SECTION 45.
       1500-HANDLE-ERROR-PARA.
           EXIT.
      *   Format and write reject into to report

       2000-AUTO-POLICY-EDIT SECTION 60.
       2000-AUTO-POLICY-EDIT-PARA.
      *   Edit details for Automobile Insurance policy transactions
      *   Including:
            MOVE AUTO-RENEWAL-DATE TO DATE-FIELD.
            PERFORM 1200-EDIT-DATES.
            IF DATE-STATUS NOT = ZERO
                MOVE DATE-STATUS TO ERROR-CODE
                PERFORM 1500-HANDLE-ERROR.
      *     More editing

       3000-LIFE-POLICY-EDIT SECTION 70.
       3000-LIFE-POLICY-EDIT-PARA.

      *    Edit details for Life Insurance policy transactions
      *    Including:
            MOVE POLICY-ACTIVATION-DATE TO DATE-FIELD.
            PERFORM 1200-EDIT-DATES.
            IF DATE-STATUS NOT = ZERO
                MOVE DATE-STATUS TO ERROR-CODE
                PERFORM 1500-HANDLE-ERROR.
      *   More editing
      *
       9010-FINAL SECTION 90.
       9010-FINAL-PARA.
           CLOSE MAIN-INPUT
                 EDIT-TABLE-FILE
                 EDIT-OUTPUT
                 ERROR-REPORT.
