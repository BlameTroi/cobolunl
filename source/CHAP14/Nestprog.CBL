       IDENTIFICATION DIVISION.                                         (1)
       PROGRAM-ID.  NESTPROG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLICY-INPUT         ASSIGN TO "POLICYIN.DAT"
                                       LINE SEQUENTIAL.
           SELECT POLICY-EDITED        ASSIGN TO "POLICYED.DAT".

       DATA DIVISION.
       FILE SECTION.
       FD  POLICY-INPUT.
       01  POLICY-RECORD                   PIC X(80).
       FD  POLICY-EDITED.
       01  GOOD-POLICY-RECORD              PIC X(80).
       WORKING-STORAGE SECTION.
       01  POLICY-INFO                     GLOBAL.                      (2)
           05  POLICY-TYPE                 PIC XXXX.
           05  POLICY-NUMBER               PIC X(12).
           05  POLICY-UPDATE-CODE          PIC XXXX.
           05  POLICY-DATE                 PIC X(8).
           05  POLICY-HOLDER               PIC X(20).
           05  POLICY-MISC-INFO            PIC X(12).
           05  FILLER                      PIC X(20).
       01  EDIT-STATUS                     PIC X.
       01  VALID-STATUS                    PIC X.
       01  ERROR-CODE                      PIC 999.
       01  POLICY-EOF-STATUS               PIC X VALUE 'N'.
           88  POLICY-EOF                  VALUE 'Y'.
       PROCEDURE DIVISION.
       0100-MAIN-CONTROL.
           OPEN INPUT POLICY-INPUT
                OUTPUT POLICY-EDITED.
           PERFORM 0800-READ-POLICY-CHANGES.
           PERFORM 0200-EDIT-CONTROL
               UNTIL POLICY-EOF.
           CLOSE POLICY-INPUT
                 POLICY-EDITED.
           CALL 'NSTSUB2C'.                                             (3)
           CALL 'NSTSUBXC'.
           STOP RUN.
       0200-EDIT-CONTROL.
           MOVE 'Y' TO EDIT-STATUS.
           CALL 'DATEEDIT' USING POLICY-DATE EDIT-STATUS.               (4)
           IF EDIT-STATUS = 'Y'
             EVALUATE POLICY-TYPE
                WHEN 'LIFE'
                  CALL 'NSTSUB1' USING EDIT-STATUS                      (5)
                WHEN 'AUTO'
                  CALL 'NSTSUB2' USING EDIT-STATUS                      (6)
                WHEN 'HLTH'
                  CALL 'NSTSUB3' USING EDIT-STATUS                      (7)
                WHEN OTHER
                  MOVE 024 TO ERROR-CODE
                  CALL 'NSTSUBX' USING ERROR-CODE                       (8)
              END-EVALUATE
           ELSE
              MOVE 023 TO ERROR-CODE
              CALL 'NSTSUBX' USING ERROR-CODE                           (9)
           END-IF.
           IF EDIT-STATUS = 'Y'
              WRITE GOOD-POLICY-RECORD FROM POLICY-INFO.
           PERFORM 0800-READ-POLICY-CHANGES.
       0800-READ-POLICY-CHANGES.
           READ POLICY-INPUT INTO POLICY-INFO
              AT END
                 MOVE 'Y' TO POLICY-EOF-STATUS.

       IDENTIFICATION DIVISION.                                         (10)
       PROGRAM-ID.  NSTSUB1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERROR-CODE-NEW-NAME             PIC 999.
       LINKAGE SECTION.
       01  MY-STATUS                       PIC X.
       PROCEDURE DIVISION USING MY-STATUS.
       0100-MAIN-CONTROL.
           IF POLICY-HOLDER  = POLICY-MISC-INFO
               MOVE 'N' TO MY-STATUS
               MOVE 101 TO ERROR-CODE-NEW-NAME
               CALL 'NSTSUBX' USING ERROR-CODE-NEW-NAME.
           EXIT PROGRAM.
       END PROGRAM  NSTSUB1.
       IDENTIFICATION DIVISION.                                         (11)
       PROGRAM-ID.  NSTSUB2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERROR-CODE                      PIC 999.
       01  RECORD-FOUND                    PIC X.
       LINKAGE SECTION.
       01  EDIT-STATUS                     PIC X.
       PROCEDURE DIVISION USING EDIT-STATUS.
       0100-MAIN-CONTROL.
           CALL 'NSTSUB2A' USING RECORD-FOUND POLICY-MISC-INFO.
           IF RECORD-FOUND = 'N'
               MOVE 'N' TO EDIT-STATUS
               MOVE 201 TO ERROR-CODE
               CALL 'NSTSUBX' USING ERROR-CODE.
           EXIT PROGRAM.
       0200-PASS-CLOSE.
           ENTRY 'NSTSUB2C'.
           CALL 'NSTSUB2B'.
           EXIT PROGRAM.
       IDENTIFICATION DIVISION.                                         (12)
       PROGRAM-ID.  NSTSUB2A.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUTO-FILE            ASSIGN TO "AUTOINFO"
                                       ORGANIZATION INDEXED
                                       ACCESS MODE DYNAMIC
                                       RECORD KEY AUTO-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  AUTO-FILE.
       01  AUTO-RECORD.
           05  AUTO-KEY                    PIC X(12).
           05  FILLER                      PIC X(68).
       WORKING-STORAGE SECTION.
       01  FIRST-TIME-STATUS               PIC X VALUE 'Y'.
       LINKAGE SECTION.
       01  FOUND-STATUS                    PIC X.
       01  AUTO-TYPE-YEAR                  PIC X(12).
       PROCEDURE DIVISION USING FOUND-STATUS AUTO-TYPE-YEAR.
       0100-MAIN-CONTROL.
            IF FIRST-TIME-STATUS = 'Y'
               OPEN INPUT AUTO-FILE
               MOVE 'N' TO FIRST-TIME-STATUS.
            MOVE AUTO-TYPE-YEAR TO AUTO-KEY.
            MOVE 'Y' TO FOUND-STATUS.
            READ AUTO-FILE
                INVALID KEY
                   MOVE 'N' TO FOUND-STATUS.
            EXIT PROGRAM.
       0300-CLOSE-FILE.
           ENTRY 'NSTSUB2B'.
           IF FIRST-TIME-STATUS NOT = 'Y'
              CLOSE AUTO-FILE.
           EXIT PROGRAM.

       END PROGRAM  NSTSUB2A.
       END PROGRAM  NSTSUB2.

       IDENTIFICATION DIVISION.                                         (13)
       PROGRAM-ID.  NSTSUB3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-CODE                        PIC 999.
       LINKAGE SECTION.
       01  EDIT-STATUS                     PIC X.
       PROCEDURE DIVISION USING EDIT-STATUS.
       0100-MAIN-CONTROL.
           IF POLICY-MISC-INFO = SPACES
               MOVE 301 TO ERR-CODE
               CALL 'NSTSUBX' USING ERR-CODE
               MOVE 'N' TO EDIT-STATUS.
           EXIT PROGRAM.
       END PROGRAM  NSTSUB3.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  NSTSUBX  COMMON  PROGRAM.                           (14)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERROR-FILE           ASSIGN TO "ERRINFO.DAT".
       DATA DIVISION.
       FILE SECTION.
       FD  ERROR-FILE.
       01  ERROR-RECORD.
           05  ERROR-INFO                  PIC X(80).
           05  ERROR-REASON                PIC X(20).
       WORKING-STORAGE SECTION.
       01  FIRST-TIME-STATUS               PIC X VALUE 'Y'.
       LINKAGE SECTION.
       01  ERROR-CODE                      PIC 999.
       PROCEDURE DIVISION USING ERROR-CODE.
       0100-MAIN-CONTROL.
            IF FIRST-TIME-STATUS = 'Y'
                OPEN OUTPUT ERROR-FILE
                MOVE 'N' TO FIRST-TIME-STATUS.
            MOVE POLICY-INFO TO ERROR-INFO.
            MOVE ERROR-CODE TO ERROR-REASON.
            WRITE ERROR-RECORD.
            EXIT PROGRAM.
       0300-CLOSE-FILE.
           ENTRY 'NSTSUBXC'.
           IF FIRST-TIME-STATUS NOT = 'Y'
              CLOSE ERROR-FILE.
           EXIT PROGRAM.
       END PROGRAM  NSTSUBX.
       END PROGRAM  NESTPROG.
