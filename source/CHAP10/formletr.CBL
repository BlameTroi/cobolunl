       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Formletr.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FORM-LETTER        ASSIGN TO "FORMFILE.DAT"
                                     LINE SEQUENTIAL.
           SELECT INSERTION-VALUES   ASSIGN TO "VALUFILE.DAT"
                                     LINE SEQUENTIAL.
           SELECT ACTUAL-LETTERS     ASSIGN TO "LETTERS.DAT".
       DATA DIVISION.
       FILE SECTION.
       FD  FORM-LETTER
           LABEL RECORDS STANDARD.
       01  FORM-LINE                  PIC X(80).
       FD  INSERTION-VALUES
           LABEL RECORDS STANDARD.
       01  VALUE-RECORD               PIC X(80).
       FD  ACTUAL-LETTERS.
       01  LETTER-LINE                PIC X(80).

       WORKING-STORAGE SECTION.
       01  VALUES-EOF-STATUS          PIC X VALUE 'N'.
           88  VALUES-EOF             VALUE 'Y'.
       01  FORMS-EOF-STATUS           PIC X VALUE 'N'.
           88  FORMS-EOF              VALUE 'Y'.
       01  FORM-LINE-SIZE             PIC 999 VALUE 80.
       01  LETTER-LINE-SIZE           PIC 999 VALUE 80.
       01  FORM-POINTER               PIC 999.
       01  LETTER-POINTER             PIC 999.
       01  VARIABLE-INFO.
           05  VARIABLE-1             PIC X(15).
           05  VARIABLE-2             PIC X(15).
           05  VARIABLE-3             PIC X(15).
           05  VARIABLE-4             PIC X(15).
           05  VARIABLE-5             PIC X(15).
           05                         PIC X(5).
       01  WORK-WORD .
           05  WORK-CHAR1-2           PIC XX.
           05  FILLER                 PIC X(50).
       01  WORD-SIZE                  PIC 99.

       PROCEDURE DIVISION.
       0100-MAIN-CONTROL.
           OPEN INPUT INSERTION-VALUES
                OUTPUT ACTUAL-LETTERS.
           PERFORM 0800-READ-INSERTION-VALUES.
           PERFORM 0200-PRODUCE-FORMS
                UNTIL VALUES-EOF.
           CLOSE INSERTION-VALUES
                 ACTUAL-LETTERS.
           STOP RUN.
       0200-PRODUCE-FORMS.
           OPEN INPUT FORM-LETTER.
           MOVE 'N' TO FORMS-EOF-status.
           PERFORM 0820-READ-FORMS-LETTER.
           MOVE 1 TO FORM-POINTER LETTER-POINTER.
           MOVE SPACES TO LETTER-LINE.
           PERFORM 0300-GENERATE-FORM
               UNTIL FORMS-EOF.
           PERFORM 0840-WRITE-LETTER-LINE.
           MOVE SPACES TO LETTER-LINE.
           WRITE LETTER-LINE AFTER ADVANCING PAGE.
           CLOSE FORM-LETTER.
           PERFORM 0800-READ-INSERTION-VALUES.

       0300-GENERATE-FORM.
           IF FORM-POINTER = 1
               INSPECT FORM-LINE 
                   TALLYING FORM-POINTER FOR LEADING SPACES
               IF FORM-POINTER > 1
                   IF LETTER-POINTER > 1
                     PERFORM 0840-WRITE-LETTER-LINE
                     MOVE SPACES TO LETTER-LINE
                   END-IF
                   MOVE FORM-POINTER TO LETTER-POINTER.
           MOVE SPACES TO WORK-WORD.
           UNSTRING FORM-LINE DELIMITED BY ALL SPACE
               INTO WORK-WORD COUNT IN WORD-SIZE
               POINTER FORM-POINTER.
           IF WORK-CHAR1-2 = '$$'
               PERFORM 0400-GET-VARIABLE.
           IF WORD-SIZE + LETTER-POINTER > LETTER-LINE-SIZE
               PERFORM 0840-WRITE-LETTER-LINE
               MOVE SPACES TO LETTER-LINE
               MOVE 1 TO LETTER-POINTER.
           STRING WORK-WORD DELIMITED BY '  '
               SPACE DELIMITED BY SIZE
               INTO LETTER-LINE
               POINTER LETTER-POINTER.
           IF FORM-POINTER > FORM-LINE-SIZE
               PERFORM 0820-READ-FORMS-LETTER
               MOVE 1 TO FORM-POINTER
               IF NOT FORMS-EOF AND FORM-LINE = SPACES
                 AND LETTER-LINE NOT = SPACES
                   PERFORM 0840-WRITE-LETTER-LINE
                   MOVE SPACES TO LETTER-LINE
                   MOVE 1 TO LETTER-POINTER
                   PERFORM 0820-READ-FORMS-LETTER.

       0400-GET-VARIABLE.
           IF WORK-WORD = '$$1'
               UNSTRING VARIABLE-1 DELIMITED BY '  '
                 INTO WORK-WORD COUNT WORD-SIZE
           ELSE
           IF WORK-WORD = '$$2'
               UNSTRING VARIABLE-2 DELIMITED BY '  '
                 INTO WORK-WORD COUNT WORD-SIZE
           ELSE
           IF WORK-WORD = '$$3'
               UNSTRING VARIABLE-3 DELIMITED BY '  '
                 INTO WORK-WORD COUNT WORD-SIZE
           ELSE
           IF WORK-WORD = '$$4'
               UNSTRING VARIABLE-4 DELIMITED BY '  '
                 INTO WORK-WORD COUNT WORD-SIZE
           ELSE
           IF WORK-WORD = '$$5'
               UNSTRING VARIABLE-5 DELIMITED BY '  '
                 INTO WORK-WORD COUNT WORD-SIZE.

       0800-READ-INSERTION-VALUES.
           READ INSERTION-VALUES INTO VARIABLE-INFO
               AT END
                   MOVE 'Y' TO VALUES-EOF-STATUS.
       0820-READ-FORMS-LETTER.
           READ FORM-LETTER
               AT END
                   MOVE 'Y' TO FORMS-EOF-STATUS.
       0840-WRITE-LETTER-LINE.
           WRITE LETTER-LINE AFTER 1.

