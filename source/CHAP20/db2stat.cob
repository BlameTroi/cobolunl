       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    DB2STAT.

      *****************************************************************
      *   DB2STAT - SAMPLE DB2 COBOL STATIC SQL PROGRAM.  THIS        *
      *             PROGRAM STATICALLY EXECUTES SQL CREATE TABLE,     *
      *             INSERT, UPDATE, OPEN, FETCH, CLOSE, AND DROP      *
      *             STATEMENTS.                                       *
      *                                                               *
      *   (C) COPYRIGHT IBM CORPORATION 1998.                         *
      *****************************************************************

       ENVIRONMENT DIVISION.
      *--------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      C01 IS TO-TOP-OF-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATAIN
                  ASSIGN TO DA-S-SYSIN.
           SELECT DATAOUT
                  ASSIGN TO UT-S-SYSPRINT.

       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD      DATAIN
               RECORD CONTAINS 80 CHARACTERS
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE OMITTED.
       01  DATAREC                    PIC X(80).

       FD  DATAOUT
               RECORD CONTAINS 80 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS OUTREC.
       01  OUTREC                     PIC X(80).

       WORKING-STORAGE SECTION.

      *****************************************************
      * RECORDS FOR REPORTING RESULTS                     *
      *****************************************************
       01  REPCRE.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(22)
                   VALUE ' CREATE TABLE MY_BOOKS'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(38)
                   VALUE SPACES.
       01  REPINS.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(21)
                   VALUE ' INSERT INTO MY_BOOKS'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(39)
                   VALUE SPACES.
       01  REPUPD.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(22)
                   VALUE ' UPDATE TABLE MY_BOOKS'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(38)
                   VALUE SPACES.
       01  REPOPN.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(37)
                   VALUE ' OPEN CURSOR FOR SELECT FROM MY_BOOKS'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(23)
                   VALUE SPACES.
       01  REPSEL.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(33)
                   VALUE ' SELECT WITH CURSOR FROM MY_BOOKS'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(27)
                   VALUE SPACES.
       01  REPCOL.
               02  FILLER PIC X(31)
                   VALUE '             TITLE             '.
               02  FILLER PIC X(5)
                   VALUE SPACES.
               02  FILLER PIC X(15)
                   VALUE '   AUTHOR     '.
               02  FILLER PIC X(39)
                   VALUE SPACES.
       01  REPROW.
               02  FILLER PIC X(1)
                   VALUE SPACES.
               02  OUT-TITLE PIC X(30)
                   VALUE SPACES.
               02  FILLER PIC X(5)
                   VALUE SPACES.
               02  OUT-AUTHOR PIC X(15)
                   VALUE SPACES.
               02  FILLER PIC X(29)
                   VALUE SPACES.
       01  REPCLS.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(13)
                   VALUE ' CLOSE CURSOR'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(47)
                   VALUE SPACES.
       01  REPDRP.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(20)
                   VALUE ' DROP TABLE MY_BOOKS'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(40)
                   VALUE SPACES.
       01  REPCMT.
               02  FILLER PIC X(10)
                   VALUE SPACES.
               02  FILLER PIC X(12)
                   VALUE ' COMMIT WORK'.
               02  FILLER PIC X(10)
                   VALUE SPACES.
               02  FILLER PIC X(48)
                   VALUE SPACES.
       01  REPCOD.
               02  FILLER PIC X(10)
                   VALUE SPACES.
               02  FILLER PIC X(12)
                   VALUE ' SQLCODE IS '.
               02  SQLCD  PIC -999
                   USAGE DISPLAY.
               02  FILLER PIC X(10)
                   VALUE SPACES.
               02  FILLER PIC X(44)
                   VALUE SPACES.

      *****************************************************
      * INCLUDE SQLCA FOR ERROR HANDLING                  *
      *****************************************************
                EXEC SQL INCLUDE SQLCA  END-EXEC.

      *****************************************************
      * SQL DECLARATION FOR TABLE MY_BOOKS                *
      *****************************************************
                EXEC SQL DECLARE MY_BOOKS TABLE
                  (TITLE       VARCHAR(30) NOT NULL,
                   AUTHOR      VARCHAR(15),
                   NUM_PAGES   INTEGER,
                   PRICE       DECIMAL(5,2),
                   DATE_BOUGHT DATE,
                   DESCRIPTION VARCHAR(40),
                   RATING      SMALLINT)
                END-EXEC.

      *****************************************************
      * HOST STRUCTURE AND INDICATOR ARRAY FOR A MY_BOOKS *
      * RECORD                                            *
      *****************************************************
      *         EXEC SQL BEGIN DECLARE SECTION.
       01 HV-BOOK.
          10 HV-TITLE.
             49 HV-TITLE-LEN  PIC S9(4) USAGE COMP.
             49 HV-TITLE-TEXT PIC X(30) VALUE SPACES.
          10 HV-AUTHOR.
             49 HV-AUTHOR-LEN  PIC S9(4) USAGE COMP.
             49 HV-AUTHOR-TEXT PIC X(15) VALUE SPACES.
          10 HV-NUM-PAGES PIC S9(9) USAGE COMP.
          10 HV-PRICE  PIC S9(3)V9(2) COMP-3.
          10 HV-DATE-BOUGHT PIC X(10).
          10 HV-DESCRIPTION.
             49 HV-DESC-LEN  PIC S9(4) USAGE COMP.
             49 HV-DESC-TEXT PIC X(40) VALUE SPACES.
          10 HV-RATING   PIC S9(4) USAGE COMP.
       01 IND-BOOK.
          10 IND-BOOK-VARS PIC S9(4) COMP OCCURS 7 TIMES.
      *         EXEC SQL END DECLARE SECTION.

      *****************************************************
      * CURSOR FOR SINGLE ROW SELECTS                     *
      *****************************************************

                EXEC SQL DECLARE CRSR CURSOR FOR
                  SELECT *
                  FROM  MY_BOOKS
                END-EXEC.

       PROCEDURE DIVISION.
      *------------------

      *****************************************************
      * SQL RETURN CODE HANDLING                          *
      *****************************************************

      *****************************************************
      * MAIN PROGRAM ROUTINE                              *
      *****************************************************
       PGM-START.
      * OPEN OUTPUT FILE
                OPEN OUTPUT DATAOUT.
      * CREATE THE MY_BOOKS TABLE
                WRITE OUTREC FROM REPCRE
                  AFTER ADVANCING TO-TOP-OF-PAGE.
                EXEC SQL CREATE TABLE MY_BOOKS
                  (TITLE   VARCHAR(30) NOT NULL,
                   AUTHOR   VARCHAR(15),
                   NUM_PAGES  INTEGER,
                   PRICE   DECIMAL(5,2),
                   DATE_BOUGHT DATE,
                   DESCRIPTION VARCHAR(40),
                   RATING SMALLINT)
                END-EXEC.
      * SHOW RESULTS OF CREATE
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * MAKE CHANGES PERMANENT
                PERFORM COMMIT-WORK.
      * INSERT A FEW ROWS INTO THE TABLE
                PERFORM INSERT-ROWS.
      * UPDATE A ROW
                WRITE OUTREC FROM REPUPD
                  AFTER ADVANCING 1 LINE.
                EXEC SQL UPDATE MY_BOOKS
                  SET DATE_BOUGHT='5/10/1994'
                  WHERE TITLE='French Cooking for Amateurs'
                END-EXEC.
      * SHOW RESULTS OF UPDATE
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * MAKE CHANGES PERMANENT
                PERFORM COMMIT-WORK.
      * OPEN A CURSOR TO SELECT ROWS FROM MY_TABLE
                WRITE OUTREC FROM REPOPN
                  AFTER ADVANCING 1 LINE.
                EXEC SQL OPEN CRSR
                END-EXEC.
      * SHOW RESULTS OF OPEN
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * SELECT ROWS FROM MY_TABLE AND DISPLAY TITLE AND AUTHOR
                WRITE OUTREC FROM REPSEL
                  AFTER ADVANCING 1 LINE.
                WRITE OUTREC FROM REPCOL
                  AFTER ADVANCING 1 LINE.
                PERFORM SELECT-AND-PRINT
                  UNTIL SQLCODE IS NOT EQUAL TO ZERO.
      * CLOSE THE CURSOR
                WRITE OUTREC FROM REPCLS
                  AFTER ADVANCING 1 LINE.
                EXEC SQL CLOSE CRSR
                END-EXEC.
      * SHOW RESULTS OF CLOSE
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * DROP THE TABLE
                WRITE OUTREC FROM REPDRP
                  AFTER ADVANCING 1 LINE.
                EXEC SQL DROP TABLE MY_BOOKS
                END-EXEC.
      * SHOW RESULTS OF DROP
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * MAKE CHANGES PERMANENT
                PERFORM COMMIT-WORK.
       PGM-END.
      * CLOSE OUTPUT FILE
                CLOSE DATAOUT.
                GOBACK.

      *****************************************************
      * ROUTINE TO INSERT ROWS INTO MY_BOOKS              *
      *****************************************************
       INSERT-ROWS.
                WRITE OUTREC FROM REPINS
                  AFTER ADVANCING 1 LINE.
                EXEC SQL INSERT INTO MY_BOOKS
                  VALUES ('Bears in the Wild', 'Ursine',
                          125, 15.95, '12/19/1991',
                          'Case studies of wild bear behavior', 4)
                END-EXEC.
      * SHOW RESULTS OF INSERT
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * MAKE CHANGES PERMANENT
                PERFORM COMMIT-WORK.
      * INSERT ANOTHER RECORD
                WRITE OUTREC FROM REPINS
                  AFTER ADVANCING 1 LINE.
                EXEC SQL INSERT INTO MY_BOOKS
                  VALUES ('French Cooking for Amateurs', 'LaForet',
                          500, NULL, NULL,
                          'Good recipes-NOT for amateurs', 2)
                END-EXEC.
      * SHOW RESULTS OF INSERT
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * MAKE CHANGES PERMANENT
                PERFORM COMMIT-WORK.
      *****************************************************
      * ROUTINE TO SELECT AND PRINT ROWS                  *
      *****************************************************
       SELECT-AND-PRINT.
                EXEC SQL FETCH CRSR INTO :HV-BOOK:IND-BOOK-VARS
                END-EXEC.
      * SHOW RESULTS OF FETCH
                IF SQLCODE IS NOT EQUAL TO 0 THEN
                  MOVE SQLCODE TO SQLCD
                  WRITE OUTREC FROM REPCOD
                    AFTER ADVANCING 1 LINE
                ELSE
                  MOVE SPACES TO OUT-TITLE
                  MOVE HV-TITLE-TEXT TO OUT-TITLE
                  IF IND-BOOK-VARS(2) IS NOT EQUAL TO -1 THEN
                    MOVE SPACES TO OUT-AUTHOR
                    MOVE HV-AUTHOR-TEXT TO OUT-AUTHOR
                  ELSE MOVE '---------------' TO OUT-AUTHOR
                  END-IF
                  WRITE OUTREC FROM REPROW
                    AFTER ADVANCING 1 LINE
                END-IF.
      *****************************************************
      * ROUTINE TO COMMIT SQL CHANGES                     *
      *****************************************************
       COMMIT-WORK.
                WRITE OUTREC FROM REPCMT
                  AFTER ADVANCING 1 LINE.
                EXEC SQL COMMIT
                END-EXEC.
      * SHOW RESULTS OF COMMIT
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.