       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    DB2CLIEN.

      *****************************************************************
      *   DB2CLIEN - DB2 COBOL CLIENT PROGRAM THAT CALLS STORED       *
      *              PROCEDURE DB2SPROC.  THIS PROGRAM CONNECTS       *
      *              TO THE REMOTE SITE WHERE THERE STORED PROCEDURE  *
      *              IS LOCATED, THEN EXECUTES THE CALL STATEMENT     *
      *              WITH THREE PARAMETERS AND THREE INDICATOR        *
      *              VARIABLES.                                       *
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
       01  REPCON.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(23)
                   VALUE ' CONNECT TO REMOTE SITE'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(37)
                   VALUE SPACES.
       01  REPREL.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(39)
                   VALUE ' PREPARE TO DISCONNECT FROM REMOTE SITE'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(21)
                   VALUE SPACES.
       01  REPCAL.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(22)
                   VALUE ' CALL STORED PROCEDURE'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(38)
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
       01  REPNOU.
               02  FILLER PIC X(10)
                   VALUE SPACES.
               02  FILLER PIC X(25)
                   VALUE ' NO DESCRIPTIONS UPDATED '.
               02  FILLER PIC X(19)
                   VALUE 'BY STORED PROCEDURE'.
               02  FILLER PIC X(10)
                   VALUE SPACES.
               02  FILLER PIC X(16)
                   VALUE SPACES.
       01  REPSUP.
               02  FILLER PIC X(10)
                   VALUE SPACES.
               02  FILLER PIC X(32)
                   VALUE ' NUMBER OF DESCRIPTIONS UPDATED '.
               02  FILLER PIC X(20)
                   VALUE 'BY STORED PROCEDURE '.
               02  DSPUPD PIC ZZZ9
                   USAGE DISPLAY.
               02  FILLER PIC X(15)
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
      * PARAMETERS TO BE PASSED TO STORED PROCEDURE       *
      *****************************************************
       01 HV-TITLE.
          49 HV-TITLE-LEN  PIC S9(4) USAGE COMP.
          49 HV-TITLE-TEXT PIC X(30) VALUE SPACES.
       01 HV-DESCRIPTION.
          49 HV-DESC-LEN  PIC S9(4) USAGE COMP.
          49 HV-DESC-TEXT PIC X(40) VALUE SPACES.
       01 UPDATED PIC S9(4) USAGE COMP.
       01 INDICATORS.
          10 IND-VAR1 PIC S9(4) COMP.
          10 IND-VAR2 PIC S9(4) COMP.
          10 IND-VAR3 PIC S9(4) COMP.
      *
       PROCEDURE DIVISION.
      *------------------
      *****************************************************
      * MAIN PROGRAM ROUTINE                              *
      *****************************************************
       PGM-START.
      * OPEN OUTPUT FILE
                OPEN OUTPUT DATAOUT.
      * CONNECT TO REMOTE SITE
                WRITE OUTREC FROM REPCON.
                EXEC SQL CONNECT TO SANTA_TERESA_LAB
                END-EXEC.
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      *****************************************************
      * SET UP FOR STORED PROCEDURE CALL.  SET INPUT      *
      * INDICATOR VARIABLES TO 0 TO INDICATE THAT INPUT   *
      * PARAMETERS HAVE NONNULL VALUES.                   *
      *****************************************************
                MOVE 0 TO IND-VAR1.
                MOVE 0 TO IND-VAR2.
                MOVE 'French Cooking for Amateurs' TO HV-TITLE-TEXT.
                MOVE 27 TO HV-TITLE-LEN.
                MOVE 'Great cookbook, even for professionals'
                  TO HV-DESC-TEXT.
                MOVE 38 TO HV-DESC-LEN.
      * CALL THE STORED PROCEDURE
                WRITE OUTREC FROM REPCAL
                  AFTER ADVANCING 1 LINE.
                EXEC SQL CALL DB2SPROC(:HV-TITLE:IND-VAR1,
                                       :HV-DESCRIPTION:IND-VAR2,
                                       :UPDATED:IND-VAR3)
                END-EXEC.
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * SHOW RESULTS OF UPDATE THROUGH STORED PROCEDURE
                IF IND-VAR3 IS NOT EQUAL TO 0 THEN
                  WRITE OUTREC FROM REPNOU
                ELSE
                  MOVE UPDATED TO DSPUPD
                  WRITE OUTREC FROM REPSUP
                    AFTER ADVANCING 1 LINE
                END-IF.
      * SCHEDULE RELEASE FROM REMOTE SITE
                WRITE OUTREC FROM REPREL.
                EXEC SQL RELEASE SANTA_TERESA_LAB
                END-EXEC.
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * MAKE CHANGES PERMANENT.  THIS ALSO MAKES THE RELEASE
      * FROM THE REMOTE SITE HAPPEN.
                PERFORM COMMIT-WORK.
       PGM-END.
      * CLOSE OUTPUT FILE
                CLOSE DATAOUT.
                GOBACK.
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