       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    DB2DYNM.
      *****************************************************************
      *   DB2DYNM - SAMPLE DB2 COBOL DYNAMIC SQL PROGRAM.  THIS       *
      *             PROGRAM IS CALLED BY DB2ALLC, WHICH ALLOCATES     *
      *             THE DATA AREAS INTO WHICH DB2 PLACES COLUMN       *
      *             VALUES.   THIS PROGRAM EXECUTES SQL PREPARE,      *
      *             DESCRIBE, EXECUTE, AND FETCH STATEMENTS.          *
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
       01  REPPRP.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(20)
                   VALUE ' PREPARE A STATEMENT'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(40)
                   VALUE SPACES.
       01  REPDSC.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(21)
                   VALUE ' DESCRIBE A STATEMENT'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(39)
                   VALUE SPACES.
       01  REPNSL.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(26)
                   VALUE ' STATEMENT IS NOT A SELECT'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(34)
                   VALUE SPACES.
       01  REPEXE.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(18)
                   VALUE ' EXECUTE STATEMENT'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(42)
                   VALUE SPACES.
       01  REPSEL.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(22)
                   VALUE ' STATEMENT IS A SELECT'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(38)
                   VALUE SPACES.
       01  REPCOL.
               02  FILLER PIC X(1)
                   VALUE SPACES.
               02  OUT-COLNAME PIC X(30)
                       VALUE SPACES.
               02  FILLER PIC X(5)
                   VALUE SPACES.
               02  OUT-COLTEXT PIC X(40)
                       VALUE SPACES.
               02  OUT-COLDEC REDEFINES OUT-COLTEXT.
                   04 OUT-DEC    PIC $ZZ9.9(2) USAGE DISPLAY.
                   04 FILLER     PIC X(33).
               02  OUT-COLINT REDEFINES OUT-COLTEXT.
                   04 OUT-INT    PIC -9(9) USAGE DISPLAY.
                   04 FILLER     PIC X(30).
               02  OUT-COLSMINT REDEFINES OUT-COLTEXT.
                   04 OUT-SMINT  PIC -9(5) USAGE DISPLAY.
                   04 FILLER     PIC X(34).
               02  FILLER PIC X(4)
                   VALUE SPACES.
       01  REPOPN.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(12)
                   VALUE ' OPEN CURSOR'.
               02  FILLER PIC X(10)
                   VALUE ' ---------'.
               02  FILLER PIC X(48)
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
      * SOME SAMPLE STATEMENTS TO EXECUTE.  IN A REAL     *
      * APPLICATION, YOU WOULD READ IN THE STATEMENTS.    *
      *****************************************************
       01  UPDSTMT-PART1.
           49  UPDSTMT-PART1-LEN   PIC S9(4) USAGE COMP
               VALUE 44.
           49  UPDSTMT-PART1-TEXT  PIC X(44) VALUE
               'UPDATE MY_BOOKS SET DATE_BOUGHT=''5/10/1994'' '.
       01  UPDSTMT-PART2.
           49  UPDSTMT-PART2-LEN   PIC S9(4) USAGE COMP
               VALUE 41.
           49  UPDSTMT-PART2-TEXT  PIC X(41) VALUE
               'WHERE TITLE=''French Cooking for Amateurs'''.
       01  SELSTMT.
           49  SELSTMT-LEN         PIC S9(4) USAGE COMP
               VALUE 33.
           49  SELSTMT-TEXT        PIC X(33) VALUE
               'SELECT TITLE, PRICE FROM MY_BOOKS'.
      *****************************************************
      * DECLARE THE SOURCE STATEMENT TO BE PREPARED.      *
      *****************************************************
       01  SRCSTMT.
           49  SRCSTMT-LEN         PIC S9(4) USAGE COMP.
           49  SRCSTMT-TEXT        PIC X(100) VALUE SPACES.
      *****************************************************
      * INCLUDE SQLCA FOR ERROR HANDLING                  *
      *****************************************************
                EXEC SQL INCLUDE SQLCA  END-EXEC.

      *****************************************************
      * DECLARE SQLDA FOR RECEIVING RESULTS OF            *
      * DESCRIBE STATEMENTS.  FOR THIS SAMPLE, WE         *
      * ASSUME THAT ALL SQL STATEMENTS ARE EXECUTED       *
      * ON TABLE MY_BOOKS, SO THE SQLDA NEEDS ROOM        *
      * FOR A MAXIMUM OF 7 COLUMNS, AND ALL DATA WILL     *
      * HAVE ONE OF THE DATA TYPES IN MY_BOOK.            *
      *****************************************************
       01  BKSQLDA.
               02  SQLDAID     PIC X(8)   VALUE 'SQLDA   '.
               02  SQLDABC     PIC S9(8) COMP  VALUE 324.
               02  SQLN        PIC S9(4) COMP  VALUE 7.
               02  SQLD        PIC S9(4) COMP  VALUE 0.
               02  SQLVAR      OCCURS 1 TO 7 TIMES
                                        DEPENDING ON SQLN.
                   03  SQLTYPE     PIC S9(4) COMP.
                   03  SQLLEN      PIC S9(4) COMP.
                   03  SQLDATA     POINTER.
                   03  SQLIND      POINTER.
                   03  SQLNAME.
                       49  SQLNAMEL    PIC S9(4) COMP.
                       49  SQLNAMEC    PIC X(30).
      **********************************************************
      *  EXPECTED VALUES OF SQLTYPE, GIVEN THAT ONLY THE TYPES *
      *  IN MY_BOOKS ARE RETURNED.  ADD 1 TO THESE TYPE VALUES *
      *  TO GET THE SQLTYPE FOR A COLUMN THAT TAKES NULLS.     *
      **********************************************************
       77  DATE-TYPE           PIC S9(4)  COMP VALUE +384.
       77  VARCHAR-TYPE        PIC S9(4)  COMP VALUE +448.
       77  DECIMAL-TYPE        PIC S9(4)  COMP VALUE +484.
       77  INTEGER-TYPE        PIC S9(4)  COMP VALUE +496.
       77  SMALLINT-TYPE       PIC S9(4)  COMP VALUE +500.

      *****************************************************
      * AREA TO HOLD CURRENT DATA TYPE                    *
      *****************************************************
       01  DATA-TYPE           PIC S9(4)  COMP.
      *****************************************************
      * COUNTER FOR MOVING THROUGH SQLVAR STRUCTURES      *
      *****************************************************
       01  I  PIC S9(4) USAGE BINARY.
      *****************************************************
      * DECLARE AN SQL VARIABLE TO HOLD PREPARED          *
      * STATEMENTS                                        *
      *****************************************************

                EXEC SQL DECLARE OBJSTMT STATEMENT
                END-EXEC.
      *****************************************************
      * DECLARE CURSOR FOR PREPARED SELECT STATEMENTS     *
      *****************************************************

                EXEC SQL DECLARE CRSR CURSOR FOR OBJSTMT
                END-EXEC.

       LINKAGE SECTION.
      *----------------
      **********************************************************
      *  AREA INTO WHICH DB2 PLACES INDICATOR VALUES DURING    *
      *  SELECTS.  THIS AREA IS PASSED FROM DB2ALLC.           *
      **********************************************************
       01  INDICATORS.
           02  IND-VARS        PIC S9(4) COMP OCCURS 7 TIMES.
      **********************************************************
      *  AREA INTO WHICH DB2 PLACES DATA VALUES DURING         *
      *  SELECTS.  THIS AREA IS PASSED FROM DB2ALLC.           *
      **********************************************************
       01  WORKAREA.
          10 WORK-VARCHAR30.
             49 VARCHAR30-LEN  PIC S9(4) USAGE COMP.
             49 VARCHAR30-TEXT PIC X(30).
          10 WORK-VARCHAR15.
             49 VARCHAR15-LEN  PIC S9(4) USAGE COMP.
             49 VARCHAR15-TEXT PIC X(30).
          10 WORK-INT    PIC S9(9) USAGE COMP.
          10 WORK-DEC    PIC S9(3)V9(2) COMP-3.
          10 WORK-DATE   PIC X(10).
          10 WORK-VARCHAR40.
             49 VARCHAR40-LEN  PIC S9(4) USAGE COMP.
             49 VARCHAR40-TEXT PIC X(40).
          10 WORK-SMINT  PIC S9(4) USAGE COMP.

       PROCEDURE DIVISION USING WORKAREA INDICATORS.
      *---------------------------------------------
      *****************************************************
      * SQL RETURN CODE HANDLING                          *
      *****************************************************
                EXEC SQL WHENEVER SQLERROR   CONTINUE END-EXEC.
                EXEC SQL WHENEVER SQLWARNING CONTINUE END-EXEC.
                EXEC SQL WHENEVER NOT FOUND  CONTINUE END-EXEC.

      *****************************************************
      * MAIN PROGRAM ROUTINE                              *
      *****************************************************
       PGM-START.
      * OPEN OUTPUT FILE
                OPEN OUTPUT DATAOUT.
      * GET THE FIRST SAMPLE STATEMENT TO EXECUTE.
                STRING UPDSTMT-PART1-TEXT UPDSTMT-PART2-TEXT
                  DELIMITED BY SIZE INTO SRCSTMT-TEXT.
                ADD UPDSTMT-PART1-LEN TO UPDSTMT-PART2-LEN
                  GIVING SRCSTMT-LEN.
                PERFORM PREPARE-DESCRIBE-EXECUTE.
      * GET THE SECOND SAMPLE SQL STATEMENT TO EXECUTE.
                MOVE SELSTMT TO SRCSTMT.
                PERFORM PREPARE-DESCRIBE-EXECUTE.
       PGM-END.
      * CLOSE OUTPUT FILE
                CLOSE DATAOUT.
                GOBACK.

      *****************************************************
      * ROUTINE TO PREPARE AND EXECUTE AN SQL STATEMENT   *
      *****************************************************
       PREPARE-DESCRIBE-EXECUTE.
      * PREPARE THE STATEMENT TO GET ITS EXECUTABLE FORM
                WRITE OUTREC FROM REPPRP
                  AFTER ADVANCING 1 LINE.
                EXEC SQL PREPARE OBJSTMT FROM :SRCSTMT
                END-EXEC.
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * DESCRIBE THE STATEMENT TO DETERMINE WHAT IT IS
                WRITE OUTREC FROM REPDSC
                  AFTER ADVANCING 1 LINE.
                EXEC SQL DESCRIBE OBJSTMT INTO :BKSQLDA
                END-EXEC.
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * IF SQLD IS 0, THIS IS NOT A SELECT, SO EXECUTE IT.
      * OTHERWISE, SET UP THE SQLDA FOR FETCHING ROWS.
                IF SQLD IS EQUAL TO 0 THEN
                  PERFORM EXECUTE-ONLY
                ELSE
                  PERFORM SET-UP-SQLDA-AND-FETCH.
      *****************************************************
      * ROUTINE TO EXECUTE A NON-SELECT STATEMENT         *
      *****************************************************
       EXECUTE-ONLY.
                WRITE OUTREC FROM REPNSL
                  AFTER ADVANCING 1 LINE.
                WRITE OUTREC FROM REPEXE
                  AFTER ADVANCING 1 LINE.
                EXEC SQL EXECUTE OBJSTMT
                END-EXEC.
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      *****************************************************
      * ROUTINE TO EXECUTE A SELECT STATEMENT             *
      *****************************************************
       SET-UP-SQLDA-AND-FETCH.
                WRITE OUTREC FROM REPSEL
                  AFTER ADVANCING 1 LINE.
                PERFORM SET-UP-SQLDA VARYING I FROM 1 BY 1
                  UNTIL I IS GREATER THAN SQLD.
      * OPEN THE CURSOR
                WRITE OUTREC FROM REPOPN
                  AFTER ADVANCING 1 LINE.
                EXEC SQL OPEN CRSR
                END-EXEC.
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      * SELECT ALL ROWS
                PERFORM SELECT-AND-PRINT
                  UNTIL SQLCODE IS NOT EQUAL TO ZERO.
      * CLOSE THE CURSOR
                WRITE OUTREC FROM REPCLS
                  AFTER ADVANCING 1 LINE.
                EXEC SQL CLOSE CRSR
                END-EXEC.
                MOVE SQLCODE TO SQLCD.
                WRITE OUTREC FROM REPCOD
                  AFTER ADVANCING 1 LINE.
      *****************************************************
      * ROUTINE TO SET UP THE SQLDA                       *
      *****************************************************
       SET-UP-SQLDA.
                MOVE SQLTYPE(I) TO DATA-TYPE.
                SET SQLIND(I) TO ADDRESS OF IND-VARS(I).
      * GET THE DATA TYPE IRRESPECTIVE OF NULLABILITY
                DIVIDE DATA-TYPE BY 2 GIVING DATA-TYPE.
                MULTIPLY DATA-TYPE BY 2 GIVING DATA-TYPE.
                EVALUATE DATA-TYPE
                 WHEN VARCHAR-TYPE
                   IF SQLLEN(I) IS EQUAL TO 30 THEN
                     SET SQLDATA(I) TO ADDRESS OF WORK-VARCHAR30
                   ELSE IF SQLLEN(I) IS EQUAL TO 15 THEN
                          SET SQLDATA(I) TO ADDRESS OF WORK-VARCHAR15
                        ELSE
                          SET SQLDATA(I) TO ADDRESS OF WORK-VARCHAR40
                        END-IF
                   END-IF,
                 WHEN INTEGER-TYPE
                   SET SQLDATA(I) TO ADDRESS OF WORK-INT,
                 WHEN SMALLINT-TYPE
                   SET SQLDATA(I) TO ADDRESS OF WORK-SMINT,
                 WHEN DECIMAL-TYPE
                   SET SQLDATA(I) TO ADDRESS OF WORK-DEC,
                 WHEN DATE-TYPE
                   SET SQLDATA(I) TO ADDRESS OF WORK-DATE,
                END-EVALUATE.
      *****************************************************
      * ROUTINE TO FETCH ROWS                             *
      *****************************************************
       SELECT-AND-PRINT.
                EXEC SQL FETCH CRSR USING DESCRIPTOR :BKSQLDA
                END-EXEC.
      * SHOW RESULTS OF FETCH
                IF SQLCODE IS NOT EQUAL TO 0 THEN
                  MOVE SQLCODE TO SQLCD
                  WRITE OUTREC FROM REPCOD
                    AFTER ADVANCING 1 LINE
                ELSE
                  PERFORM MOVE-RESULTS VARYING I FROM 1 BY 1
                    UNTIL I IS GREATER THAN SQLD
                END-IF.
       MOVE-RESULTS.
                MOVE SQLNAMEC(I) TO OUT-COLNAME.
                MOVE SPACES TO OUT-COLTEXT.
                IF IND-VARS(I) IS NOT EQUAL TO -1 THEN
                  MOVE SQLTYPE(I) TO DATA-TYPE
      * GET THE DATA TYPE IRRESPECTIVE OF NULLABILITY
                  DIVIDE DATA-TYPE BY 2 GIVING DATA-TYPE
                  MULTIPLY DATA-TYPE BY 2 GIVING DATA-TYPE
                  EVALUATE DATA-TYPE
                   WHEN VARCHAR-TYPE
                     IF SQLLEN(I) IS EQUAL TO 30 THEN
                       MOVE VARCHAR30-TEXT TO OUT-COLTEXT
                     ELSE IF SQLLEN(I) IS EQUAL TO 15 THEN
                            MOVE VARCHAR15-TEXT TO OUT-COLTEXT
                          ELSE
                            MOVE VARCHAR40-TEXT TO OUT-COLTEXT
                          END-IF
                     END-IF,
                   WHEN INTEGER-TYPE
                     MOVE WORK-INT TO OUT-INT,
                   WHEN SMALLINT-TYPE
                     MOVE WORK-SMINT TO OUT-SMINT,
                   WHEN DECIMAL-TYPE
                     MOVE WORK-DEC TO OUT-DEC,
                   WHEN DATE-TYPE
                     MOVE WORK-DATE TO OUT-COLTEXT,
                  END-EVALUATE
                ELSE
                  MOVE '---------------' TO OUT-COLTEXT
                END-IF.
                WRITE OUTREC FROM REPCOL
                  AFTER ADVANCING 1 LINE.