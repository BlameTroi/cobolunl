       CBL RENT
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    DB2SPROC.
      *****************************************************************
      *   DB2SPROC - DB2 COBOL STORED PROCEDURE THAT RECEIVES TWO     *
      *              PARAMETERS AND RETURNS ONE PARAMETER.  THIS      *
      *              STORED PROCEDURE HAS THE GENERAL LINKAGE         *
      *              CONVENTION.                                      *
      *                                                               *
      *   (C) COPYRIGHT IBM CORPORATION 1998.                         *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

      *****************************************************
      * INCLUDE SQLCA FOR ERROR HANDLING                  *
      *****************************************************
           EXEC SQL INCLUDE SQLCA END-EXEC.
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
      ****************************************************
      *   DECLARE A HOST VARIABLE TO COUNT THE NUMBER    *
      *   OF ENTRIES IN MY_BOOKS WITH A GIVEN TITLE.     *
      ****************************************************
       01  MATCH   PIC S9(4) USAGE COMP.
      *****************************************************
      * DECLARE LOCAL COPIES OF PARAMETERS USED IN        *
      * SQL STATEMENTS.                                   *
      *****************************************************
       01 IN-HVS.
          10 HV-TITLE.
             49 HV-TITLE-LEN  PIC S9(4) USAGE COMP.
             49 HV-TITLE-TEXT PIC X(30) VALUE SPACES.
          10 HV-DESCRIPTION.
             49 HV-DESC-LEN  PIC S9(4) USAGE COMP-4.
             49 HV-DESC-TEXT PIC X(40).

       LINKAGE SECTION.
      *****************************************************
      * DECLARE PARAMETERS FOR THE PROCEDURE              *
      *****************************************************
       01 BK-TITLE.
          10 BK-TITLE-LEN  PIC S9(4) USAGE COMP.
          10 BK-TITLE-TEXT PIC X(30).
       01 BK-DESCRIPTION.
          10 DESC-LEN  PIC S9(4) USAGE COMP-4.
          10 DESC-TEXT PIC X(40).
       01 UPDATED PIC S9(4) USAGE COMP-4.
       01 INDICATORS.
          10 IND-VAR1  PIC S9(4) USAGE COMP.
          10 IND-VAR2  PIC S9(4) USAGE COMP.
          10 IND-VAR3  PIC S9(4) USAGE COMP.

       PROCEDURE DIVISION USING BK-TITLE,
                 BK-DESCRIPTION, UPDATED, INDICATORS.
      *****************************************************
      * CHECK FOR NULL INPUT VALUES.  DON'T TRY TO UPDATE *
      * A ROW IF EITHER INPUT VALUE IS NULL.              *
      *****************************************************
           IF IND-VAR1 IS NOT EQUAL TO -1 THEN
             MOVE BK-TITLE TO HV-TITLE
           ELSE MOVE -1 TO IND-VAR3.
           IF IND-VAR2 IS NOT EQUAL TO -1 THEN
             MOVE BK-DESCRIPTION TO HV-DESCRIPTION
           ELSE MOVE -1 TO IND-VAR3.
           IF IND-VAR3 IS NOT EQUAL TO -1 THEN
      *****************************************************
      * EXECUTE A SELECT STATEMENT AGAINST MY_BOOKS TO    *
      * SEE IF A COPY OF THE ENTRY YOU WANT TO ADD        *
      * ALREADY EXISTS.                                   *
      *****************************************************
             MOVE 0 TO IND-VAR3
             EXEC SQL
                SELECT COUNT(*) INTO :MATCH
                   FROM MY_BOOKS
                   WHERE TITLE=:HV-TITLE
             END-EXEC
      * IF THE ROW DOESN"T EXIST, DON'T TRY TO UPDATE IT.
             IF MATCH IS EQUAL TO 0 THEN
               MOVE -1 TO IND-VAR3
             ELSE
      *****************************************************
      * EXECUTE AN UPDATE STATEMENT TO UPDATE THE         *
      * DESCRIPTION OF THE BOOK WITH THE GIVEN TITLE.     *
      * IF THERE ARE MULTIPLE ENTRIES FOR THIS BOOK,      *
      * ALL ENTRIES ARE UPDATED.                          *
      *****************************************************
               EXEC SQL UPDATE MY_BOOKS
                 SET DESCRIPTION=:HV-DESCRIPTION
                 WHERE TITLE=:HV-TITLE
               END-EXEC
               IF SQLCODE IS EQUAL TO 0 THEN
                 MOVE MATCH TO UPDATED
               ELSE MOVE -1 TO IND-VAR3
               END-IF
             END-IF
            END-IF.
            GOBACK.