       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    DB2ALLC.
      *****************************************************************
      *   DB2ALLC - COBOL PROGRAM TO ALLOCATE WORK AREA INTO WHICH    *
      *             DB2 WILL PUT DATA.  FOR VERSIONS OF COBOL THAT    *
      *             RUN ON PLATFORMS OTHER THAN MVS AND VM, YOU       *
      *             CAN ALLOCATE THIS SPACE IN THE SAME PROGRAM       *
      *             THAT YOU USE IT.                                  *
      *                                                               *
      *             THIS PROGRAM CALLS DB2DYNM, WHICH PERFORMS        *
      *             DYNAMIC SQL.                                      *
      *                                                               *
      *   (C) COPYRIGHT IBM CORPORATION 1998.                         *
      *****************************************************************
       ENVIRONMENT DIVISION.
      *--------------------
       CONFIGURATION SECTION.
       DATA DIVISION.
      *-------------
       WORKING-STORAGE SECTION.
      **********************************************************
      *  AREA IN WHICH DB2 PLACES DATA FROM SELECTS.  FOR      *
      *  SIMPLICITY, WE DEFINE A DATA AREA THAT MATCHES THE    *
      *  HOST EQUIVALENTS OF THE COLUMNS IN THE MY_BOOKS       *
      *  TABLE.                                                *
      **********************************************************
       01  WORKAREA.
          10 WORK-VARCHAR30.
             49 VARCHAR30-LEN  PIC S9(4) USAGE COMP.
             49 VARCHAR30-TEXT PIC X(30) VALUE SPACES.
          10 WORK-VARCHAR15.
             49 VARCHAR15-LEN  PIC S9(4) USAGE COMP.
             49 VARCHAR15-TEXT PIC X(30) VALUE SPACES.
          10 WORK-INT    PIC S9(9) USAGE COMP.
          10 WORK-DEC    PIC S9(3)V9(2) COMP-3.
          10 WORK-DATE   PIC X(10).
          10 WORK-VARCHAR40.
             49 VARCHAR40-LEN  PIC S9(4) USAGE COMP.
             49 VARCHAR40-TEXT PIC X(40) VALUE SPACES.
          10 WORK-SMINT  PIC S9(4) USAGE COMP.
      **********************************************************
      *  AREA IN WHICH DB2 PLACES INDICATOR DATA.  WE DEFINE   *
      *  ENOUGH INDICATORS FOR THE MAXIMUM NUMBER OF COLUMNS   *
      *  RETURNED FROM THE MY_BOOKS TABLE (7).                 *
      **********************************************************
       01  INDICATORS.
           02  IND-VARS        PIC S9(4) COMP OCCURS 7 TIMES.

       PROCEDURE DIVISION.
      *------------------
                CALL 'DB2DYNM' USING WORKAREA INDICATORS.
                GOBACK.