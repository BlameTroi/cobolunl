0010   IDENTIFICATION DIVISION.
       PROGRAM-ID. Commissb.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-SALES-FILE     ASSIGN TO "INSALES.DAT".
           SELECT INPUT-SALES-SOURCE   ASSIGN TO "INSALESB.DAT"
                                       LINE SEQUENTIAL.
       DATA DIVISION.
0020   FILE SECTION.
0030   FD  INPUT-SALES-FILE.
0040   01  INPUT-SALES-RECORD.
0050       05  INPSAL-SALESMAN-ID             PIC X(9).
0060       05  INPSAL-YEAR-TO-DATE-COMMIS     PIC 9(6)V99.
0070       05  INPSAL-NR-SALES                PIC 99.
0080       05  INPSA-SALES-ITEMS OCCURS 0 TO 99 TIMES
0090           DEPENDING ON INPSAL-NR-SALES.
0100           10  INPSAL-INVOICE-NR         PIC X(9).
0110           10  INPSAL-INV-AMT            PIC 9(7)V99.
0120           10  INPSAL-AMT-PAID           PIC 9(7)V99.
0130           10  INPSAL-AMT-CR-FOR-COMMIS  PIC 9(7)V99.
0140
       FD  INPUT-SALES-SOURCE.
       01  INPUT-SALES-SRECORD.
           05  INPSOU-TYPE                    PIC X.
           05  INPSOU-SALES-ITEM              PIC X(36).
           05  INPSOU-SALESMAN REDEFINES INPSOU-SALES-ITEM.
               10  INPSOU-SMAN-ID             PIC X(9).
               10  INPSOU-SMAN-YTD            PIC 9(6)V99.
               10  FILLER                     PIC X(19).
           05  FILLER                         PIC X(43).

0340   WORKING-STORAGE SECTION.
0350   01  INPUT-EOF-STATUS                  PIC XXX VALUE 'NO '.
0360       88  END-OF-INPUT                  VALUE 'YES'.
0400   01  INV-SUB                           PIC 99.
0410
0420   PROCEDURE DIVISION.
0430   0000-MAIN-CONTROL.
0440       OPEN INPUT INPUT-SALES-SOURCE
0450           OUTPUT INPUT-SALES-FILE.
0470       PERFORM 0500-READ-SALES-FILE.
           MOVE SPACES TO INPSAL-SALESMAN-ID.
           MOVE ZERO   TO INPSAL-YEAR-TO-DATE-COMMIS
                          INPSAL-NR-SALES.
0480       PERFORM 0100-MAIN-LOOP
0490           UNTIL END-OF-INPUT.
           IF INPSAL-NR-SALES > ZERO
               WRITE INPUT-SALES-RECORD.
0500       CLOSE INPUT-SALES-FILE
0510             INPUT-SALES-SOURCE.
0530       STOP RUN.
0540
0550   0100-MAIN-LOOP.
           IF INPSOU-TYPE = '1' AND INPSAL-NR-SALES > ZERO
               WRITE INPUT-SALES-RECORD.
           IF INPSOU-TYPE = '1'
               MOVE INPSOU-SMAN-ID TO INPSAL-SALESMAN-ID
               MOVE INPSOU-SMAN-YTD TO INPSAL-YEAR-TO-DATE-COMMIS
               MOVE ZERO TO INPSAL-NR-SALES.
           IF INPSOU-TYPE = '2'
               ADD 1 TO INPSAL-NR-SALES
               MOVE INPSOU-SALES-ITEM
                    TO INPSA-SALES-ITEMS (INPSAL-NR-SALES).
0700       PERFORM 0500-READ-SALES-FILE.
0710
0800   0500-READ-SALES-FILE.
0810        READ INPUT-SALES-SOURCE
0820            AT END
0830                MOVE 'YES' TO INPUT-EOF-STATUS.

