0010   IDENTIFICATION DIVISION.
       PROGRAM-ID. Commiss.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-SALES-FILE     ASSIGN TO "INSALES.DAT".
           SELECT OUTPUT-SALES-FILE    ASSIGN TO "OUTSALES.DAT".
           SELECT COMMISSIONS-OWED-FILE
                                       ASSIGN TO "COMMOWED.DAT".
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
0150   FD  OUTPUT-SALES-FILE.
0160   01  OUTPUT-SALES-RECORD.
0170       05  OUTSAL-SALESMAN-ID             PIC X(9).
0180       05  OUTSAL-YEAR-TO-DATE-COMMIS     PIC 9(6)V99.
0190       05  OUTSAL-NR-SALES                PIC 99.
0200       05  OUTSA-SALES-ITEMS OCCURS 0 TO 99 TIMES
0210           DEPENDING ON OUTSAL-NR-SALES.
0220           10  OUTSAL-INVOICE-NR         PIC X(9).
0230           10  OUTSAL-INV-AMT            PIC 9(7)V99.
0240           10  OUTSAL-AMT-PAID           PIC 9(7)V99.
0250           10  OUTSAL-AMT-CR-COMMIS      PIC 9(7)V99.
0260
0270   FD  COMMISSIONS-OWED-FILE.
0280   01  COMMIS-OWED-RECORD.
0290       05  COMMIS-SALESMAN-ID            PIC X(9).
0300       05  COMMIS-NR-ITEMS               PIC 99.
0310       05  COMMIS-AMOUNT-SUBJECT         PIC 9(7)V99.
0320       05  COMMIS-COMMISSION-AMOUNT      PIC 9(7)V99.
0330
0340   WORKING-STORAGE SECTION.
0350   01  INPUT-EOF-STATUS                  PIC XXX VALUE 'NO '.
0360       88  END-OF-INPUT                  VALUE 'YES'.
0370   01  NR-PAYING-INVOICES                PIC 99.
0380   01  TOTAL-SUBJECT-TO-COMMIS           PIC 9(7)V99.
0390   01  TOTAL-COMMISSION                  PIC 9(7)V99.
0400   01  INV-SUB                           PIC 99.
0410
0420   PROCEDURE DIVISION.
0430   0000-MAIN-CONTROL.
0440       OPEN INPUT INPUT-SALES-FILE
0450           OUTPUT OUTPUT-SALES-FILE
0460                  COMMISSIONS-OWED-FILE.
0470       PERFORM 0500-READ-SALES-FILE.
0480       PERFORM 0100-MAIN-LOOP
0490           UNTIL END-OF-INPUT.
0500       CLOSE INPUT-SALES-FILE
0510             OUTPUT-SALES-FILE
0520             COMMISSIONS-OWED-FILE.
0530       STOP RUN.
0540
0550   0100-MAIN-LOOP.
0560       MOVE ZERO TO NR-PAYING-INVOICES
0570                    TOTAL-SUBJECT-TO-COMMIS
0580                    TOTAL-COMMISSION.
0590       MOVE INPUT-SALES-RECORD TO OUTPUT-SALES-RECORD.
0600       PERFORM 0150-ACCUMULATE-COMMISSIONS
0610           VARYING INV-SUB FROM 1 BY 1
0620           UNTIL INV-SUB > OUTSAL-NR-SALES.
0630       MOVE OUTSAL-SALESMAN-ID TO COMMIS-SALESMAN-ID.
0640       MOVE NR-PAYING-INVOICES TO COMMIS-NR-ITEMS.
0650       MOVE TOTAL-SUBJECT-TO-COMMIS TO COMMIS-AMOUNT-SUBJECT.
0660       MOVE TOTAL-COMMISSION TO COMMIS-COMMISSION-AMOUNT.
0670       WRITE COMMIS-OWED-RECORD.
0680       ADD TOTAL-COMMISSION TO OUTSAL-YEAR-TO-DATE-COMMIS.
0690       WRITE OUTPUT-SALES-RECORD.
0700       PERFORM 0500-READ-SALES-FILE.
0710
0720   0150-ACCUMULATE-COMMISSIONS.
0730       IF OUTSAL-AMT-PAID (INV-SUB) > OUTSAL-AMT-CR-COMMIS (INV-SUB)
0740         ADD 1 TO NR-PAYING-INVOICES
0750         COMPUTE TOTAL-SUBJECT-TO-COMMIS = TOTAL-SUBJECT-TO-COMMIS +
0760          OUTSAL-AMT-PAID (INV-SUB) - OUTSAL-AMT-CR-COMMIS (INV-SUB)
0770         COMPUTE TOTAL-COMMISSION = TOTAL-COMMISSION + .07 *
0780          (OUTSAL-AMT-PAID (INV-SUB)
                 - OUTSAL-AMT-CR-COMMIS (INV-SUB))
0790         MOVE OUTSAL-AMT-PAID (INV-SUB)
0790             TO OUTSAL-AMT-CR-COMMIS (INV-SUB).
0800   0500-READ-SALES-FILE.
0810        READ INPUT-SALES-FILE
0820            AT END
0830                MOVE 'YES' TO INPUT-EOF-STATUS.
