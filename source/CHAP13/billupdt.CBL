       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Billupdt.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-BILLING-MASTER   ASSIGN TO "INPMSTR.DAT"
                                         LINE SEQUENTIAL.
      *    SELECT OUTPUT-BILLING-MASTER  ASSIGN TO "OUTMSTR.DAT".
           SELECT DAILY-PAYMENTS         ASSIGN TO "DAYPMTS.DAT"
                                         LINE SEQUENTIAL.
           SELECT PAYMENT-OVERDUE-REPORT ASSIGN TO "OVERDUE.DAT".


       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-BILLING-MASTER.
       01  MSTR-IN-BILLING-RECORD.
           05  MSTR-IN-ACCOUNT-NR      PIC X(8).
           05  MSTR-IN-LAST-BILLED-DATE PIC 9(6).
           05  MSTR-IN-LAST-PAID-DATE  PIC 9(6).
           05  FILLER                  PIC X(60).

      *  Other Input Master Billing Definitions

       FD  DAILY-PAYMENTS.
       01  PYMT-RECEIVED-RECORD.
           05  PYMT-ACCOUNT-NR         PIC X(8).
           05  FILLER                  PIC X(72).

      *  Daily Payment Information Definitions

       FD  PAYMENT-OVERDUE-REPORT.
       01  OVRDUE-PAYMENT-RECORD.
           05  OVRDUE-ACCOUNT-NR       PIC X(8).
           05                          PIC X.
           05  OVRDUE-BILLED-DATE      PIC 9(8).

      *  Other Information for Overdue Report

       WORKING-STORAGE SECTION.
       01  CURRENT-DATE-INFO           PIC X(21).
       01  CURRENT-DATE-INFO-GROUP REDEFINES CURRENT-DATE-INFO.
           05  CURRENT-DATE-INTEGER    PIC 9(8).
           05  FILLER         PIC X(13).
       01  MASTER-EOF-STATUS           PIC X VALUE 'N'.
           88  MASTER-END              VALUE 'Y'.
       01  PAYMENT-EOF-STATUS          PIC X VALUE 'N'.
           88  PAYMENT-END             VALUE 'Y'.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           OPEN INPUT INPUT-BILLING-MASTER
                      DAILY-PAYMENTS
                OUTPUT
      *                OUTPUT-BILLING-MASTER
                       PAYMENT-OVERDUE-REPORT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-INFO.
           PERFORM 0800-READ-INPUT-MASTER.
           PERFORM 0820-READ-PAYMENTS.
           PERFORM 0200-UPDATE-CONTROL-LOOP
               UNTIL MASTER-END AND PAYMENT-END.

           CLOSE INPUT-BILLING-MASTER
                 DAILY-PAYMENTS
      *          OUTPUT-BILLING-MASTER
                 PAYMENT-OVERDUE-REPORT.
           STOP RUN.

       0200-UPDATE-CONTROL-LOOP.
           IF MSTR-IN-ACCOUNT-NR > PYMT-ACCOUNT-NR
               PERFORM 0300-NO-MASTER-MATCH
               PERFORM 0820-READ-PAYMENTS
           ELSE
           IF MSTR-IN-ACCOUNT-NR < PYMT-ACCOUNT-NR
               PERFORM 0400-OUTPUT-MASTER
               PERFORM 0800-READ-INPUT-MASTER
           ELSE
               PERFORM 0500-POST-PAYMENT
               PERFORM 0820-READ-PAYMENTS.

       0300-NO-MASTER-MATCH.
           EXIT.

       0400-OUTPUT-MASTER.
           IF MSTR-IN-LAST-BILLED-DATE < 
                   FUNCTION INTEGER-OF-DATE (CURRENT-DATE-INTEGER) + 25
             AND MSTR-IN-LAST-BILLED-DATE < MSTR-IN-LAST-PAID-DATE
               MOVE SPACES TO OVRDUE-PAYMENT-RECORD
               MOVE MSTR-IN-ACCOUNT-NR TO OVRDUE-ACCOUNT-NR
               COMPUTE OVRDUE-BILLED-DATE = 
                   FUNCTION DATE-OF-INTEGER (MSTR-IN-LAST-BILLED-DATE)

      *  Additional Overdue Report Formatting

               PERFORM 0840-WRITE-OVERDUE-RECORD.
      *    PERFORM 0860-WRITE-OUTPUT-MASTER.

       0500-POST-PAYMENT.
           IF MSTR-IN-LAST-BILLED-DATE < 
                 FUNCTION INTEGER-OF-DATE (CURRENT-DATE-INTEGER) + 25
               PERFORM 0520-DETERMINE-LATE-CHARGES.
               COMPUTE MSTR-IN-LAST-PAID-DATE = 
                 FUNCTION INTEGER-OF-DATE (CURRENT-DATE-INTEGER).

      *  Additional Payment Processing.

       0520-DETERMINE-LATE-CHARGES.
           EXIT.

       0800-READ-INPUT-MASTER.

           READ INPUT-BILLING-MASTER
               AT END
                   MOVE HIGH-VALUES TO MSTR-IN-ACCOUNT-NR
                   MOVE 'Y' TO MASTER-EOF-STATUS.

       0820-READ-PAYMENTS.

           READ DAILY-PAYMENTS
               AT END
                   MOVE HIGH-VALUES TO PYMT-ACCOUNT-NR
                   MOVE 'Y' TO PAYMENT-EOF-STATUS.

       0840-WRITE-OVERDUE-RECORD.

           WRITE OVRDUE-PAYMENT-RECORD
               AFTER ADVANCING 1 LINE.

      *  Remainder of Program

