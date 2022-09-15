       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Sort1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILLED-ORDERS        ASSIGN TO "INDATA1.DAT"
                                       LINE SEQUENTIAL.
           SELECT OUTSTANDING-ORDERS   ASSIGN TO "INDATA2"
                                       ORGANIZATION INDEXED
                                       ACCESS SEQUENTIAL
                                       RECORD KEY ORDER-KEY.
           SELECT OUTPUT-DATA          ASSIGN TO "OUTDATA.DAT"
                                       LINE SEQUENTIAL.
           SELECT SORT-WORK            ASSIGN TO SORTWORK.

       DATA DIVISION.
       FILE SECTION.
       FD  FILLED-ORDERS.
       01  FILLED-ORDER-RECORD            PIC X(80).

       FD  OUTSTANDING-ORDERS.
       01  OUTSTANDING-ORDER-RECORD.
           05  ORDER-KEY.
               10  ORDER-DATE-YYYYMMDD    PIC 9(8).
               10  ORDER-NUMBER           PIC 9(6).
           05  FILLER                     PIC X(66).
       FD  OUTPUT-DATA
           LABEL RECORDS STANDARD.
       01  OUTPUT-RECORD                  PIC X(80).
       SD  SORT-WORK.
       01  SORT-RECORD.
           05  SORT-ORDER-DATE-YYYYMMDD   PIC 9(8).
           05  SORT-ORDER-NUMBER          PIC 9(6).
           05  SORT-CUSTOMER-CODE         PIC X(6).
           05  SORT-CUSTOMER-NAME         PIC X(25).
           05  FILLER                     PIC X(10).
           05  SORT-SALESMAN-NAME         PIC X(25).

       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           SORT SORT-WORK
               ASCENDING KEY  SORT-SALESMAN-NAME
               DESCENDING KEY SORT-ORDER-DATE-YYYYMMDD
               ASCENDING KEY  SORT-CUSTOMER-NAME
                              SORT-ORDER-NUMBER
               USING FILLED-ORDERS
                     OUTSTANDING-ORDERS
               GIVING OUTPUT-DATA.
           STOP RUN.