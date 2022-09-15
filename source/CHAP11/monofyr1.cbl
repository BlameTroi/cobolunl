       IDENTIFICATION DIVISION.
       PROGRAM-ID. Monofyr1.
       WORKING-STORAGE SECTION.

       01  COMPUTER-DATE.
           05  THIS-YEAR               PIC XX.
           05  THIS-MONTH              PIC 99.
           05  THIS-DAY                PIC XX.

       01  MONTH-LIST.
           05  FILLER PIC X(30) VALUE 'January   February  March'.
           05  FILLER PIC X(30) VALUE 'April     May       June'.
           05  FILLER PIC X(30) VALUE 'July      August    September'.
           05  FILLER PIC X(30) VALUE 'October   November  December'.
       01  MONTH-TABLE REDEFINES MONTH-LIST.
           05  MONTH-NAME OCCURS 12 TIMES PIC X(10).

       PROCEDURE DIVISION.

       0100-SHOW-THE-MONTH.
            ACCEPT COMPUTER-DATE FROM DATE.
            DISPLAY MONTH-NAME (THIS-MONTH) THIS-DAY ', ' THIS-YEAR.
            STOP RUN.

