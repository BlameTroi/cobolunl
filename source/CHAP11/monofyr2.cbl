       IDENTIFICATION DIVISION.
       PROGRAM-ID. Monofyr2.
       WORKING-STORAGE SECTION.

       01  COMPUTER-DATE.
           05  THIS-YEAR               PIC XXXX.
           05  THIS-MONTH              PIC 99.
           05  THIS-DAY                PIC XX.
           05  FILLER                  PIC X(13).

       01  MONTH-LIST.
           05  FILLER PIC X(30) VALUE 'January   February  March'.
           05  FILLER PIC X(30) VALUE 'April     May       June'.
           05  FILLER PIC X(30) VALUE 'July      August    September'.
           05  FILLER PIC X(30) VALUE 'October   November  December'.
       01  MONTH-TABLE REDEFINES MONTH-LIST.
           05  MONTH-NAME OCCURS 12 TIMES PIC X(10).

       01  DISPLAY-AREA                PIC X(40) VALUE SPACES.
       01  POSITION-NUMBER             PIC 99    VALUE 1.

       PROCEDURE DIVISION.

       0100-SHOW-THE-MONTH.
           MOVE FUNCTION CURRENT-DATE TO COMPUTER-DATE.
           STRING MONTH-NAME (THIS-MONTH) DELIMITED BY SPACE
               ' ' DELIMITED BY SIZE
               INTO DISPLAY-AREA
               POINTER POSITION-NUMBER.
           IF THIS-DAY (1:1) = '0'
               STRING THIS-DAY (2:1) DELIMITED BY SIZE
                 INTO DISPLAY-AREA
                 POINTER POSITION-NUMBER
           ELSE
               STRING THIS-DAY DELIMITED BY SIZE
                 INTO DISPLAY-AREA
                 POINTER POSITION-NUMBER
           END-IF.
           STRING ', ' DELIMITED BY SIZE
               THIS-YEAR DELIMITED BY SIZE
               INTO DISPLAY-AREA
               POINTER POSITION-NUMBER.
           DISPLAY DISPLAY-AREA.
           STOP RUN.

