       IDENTIFICATION DIVISION.
       PROGRAM-ID. Monofyr3.
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

       01  DISPLAY-AREA                PIC X(80) VALUE SPACES.
       01  POSITION-NUMBER             PIC 99    VALUE 1.
       01  CHAR-CTR                    PIC 99    VALUE ZERO.

       PROCEDURE DIVISION.

       0100-SHOW-THE-MONTH.
           MOVE FUNCTION CURRENT-DATE TO COMPUTER-DATE.
           MOVE THIS-YEAR TO DISPLAY-AREA (77:4).
           MOVE ',' TO DISPLAY-AREA (75:1).
           IF THIS-DAY (1:1) = '0'
               MOVE THIS-DAY (2:1) TO DISPLAY-AREA (74:1)
               MOVE 73 TO POSITION-NUMBER
           ELSE
               MOVE THIS-DAY TO DISPLAY-AREA (73:2)
               MOVE 72 TO POSITION-NUMBER.
           INSPECT MONTH-NAME (THIS-MONTH) TALLYING CHAR-CTR
               FOR CHARACTERS BEFORE INITIAL SPACE.
           SUBTRACT CHAR-CTR FROM POSITION-NUMBER.
           MOVE MONTH-NAME (THIS-MONTH) (1:CHAR-CTR)
               TO DISPLAY-AREA (POSITION-NUMBER:CHAR-CTR).
           DISPLAY DISPLAY-AREA.
           STOP RUN.

