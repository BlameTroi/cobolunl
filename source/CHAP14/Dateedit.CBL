       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DATEEDIT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WORK-YEAR                  PIC 9999.
       01  WORK-REMAINDER             PIC 9.
            88  LEAP-YEAR              VALUE ZERO.

       LINKAGE SECTION.
       01  EDIT-DATE.
           05   EDIT-YEAR             PIC 9999.
           05   EDIT-MONTH            PIC 99.
              88  31-DAY-MONTH         VALUES 1 3 5 7 8 10 12.
              88  30-DAY-MONTH         VALUES 4 6 9 11.
              88  FEBRUARY             VALUE 2.
           05   EDIT-DAY              PIC 99.
              88  DAYS1-TO-31          VALUES 1 THRU 31.
              88  DAYS1-TO-30          VALUES 1 THRU 30.
              88  DAYS1-TO-28          VALUES 1 THRU 28.
              88  DAY29                VALUE 29.
       01  DATE-VALID-SWITCH          PIC X.

       PROCEDURE DIVISION USING EDIT-DATE DATE-VALID-SWITCH.
           MOVE 'N' TO DATE-VALID-SWITCH.
           DIVIDE EDIT-YEAR BY 4 GIVING WORK-YEAR
              REMAINDER WORK-REMAINDER.
           IF (31-DAY-MONTH AND DAYS1-TO-31)
              OR (30-DAY-MONTH AND DAYS1-TO-30)
              OR (FEBRUARY AND DAYS1-TO-28)
              OR (FEBRUARY AND LEAP-YEAR AND DAY29)
                 MOVE 'Y' TO DATE-VALID-SWITCH.
       EXIT PROGRAM.
