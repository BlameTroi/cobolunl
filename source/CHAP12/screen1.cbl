       IDENTIFICATION DIVISION.
       PROGRAM-ID. Screen1.
       WORKING-STORAGE SECTION.
       01  ROW-NR                               PIC 99 VALUE 1.
       01  COL-NR                               PIC 99 VALUE 1.
       01  CTR                                  PIC 9  VALUE ZERO.
       01  DISPLAY-SCREEN                              VALUE SPACES.
           05  SCREEN-LINE OCCURS 24 TIMES.
               10  SCREEN-CHAR OCCURS 80 TIMES  PIC X.
       PROCEDURE DIVISION.
       0000-ONLY-PARAGRAPH.
           PERFORM VARYING ROW-NR FROM 3 BY 2 UNTIL ROW-NR > 9
               PERFORM VARYING COL-NR FROM 12 BY 2 UNTIL COL-NR > 18
                   ADD 1 TO  CTR
                   MOVE  CTR TO SCREEN-CHAR (ROW-NR COL-NR).

           PERFORM VARYING ROW-NR FROM 1 BY 1 UNTIL ROW-NR > 24
               DISPLAY SCREEN-LINE (ROW-NR).
           STOP RUN.
