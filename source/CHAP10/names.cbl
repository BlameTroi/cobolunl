       IDENTIFICATION DIVISION.
       PROGRAM-ID. Names.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FULL-NAME-INPUT         PIC X(79).
       01  FULL-NAME-SIZE          PIC 99 VALUE 79.
       01  NAME-POINTER            PIC 99.
       01  FIRST-NAME              PIC X(50).
       01  MIDDLE-NAME-1.
           05  MIDDLE-INITIAL-1    PIC X.
           05  FILLER              PIC X(49).
       01  MIDDLE-NAME-2.
           05  MIDDLE-INITIAL-2    PIC X.
           05  FILLER              PIC X(49).
       01  MIDDLE-NAME-3.
           05  MIDDLE-INITIAL-3    PIC X.
           05  FILLER              PIC X(49).
       01  LAST-NAME               PIC X(50).
       01  SUFFIX                  PIC X(50).
           88  SUFFIX-VALUE VALUE 'JR' 'SR' 'II' 'III'
                                  'IV' 'V' 'ESQ' 'Jr'
                                  'Sr' 'Esq'.
       01  FULL-NAME-LAST-FIRST    PIC X(79).
       01  FIRST-NAME-SIZE         PIC 99.
       01  NUMBER-OF-NAMES         PIC 99 VALUE ZERO.

       PROCEDURE DIVISION.
       0001-MAIN-PARAGRAPH.
           MOVE SPACES TO FULL-NAME-INPUT.
           DISPLAY 'ENTER YOUR FULL NAME PLEASE'.
           ACCEPT FULL-NAME-INPUT.
           INSPECT FULL-NAME-INPUT REPLACING ALL '.' BY SPACE.
           MOVE 1 TO NAME-POINTER.
           INSPECT FULL-NAME-INPUT
                 TALLYING NAME-POINTER FOR LEADING SPACES.
           MOVE SPACES TO MIDDLE-INITIAL-1 MIDDLE-INITIAL-2
                          MIDDLE-INITIAL-3
                          LAST-NAME SUFFIX.
           UNSTRING FULL-NAME-INPUT DELIMITED BY ALL SPACES
               INTO FIRST-NAME COUNT IN FIRST-NAME-SIZE
                    MIDDLE-NAME-1
                    MIDDLE-NAME-2
                    MIDDLE-NAME-3
                    LAST-NAME
                    SUFFIX
               POINTER NAME-POINTER
               TALLYING NUMBER-OF-NAMES
               ON OVERFLOW
                 DISPLAY 'WE CAN ONLY HANDLE 3 MIDDLE NAMES'
                 PERFORM 0100-EXTRA-NAMES
                    UNTIL NAME-POINTER > FULL-NAME-SIZE
           END-UNSTRING.
           EVALUATE NUMBER-OF-NAMES
             WHEN 1
               CONTINUE
             WHEN 2
               MOVE MIDDLE-NAME-1 TO LAST-NAME
               MOVE SPACES TO MIDDLE-NAME-1
             WHEN 3
               MOVE MIDDLE-NAME-2 TO SUFFIX
               IF SUFFIX-VALUE
                 MOVE MIDDLE-NAME-1 TO LAST-NAME
                 MOVE SPACES TO MIDDLE-NAME-1 MIDDLE-NAME-2
               ELSE
                 MOVE MIDDLE-NAME-2 TO LAST-NAME
                 MOVE SPACES TO MIDDLE-NAME-2 SUFFIX
               END-IF
             WHEN 4
               MOVE MIDDLE-NAME-3 TO SUFFIX
               IF SUFFIX-VALUE
                 MOVE MIDDLE-NAME-2 TO LAST-NAME
                 MOVE SPACES TO MIDDLE-NAME-2 MIDDLE-NAME-3
               ELSE
                 MOVE MIDDLE-NAME-3 TO LAST-NAME
                 MOVE SPACES TO MIDDLE-NAME-3 SUFFIX
               END-IF
             WHEN 5
               MOVE LAST-NAME TO SUFFIX
               IF SUFFIX-VALUE
                 MOVE MIDDLE-NAME-3 TO LAST-NAME
                 MOVE SPACES TO MIDDLE-NAME-3
               ELSE
                 MOVE SPACES TO SUFFIX
               END-IF
             WHEN 6
               IF NOT SUFFIX-VALUE
                 MOVE SUFFIX TO LAST-NAME
                 MOVE SPACES TO SUFFIX
               END-IF
           END-EVALUATE.
           MOVE 1 TO NAME-POINTER.
           MOVE SPACES TO FULL-NAME-LAST-FIRST.
           STRING LAST-NAME DELIMITED BY SPACES
                 ', ' DELIMITED BY SIZE
                 FIRST-NAME DELIMITED BY SPACE
               INTO FULL-NAME-LAST-FIRST
               POINTER NAME-POINTER.
           IF FIRST-NAME-SIZE = 1
               STRING '.' DELIMITED BY SIZE
                   INTO FULL-NAME-LAST-FIRST
                   POINTER NAME-POINTER.
           STRING SPACE DELIMITED BY SIZE
               INTO FULL-NAME-LAST-FIRST
               POINTER NAME-POINTER.
           IF MIDDLE-INITIAL-1 NOT = SPACES
               STRING MIDDLE-INITIAL-1 DELIMITED BY SIZE
                   '. ' DELIMITED BY SIZE
                   INTO FULL-NAME-LAST-FIRST
                   POINTER NAME-POINTER.
           IF MIDDLE-INITIAL-2 NOT = SPACES
               STRING MIDDLE-INITIAL-2 DELIMITED BY SIZE
                   '. ' DELIMITED BY SIZE
                   INTO FULL-NAME-LAST-FIRST
                   POINTER NAME-POINTER.
           IF MIDDLE-INITIAL-3 NOT = SPACES
               STRING MIDDLE-INITIAL-3 DELIMITED BY SIZE
                   '. ' DELIMITED BY SIZE
                   INTO FULL-NAME-LAST-FIRST
                   POINTER NAME-POINTER.
           IF SUFFIX NOT = SPACES
               STRING SUFFIX DELIMITED BY SPACE
                   INTO FULL-NAME-LAST-FIRST
                   POINTER NAME-POINTER.
           DISPLAY 'YOUR NAME WILL BE RECORDED AS: '
               FULL-NAME-LAST-FIRST.
           STOP RUN.
       0100-EXTRA-NAMES.
           MOVE SUFFIX TO LAST-NAME.
           UNSTRING FULL-NAME-INPUT DELIMITED BY ALL SPACES
               INTO SUFFIX
               POINTER NAME-POINTER.

