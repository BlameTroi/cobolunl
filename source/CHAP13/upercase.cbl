       IDENTIFICATION DIVISION.
       PROGRAM-ID. Upercase.
       WORKING-STORAGE SECTION.
       01  INPUT-INFO             PIC X(40) VALUE SPACES.
       01  SAVE-UPPER             PIC X(40).
       PROCEDURE DIVISION.
       0100-MAIN.
           DISPLAY 'Enter a message please '.
           ACCEPT INPUT-INFO.
           DISPLAY 'Your message in all caps: '
               FUNCTION UPPER-CASE (INPUT-INFO).
           DISPLAY 'Your original message: '
               INPUT-INFO.
           MOVE FUNCTION UPPER-CASE (INPUT-INFO) TO SAVE-UPPER.
           STOP RUN.

