       IDENTIFICATION DIVISION.
       PROGRAM-ID. Examp43.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIELD-A                 PIC X VALUE SPACE.
       01  FIELD-B                 PIC X VALUE SPACE.
       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'ENTER FIELD-A'.
           ACCEPT FIELD-A.
           DISPLAY 'ENTER FIELD-B'.
           ACCEPT FIELD-B.

           DISPLAY 'Performing 0100 thru 0800'.
           PERFORM 0100-PARAGRAPH THRU 0800-PARAGRAPH.
           DISPLAY 'Back from 0100 thru 0800 and done'.
           STOP RUN.

       0100-PARAGRAPH.

           DISPLAY 'In 0100'.
           IF FIELD-A = FIELD-B
              DISPLAY 'A = B, Performing 0400 thru 1100'
              PERFORM 0400-PARAGRAPH THRU 1100-PARAGRAPH
              DISPLAY 'Back from 0400 thru 1100'.
           DISPLAY 'Exiting 0100'.

       0400-PARAGRAPH.

           DISPLAY 'In 0400, changing B to HIGH-VALUES'.
           MOVE HIGH-VALUES TO FIELD-B.

       0800-PARAGRAPH.

           DISPLAY 'In 0800'.

       1100-PARAGRAPH.

           DISPLAY 'In 1100'.
