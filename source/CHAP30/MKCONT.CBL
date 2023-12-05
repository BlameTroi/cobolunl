       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MKCONT.

      *=================================================================
      * This creates an indexed file for CGIMAINT containing CONTACT
      * information.
      *
      * This program has been compiled and tested with Micro Focus
      * COBOL 4.0.32 under Windows NT 4.0 SP3.
      *=================================================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTACT ASSIGN CONTACT-FILESPEC
               ORGANIZATION INDEXED
               ACCESS       DYNAMIC
               RECORD KEY   CONTACT-KEY
               FILE STATUS  CONTACT-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CONTACT LABEL RECORDS STANDARD.
       01  CONTACT-RECORD.
         05  CONTACT-KEY.
           10  CONTACT-LAST-NAME   PIC X(30).
           10  CONTACT-FIRST-NAME  PIC X(15).
         05  CONTACT-STREET        PIC X(80).
         05  CONTACT-CITY          PIC X(50).
         05  CONTACT-PHONE         PIC 9(10).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CONTACT-FILESPEC        PIC X(13) VALUE "CONTACT.DAT".
       01  CONTACT-FILESTATUS      PIC X(2).
         88  CONTACT-IO-OK                 VALUE "00" THRU "09".

       PROCEDURE DIVISION.

      *=================================================================
       MAIN.
      *=================================================================
           OPEN OUTPUT CONTACT
           IF NOT CONTACT-IO-OK
               DISPLAY "Unable to open file CONTACT for output"
           END-IF

           IF CONTACT-IO-OK
               MOVE "Jackson"                   TO CONTACT-LAST-NAME
               MOVE "William"                   TO CONTACT-FIRST-NAME
               MOVE "234 Sutherland Drive"      TO CONTACT-STREET
               MOVE "Toronto"                   TO CONTACT-CITY
               MOVE 4169324856                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Edwards"                   TO CONTACT-LAST-NAME
               MOVE "Michelle"                  TO CONTACT-FIRST-NAME
               MOVE "43 Windsor Drive  Apt 38"  TO CONTACT-STREET
               MOVE "Mississauga"               TO CONTACT-CITY
               MOVE 4166844059                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Zimmermann"                TO CONTACT-LAST-NAME
               MOVE "Jason"                     TO CONTACT-FIRST-NAME
               MOVE "3456 Fifth Avenue Apt 376" TO CONTACT-STREET
               MOVE "New York"                  TO CONTACT-CITY
               MOVE 2125784225                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Lopez"                     TO CONTACT-LAST-NAME
               MOVE "Angelo"                    TO CONTACT-FIRST-NAME
               MOVE "2241 Boston Boulevard"     TO CONTACT-STREET
               MOVE "Springfield"               TO CONTACT-CITY
               MOVE 033474239                   TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Ho"                        TO CONTACT-LAST-NAME
               MOVE "Jun-Jin"                   TO CONTACT-FIRST-NAME
               MOVE "424 Jasper Ave Suite 600"  TO CONTACT-STREET
               MOVE "Edmonton"                  TO CONTACT-CITY
               MOVE 4034571908                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Dubois"                    TO CONTACT-LAST-NAME
               MOVE "George"                    TO CONTACT-FIRST-NAME
               MOVE "114 Rue Viger"             TO CONTACT-STREET
               MOVE "Montreal"                  TO CONTACT-CITY
               MOVE 5143789135                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Heath"                     TO CONTACT-LAST-NAME
               MOVE "Michael"                   TO CONTACT-FIRST-NAME
               MOVE "42 Corporate Row"          TO CONTACT-STREET
               MOVE "St Louis"                  TO CONTACT-CITY
               MOVE 3143478612                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Randell"                   TO CONTACT-LAST-NAME
               MOVE "Tony"                      TO CONTACT-FIRST-NAME
               MOVE "15 Gillette Park"          TO CONTACT-STREET
               MOVE "Boston"                    TO CONTACT-CITY
               MOVE 6174786423                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Pike"                      TO CONTACT-LAST-NAME
               MOVE "John"                      TO CONTACT-FIRST-NAME
               MOVE "119 Sansome Street"        TO CONTACT-STREET
               MOVE "San Francisco"             TO CONTACT-CITY
               MOVE 4153498732                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Nathon-Connor"             TO CONTACT-LAST-NAME
               MOVE "Roxanne"                   TO CONTACT-FIRST-NAME
               MOVE "234 Fifth Avenue"          TO CONTACT-STREET
               MOVE "Vancouver"                 TO CONTACT-CITY
               MOVE 6045836177                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Gabriel"                   TO CONTACT-LAST-NAME
               MOVE "Jacob"                     TO CONTACT-FIRST-NAME
               MOVE "3443 Rio Grande"           TO CONTACT-STREET
               MOVE "Orlando"                   TO CONTACT-CITY
               MOVE 4074489774                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Kim"                       TO CONTACT-LAST-NAME
               MOVE "Hyo-Min"                   TO CONTACT-FIRST-NAME
               MOVE "234 Mitchel Street"        TO CONTACT-STREET
               MOVE "Atlanta"                   TO CONTACT-CITY
               MOVE 4044531459                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Simon"                     TO CONTACT-LAST-NAME
               MOVE "Darryl"                    TO CONTACT-FIRST-NAME
               MOVE "137 King Street"           TO CONTACT-STREET
               MOVE "Washington"                TO CONTACT-CITY
               MOVE 2023248975                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           IF CONTACT-IO-OK
               MOVE "Ramir"                     TO CONTACT-LAST-NAME
               MOVE "Ahmed"                     TO CONTACT-FIRST-NAME
               MOVE "32 Riverside Drive"        TO CONTACT-STREET
               MOVE "Ottawa"                    TO CONTACT-CITY
               MOVE 6133987340                  TO CONTACT-PHONE
               WRITE CONTACT-RECORD
           END-IF

           CLOSE CONTACT
           STOP RUN
           .

