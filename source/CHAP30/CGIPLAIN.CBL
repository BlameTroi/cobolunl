       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CGIPLAIN.
      *=================================================================
      *
      * This program illustrates using a MIME header emit a statement
      * as plain text.
      *
      * This is one of the simplest CGI programs possible.  It accepts
      * no input data and emits and emits only a MIME header and a line
      * of text.
      *
      *=================================================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  STDOUT ASSIGN  "STDOUT"
               ORGANIZATION LINE SEQUENTIAL
               ACCESS       SEQUENTIAL
               FILE STATUS  STDOUT-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  STDOUT LABEL RECORDS STANDARD.
       01  STDOUT-RECORD           PIC X(256).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  STDOUT-FILESTATUS       PIC XX.

       PROCEDURE DIVISION.

      *=================================================================
       MAIN.
      *=================================================================
           OPEN OUTPUT STDOUT
           MOVE  "Content-type: text/plain" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE SPACE TO STDOUT-RECORD
           WRITE STDOUT-REcORD
           MOVE "Hello World!" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           CLOSE STDOUT
           STOP RUN
           .

