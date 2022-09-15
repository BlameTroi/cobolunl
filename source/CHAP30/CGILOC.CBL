       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CGILOC.
      *=================================================================
      *
      * This program illustrates using a MIME header to redirect the
      * client to a different URL. This example uses a sequential file
      * and provides alternatives for emitting a carriage return and
      * line feed (hex 0D0A).
      *
      * This is one of the simplest CGI programs possible.  It accepts
      * no input data and emits and emits only a MIME header.
      *
      *=================================================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  STDOUT ASSIGN  "STDOUT"
               ORGANIZATION SEQUENTIAL
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

      * Carriage return/Line Feed
        01  CRLF                    PIC XX VALUE X"0D0A".

      * Alternative method using a binary numeric. Depending on the
      * compiler and operating system, the value may need to be 2573.
      * It depends on whether the byte order is swapped.
      *
      *01  CRLF.
      *  05  FILLER                PIC S9(4) BINARY VALUE 3338.
      *
      * Yet another technique which avoids the byte ordering problem
      *01  CRLF.
      *  05  FILLER                PIC 99 BINARY VALUE 13.
      *  05  FILLER                PIC 99 BINARY VALUE 10.
      *

       PROCEDURE DIVISION.

      *=================================================================
       MAIN.
      *=================================================================
           OPEN OUTPUT STDOUT
           INITIALIZE STDOUT-RECORD
           STRING "Location: http://www.mcp.com"
                  CRLF
                  CRLF     DELIMITED SIZE INTO STDOUT-RECORD
           END-STRING
           WRITE STDOUT-RECORD
           CLOSE STDOUT
           STOP RUN
           .

