       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CGISHOW.
      *=================================================================
      * This program shows the values of various environment variables
      * passed by the web server to a CGI program.
      *
      * This program uses X/OPEN extensions to the DISPLAY/ACCEPT verb
      * to query the value of  environment variables.  These extensions
      * are supported by many COBOL compilers.  In the event they are
      * not supported, it is possible to call operating system functions
      * instead.
      *
      * This program creates a simple form with forms for both GET
      * and SHOW request methods.
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
       01  VAR-NAMES.
         05  FILLER PIC X(18) VALUE "SERVER_SOFTWARE".
         05  FILLER PIC X(18) VALUE "SERVER_NAME".
         05  FILLER PIC X(18) VALUE "GATEWAY_INTERFACE".
         05  FILLER PIC X(18) VALUE "SERVER_PROTOCOL".
         05  FILLER PIC X(18) VALUE "SERVER_PORT".
         05  FILLER PIC X(18) VALUE "REQUEST_METHOD".
         05  FILLER PIC X(18) VALUE "PATH_INFO".
         05  FILLER PIC X(18) VALUE "PATH_TRANSLATED".
         05  FILLER PIC X(18) VALUE "SCRIPT_NAME".
         05  FILLER PIC X(18) VALUE "QUERY_STRING".
         05  FILLER PIC X(18) VALUE "REMOTE_HOST".
         05  FILLER PIC X(18) VALUE "REMOTE_ADDR".
         05  FILLER PIC X(18) VALUE "AUTH_TYPE".
         05  FILLER PIC X(18) VALUE "REMOTE_USER".
         05  FILLER PIC X(18) VALUE "REMOTE_IDENT".
         05  FILLER PIC X(18) VALUE "CONTENT_TYPE".
         05  FILLER PIC X(18) VALUE "CONTENT_LENGTH".
         05  FILLER PIC X(18) VALUE "HTTP_ACCEPT".
         05  FILLER PIC X(18) VALUE "HTTP_USER_AGENT".
       01  FILLER REDEFINES VAR-NAMES.
         05  VAR-NAME              PIC X(18) OCCURS 19.
       01  VAR-VALUE               PIC X(249)  VALUE SPACE.
       01  VAR-IDX                 PIC 9(4) BINARY.

       01  STDOUT-FILESTATUS       PIC XX.

       PROCEDURE DIVISION.

      *=================================================================
       MAIN.
      *=================================================================
           OPEN OUTPUT STDOUT
      * The reply starts with a mime header followed by a blank line
           MOVE "Content-type: text/html" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE SPACE      TO STDOUT-RECORD
           WRITE STDOUT-RECORD

      * Minimal HTML header
           MOVE "<HTML>"   TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<HEAD>"   TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<TITLE>COBOL CGI SHOW Example</TITLE>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "</HEAD>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<BODY>"   TO STDOUT-RECORD
           WRITE STDOUT-RECORD

      * Give some instructions
           MOVE "<H3>Instructions</H3>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "Press either the <STRONG>GET</STRONG> button or the <ST
      -    "RONG>POST</STRONG> button to view the environment variables
      -    "passed by the web server to the CGI program."
               TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<P>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD

      * Create a GET form for the user to enter information
           MOVE "<FORM METHOD=GET ACTION=CGISHOW.EXE>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<INPUT TYPE=SUBMIT VALUE=GET>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<INPUT TYPE=HIDDEN NAME=NAME VALUE=VALUE>"
               TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "</FORM>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD

      * Create a POST form for the user to enter information
           MOVE "<FORM METHOD=POST ACTION=CGISHOW.EXE>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<INPUT TYPE=SUBMIT VALUE=POST>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<INPUT TYPE=HIDDEN NAME=NAME VALUE=VALUE>"
               TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "</FORM>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD

      * Emit the various environment variables
           MOVE "<PRE>"    TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           PERFORM VARYING VAR-IDX FROM 1 BY 1 UNTIL VAR-IDX > 19
               INITIALIZE VAR-VALUE
               DISPLAY VAR-NAME (VAR-IDX) UPON ENVIRONMENT-NAME
               ACCEPT  VAR-VALUE          FROM ENVIRONMENT-VALUE
               INITIALIZE STDOUT-RECORD
               STRING "<BR>"
                      VAR-NAME (VAR-IDX)
                      " = "
                      VAR-VALUE  DELIMITED SIZE INTO STDOUT-RECORD
               END-STRING
               WRITE STDOUT-RECORD
           END-PERFORM
           MOVE "</PRE>"    TO STDOUT-RECORD
           WRITE STDOUT-RECORD

      * HTML footer
           MOVE "</BODY>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "</HTML>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           CLOSE STDOUT

           STOP RUN
           .

