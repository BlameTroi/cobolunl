       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGIPOST.

      *=================================================================
      * This program illustrates a CGI program which supports the POST
      * request method.
      *
      * This program uses X/OPEN extensions to the DISPLAY/ACCEPT verb
      * to query the value of  environment variables.  These extensions
      * are supported by many COBOL compilers.  In the event they are
      * not supported, it is possible to call operating system functions
      * instead.
      *
      * This program creates a simple form with a single entry field
      * which uses the "POST" request method to interact with this
      * program.
      *=================================================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  STDIN ASSIGN  "STDIN"
               ORGANIZATION LINE SEQUENTIAL
               ACCESS       SEQUENTIAL
               FILE STATUS  STDIN-FILESTATUS.
           SELECT  STDOUT ASSIGN  "STDOUT"
               ORGANIZATION LINE SEQUENTIAL
               ACCESS       SEQUENTIAL
               FILE STATUS  STDOUT-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  STDIN LABEL RECORDS STANDARD.
       01  STDIN-RECORD.
         05  FILLER                PIC X OCCURS 1 TO 256
                                   DEPENDING ON CONTENT-LENGTH.

       FD  STDOUT LABEL RECORDS STANDARD.
       01  STDOUT-RECORD           PIC X(256).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  REQUEST-METHOD          PIC X(4)    VALUE SPACE.
       01  CONTENT-LENGTH-X        PIC X(4).
       01  CONTENT-LENGTH          PIC 9(4).

       01  FIELD-NAME              PIC X(10)   VALUE SPACE.
       01  FIELD-VALUE             PIC X(249)  VALUE SPACE.
       01  FV-PTR                  PIC 9(4) BINARY.
       01  TEMP-VALUE              PIC X(249)  VALUE SPACE.
       01  TEMP-PTR                PIC 9(4) BINARY.
       01  TEMP-CNT                PIC 9(4) BINARY.

       01  CHAR-ENCODED.
         05  CHAR-ENCODED-BINARY   PIC 99 BINARY OCCURS 2.
       01  CHAR-DECODED.
         05  CHAR-DECODED-BINARY   PIC 99 BINARY.
       01  CHAR-HEX                PIC X(16) VALUE "0123456789ABCDEF".
       01  CHAR-DECIMAL            PIC X(16) VALUE
                                  X"000102030405060708090A0B0C0D0E0F".

       01  STDIN-FILESTATUS        PIC X(2).
       01  STDOUT-FILESTATUS       PIC X(2).

       PROCEDURE DIVISION.

      *=================================================================
       MAIN.
      *=================================================================
           PERFORM GET-INPUT
           PERFORM PUT-OUTPUT
           STOP RUN
           .

      *=================================================================
       GET-INPUT.
      *-----------------------------------------------------------------
      * The URL encoded string is retrieved from the standard input
      * buffer using the file STDIN.
      *
      * STDIN is declared as a variable length LINE SEQUENTIAL file.
      * The environment variable CONTENT_LENGTH is used to set the
      * length of STDIN-RECORD.
      *=================================================================
           DISPLAY "REQUEST_METHOD" UPON ENVIRONMENT-NAME
           ACCEPT  REQUEST-METHOD   FROM ENVIRONMENT-VALUE
           IF REQUEST-METHOD = "POST"
               DISPLAY "CONTENT_LENGTH" UPON ENVIRONMENT-NAME
               ACCEPT  CONTENT-LENGTH-X FROM ENVIRONMENT-VALUE
               MOVE FUNCTION NUMVAL(CONTENT-LENGTH-X) TO CONTENT-LENGTH
               IF CONTENT-LENGTH > 0
                   OPEN INPUT STDIN
                   READ STDIN
                   CLOSE STDIN
                   PERFORM DECODE-URL-STRING
               END-IF
           END-IF
           .

      *=================================================================
       DECODE-URL-STRING.
      *-----------------------------------------------------------------
      * This paragraph converts encoded characters to their ascii values.
      *
      * Encoded strings consist of name/value pairs separated by the '&'
      * symbol.  The '+' (plus) symbol is used to denote a space.
      * The '%' is used to denote a 2 digit hexadecimal number which
      * represents an encoded symbol.
      *
      * For example:
      *        FIELD1=some+value&FIELD2=some+other+value%21
      *
      *     becomes:
      *         FIELD1=some value
      *         FIELD2=some other value!
      *
      * This routine expects a single field.
      *
      *            FIELD-VALUE
      *            FN-PTR      - length of FIELD-NAME
      *            FV-PTR      - length of FIELD-VALUE
      *=================================================================
      * Parse the name from the string
           UNSTRING STDIN-RECORD DELIMITED ALL "=" OR "&"
               INTO FIELD-NAME
                    TEMP-VALUE
           END-UNSTRING

      * STDIN-RECORD is preserved to display later otherwise
      * the following INSPECT could be performed upon STDIN-RECORD
      * instead of each field value.
           INSPECT TEMP-VALUE REPLACING ALL "+" BY SPACE

      * Now parse the field value for all encoded characters
           MOVE 1 TO TEMP-PTR FV-PTR
           PERFORM UNTIL TEMP-PTR >= CONTENT-LENGTH
               UNSTRING TEMP-VALUE DELIMITED ALL "%"
                   INTO    FIELD-VALUE (FV-PTR:)
                   COUNT   TEMP-CNT
                   POINTER TEMP-PTR
               END-UNSTRING
               ADD TEMP-CNT TO FV-PTR
               IF TEMP-PTR < CONTENT-LENGTH
                   MOVE TEMP-VALUE (TEMP-PTR:2) TO CHAR-ENCODED
                   PERFORM CONVERT-TO-CHARACTER

      * Put the character back in TEMP-VALUE and reposition
      * the pointer to start at the character. The converted character
      * will be picked up by the next UNSTRING
                   MOVE CHAR-DECODED TO TEMP-VALUE (TEMP-PTR + 1:1)
                   ADD 1 TO TEMP-PTR
               END-IF
           END-PERFORM
           .

       CONVERT-TO-CHARACTER.
      *-----------------------------------------------------------------
      * This section converts an encoded character to it's ASCII value.
      * Each of 2 bytes are converted from their ASCII display value
      * to a binary numeric value. These values are used to compute
      * the binary value of the ASCII character.
      *
      * Input:     CHAR-ENCODED
      * Output:    CHAR-DECODED
      *=================================================================
           INSPECT CHAR-ENCODED CONVERTING CHAR-HEX TO CHAR-DECIMAL
           COMPUTE CHAR-DECODED-BINARY =
               (16 * CHAR-ENCODED-BINARY (1)) + CHAR-ENCODED-BINARY (2)
           END-COMPUTE
           .

      *=================================================================
       PUT-OUTPUT.
      *-----------------------------------------------------------------
      * This section creates a minimal HTML document to accept and
      * display an  input string. HTML strings can be written as
      * separate records or concatenated in any arbitrary fashion.
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
           MOVE "<TITLE>COBOL CGI POST Example</TITLE>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "</HEAD>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<BODY>"   TO STDOUT-RECORD
           WRITE STDOUT-RECORD

      * Prompt the user for input
           MOVE "Type something in field and press " TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<STRONG>Submit</STRONG>." To STDOUT-RECORD
           WRITE STDOUT-RECORD

      * Create a form for the user to enter information
           MOVE "<FORM METHOD=POST ACTION=CGIPOST.EXE>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<INPUT NAME=FIELD MAXLENGTH=80>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<INPUT TYPE=SUBMIT VALUE=SUBMIT>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "</FORM>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD

      * When this program is invoked the first time, the request method
      * is 'GET'.  This edit prevents the error message from appearing
      * the first time the page is created.
           IF REQUEST-METHOD = "GET"
               CONTINUE
           ELSE
               IF FIELD-VALUE = SPACE
                   MOVE "<P>You did not type anything in "
                     TO STDOUT-RECORD
                   WRITE STDOUT-RECORD
                   MOVE FIELD-NAME    TO STDOUT-RECORD
                   WRITE STDOUT-RECORD
               ELSE
      *   Echo the user's input
                   MOVE "<P>You typed <EM>" TO STDOUT-RECORD
                   WRITE STDOUT-RECORD
                   MOVE FIELD-VALUE         TO STDOUT-RECORD
                   WRITE STDOUT-RECORD
                   MOVE "</EM> into "       TO STDOUT-RECORD
                   WRITE STDOUT-RECORD
                   MOVE FIELD-NAME          TO STDOUT-RECORD
                   WRITE STDOUT-RECORD
                   Move "<P>The length is " TO STDOUT-RECORD
                   WRITE STDOUT-RECORD
                   MOVE CONTENT-LENGTH-X    TO STDOUT-RECORD
                   WRITE STDOUT-RECORD

      *   Echo the URL encoded string
                   MOVE "<P>It arrived as <EM>" TO STDOUT-RECORD
                   WRITE STDOUT-RECORD
                   MOVE STDIN-RECORD   TO STDOUT-RECORD
                   WRITE STDOUT-RECORD
                   MOVE "</EM>"        TO STDOUT-RECORD
                   WRITE STDOUT-RECORD
               END-IF
           END-IF

      * HTML footer
           MOVE "</BODY>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "</HTML>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           CLOSE STDOUT
           .

