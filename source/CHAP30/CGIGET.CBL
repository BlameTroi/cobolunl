       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CGIGET.

      *=================================================================
      * This program illustrates a CGI program which supports the GET
      * request method.
      *
      * This program uses X/OPEN extensions to the DISPLAY/ACCEPT verb
      * to query the value of  environment variables.  These extensions
      * are supported by many COBOL compilers.  In the event they are
      * not supported, it is possible to call operating system functions
      * instead.
      *
      * This program creates a simple form with a single entry field
      * which uses the "GET" request method to interact with this
      * program.
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
       01  REQUEST-METHOD          PIC X(4)    VALUE SPACE.
       01  QUERY-STRING            PIC X(256)  VALUE SPACE.

       01  FIELD-NAME              PIC X(10)   VALUE SPACE.
       01  FIELD-VALUE             PIC X(249)  VALUE SPACE.
       01  QS-PTR                  PIC 9(4) BINARY.
       01  FN-PTR                  PIC 9(4) BINARY.
       01  FV-PTR                  PIC 9(4) BINARY.

       01  CHAR-ENCODED.
         05  CHAR-ENCODED-BINARY   PIC 99 BINARY OCCURS 2.
       01  CHAR-DECODED            PIC X.
       01  CHAR-DECODED-BINARY REDEFINES CHAR-DECODED
                                   PIC 99 BINARY.
       01  CHAR-PTR                PIC 99 BINARY.

       01  STDOUT-FILESTATUS       PIC XX.

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
      * This section uses an X/OPEN extension to retrieve environment
      * variable values. The GET method uses the environment variable
      * QUERY_STRING to supply the URL encoded input string.
      *=================================================================
           DISPLAY "REQUEST_METHOD" UPON ENVIRONMENT-NAME
           ACCEPT  REQUEST-METHOD   FROM ENVIRONMENT-VALUE
           IF REQUEST-METHOD = "GET"
               DISPLAY "QUERY_STRING"  UPON ENVIRONMENT-NAME
               ACCEPT QUERY-STRING     FROM ENVIRONMENT-VALUE
               PERFORM DECODE-URL-STRING
           ELSE
               MOVE "Unexpected REQUEST_METHOD" TO FIELD-VALUE
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
           MOVE 0 TO FN-PTR
           PERFORM VARYING QS-PTR FROM 1 BY 1
             UNTIL QS-PTR > LENGTH OF QUERY-STRING
             OR    QUERY-STRING (QS-PTR:1) = "="
               IF QUERY-STRING (QS-PTR:1) = "="
                   CONTINUE
               ELSE
                   ADD 1 TO FN-PTR
                   MOVE QUERY-STRING (QS-PTR:1) TO FIELD-NAME (FN-PTR:1)
               END-IF
           END-PERFORM
           ADD 1 TO QS-PTR

      * Parse the corresponding value
           MOVE 0 TO FV-PTR
           PERFORM VARYING QS-PTR FROM QS-PTR BY 1
             UNTIL QS-PTR > LENGTH OF QUERY-STRING
             OR    QUERY-STRING (QS-PTR:1) = "&"
               EVALUATE QUERY-STRING (QS-PTR:1)
                 WHEN "+"
                   ADD 1 TO FV-PTR
                   MOVE SPACE TO FIELD-VALUE (FV-PTR:1)
                 WHEN "%"
                   MOVE QUERY-STRING (QS-PTR + 1:2) TO CHAR-ENCODED
                   PERFORM CONVERT-TO-CHARACTER
                   ADD 1 TO FV-PTR
                   MOVE CHAR-DECODED  TO FIELD-VALUE (FV-PTR:1)
                   ADD 2 TO QS-PTR
                 WHEN OTHER
                   ADD 1 TO FV-PTR
                   MOVE QUERY-STRING (QS-PTR:1) TO
                        FIELD-VALUE (FV-PTR:1)
               END-EVALUATE
           END-PERFORM
           .

      *=================================================================
       CONVERT-TO-CHARACTER.
      *-----------------------------------------------------------------
      * This section converts an encoded character to it's ASCII value.
      *
      * Input:     CHAR-ENCODED
      * Output:    CHAR-DECODED
      *=================================================================
           PERFORM VARYING CHAR-PTR FROM 1 BY 1 UNTIL CHAR-PTR > 2
           EVALUATE TRUE
             WHEN (CHAR-ENCODED-BINARY (CHAR-PTR) >= 48) AND
                  (CHAR-ENCODED-BINARY (CHAR-PTR) <= 57)
               SUBTRACT 48 FROM CHAR-ENCODED-BINARY (CHAR-PTR)
             WHEN (CHAR-ENCODED-BINARY (CHAR-PTR) >= 65) AND
                  (CHAR-ENCODED-BINARY (CHAR-PTR) <= 70)
               SUBTRACT 55 FROM CHAR-ENCODED-BINARY (CHAR-PTR)
           END-EVALUATE
           END-PERFORM
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
           MOVE "<TITLE>COBOL CGI GET Example</TITLE>" TO STDOUT-RECORD
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
           MOVE "<FORM METHOD=GET ACTION=CGIGET.EXE>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<INPUT NAME=FIELD MAXLENGTH=80>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "<INPUT TYPE=SUBMIT VALUE=SUBMIT>" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "</FORM>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD

           IF FIELD-VALUE = SPACE
               CONTINUE
           ELSE
      *      Echo the user's input
               MOVE "<P>You typed <EM>" TO STDOUT-RECORD
               WRITE STDOUT-RECORD
               MOVE FIELD-VALUE         TO STDOUT-RECORD
               WRITE STDOUT-RECORD
               MOVE "</EM> into "       TO STDOUT-RECORD
               WRITE STDOUT-RECORD
               MOVE FIELD-NAME          TO STDOUT-RECORD
               WRITE STDOUT-RECORD

      *      Echo the URL encoded string
               MOVE "<P>It arrived as <EM>" TO STDOUT-RECORD
               WRITE STDOUT-RECORD
               MOVE QUERY-STRING   TO STDOUT-RECORD
               WRITE STDOUT-RECORD
               MOVE "</EM>"        TO STDOUT-RECORD
               WRITE STDOUT-RECORD
           END-IF

      * HTML footer
           MOVE "</BODY>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE "</HTML>"  TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           CLOSE STDOUT
           .

