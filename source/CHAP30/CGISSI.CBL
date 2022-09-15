       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CGISSI.

      *=================================================================
      * This program provides file maintenance on an indexed file which
      * tracks name and address information for contacts.
      *
      * This program uses X/OPEN extensions to the DISPLAY/ACCEPT verb
      * to query the value of  environment variables.  These extensions
      * are supported by many COBOL compilers.  In the event they are
      * not supported, it is possible to call operating system functions
      * instead.
      *
      * This program creates a form which permits the editing of a
      * contact file. THis program uses server-side includes and the
      * CGISSI.STM base document to provide output.
      *
      * A sample contact file can be created by compiling and running
      * MKCONT.CBL.
      *
      * This program has been compiled and tested with Micro Focus
      * COBOL 4.0.32 on a Windows NT Workstation 4.0 SP3 machine running
      * Microsoft Peer Web Server 3.0.
      *
      *=================================================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STDIN  ASSIGN "STDIN"
               ORGANIZATION LINE SEQUENTIAL
               ACCESS       SEQUENTIAL
               FILE STATUS  STDIN-FILESTATUS.
           SELECT STDOUT ASSIGN "STDOUT"
               ORGANIZATION LINE SEQUENTIAL
               ACCESS       SEQUENTIAL
               FILE STATUS  STDOUT-FILESTATUS.
           SELECT CONTACT ASSIGN "CONTACT.DAT"
               ORGANIZATION INDEXED
               ACCESS       DYNAMIC
               RECORD KEY   CONTACT-KEY
               FILE STATUS  CONTACT-FILESTATUS.
           SELECT SSI ASSIGN SSI-FILESPEC
               ORGANIZATION LINE SEQUENTIAL
               ACCESS       SEQUENTIAL
               FILE STATUS  SSI-FILESTATUS.

      *=================================================================
       DATA DIVISION.
      *=================================================================

      *-----------------------------------------------------------------
       FILE SECTION.
      *-----------------------------------------------------------------
       FD  STDIN LABEL RECORDS STANDARD.
       01  STDIN-RECORD.
         05  FILLER                PIC X OCCURS 1 TO 2048
                                   DEPENDING ON CONTENT-LENGTH.

       FD  STDOUT LABEL RECORDS STANDARD.
       01  STDOUT-RECORD           PIC X(256).

       FD  CONTACT LABEL RECORDS STANDARD.
       01  CONTACT-RECORD.
         05  CONTACT-KEY.
           10  CONTACT-LAST-NAME   PIC X(30).
           10  CONTACT-FIRST-NAME  PIC X(15).
         05  CONTACT-STREET        PIC X(80).
         05  CONTACT-CITY          PIC X(50).
         05  CONTACT-PHONE         PIC 9(10).

       FD  SSI LABEL RECORDS STANDARD.
       01  SSI-RECORD           PIC X(256).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  REQUEST-METHOD          PIC X(4)    VALUE SPACE.
       01  CONTENT-LENGTH-X        PIC X(4).
       01  CONTENT-LENGTH          PIC 9(4) BINARY.

       01  FIELD-NAME              PIC X(20)   VALUE SPACE.
       01  FIELD-VALUE             PIC X(160)  VALUE SPACE.
       01  FV-PTR                  PIC 9(4) BINARY.
       01  TEMP-VALUE              PIC X(160)  VALUE SPACE.
       01  TEMP-PTR                PIC 9(4) BINARY.
       01  TEMP-CNT                PIC 9(4) BINARY.
       01  IN-PTR                  PIC 9(4) BINARY VALUE 1.

       01  CHAR-ENCODED.
         05  CHAR-ENCODED-BINARY   PIC 99 BINARY OCCURS 2.
       01  CHAR-DECODED            PIC X.
       01  CHAR-DECODED-BINARY REDEFINES CHAR-DECODED
                                   PIC 99 BINARY.
       01  CHAR-PTR                PIC 99 BINARY.
       01  CHAR-HEX                PIC X(16) VALUE "0123456789ABCDEF".
       01  CHAR-DECIMAL            PIC X(16) VALUE
                                  X"000102030405060708090A0B0C0D0E0F".

       01  STDIN-FILESTATUS        PIC X(2).
       01  STDOUT-FILESTATUS       PIC X(2).
       01  CONTACT-FILESTATUS      PIC X(2).
         88  CONTACT-IO-OK                   VALUE "00" THRU "09".
       01  SSI-FILESPEC            PIC X(40).
       01  SSI-FILESTATUS          PIC X(2).
         88  SSI-IO-OK                    VALUE "00" THRU "09".

       01  CLIENT-REQUEST          PIC X(8).
       01  STATUS-MESSAGE          PIC X(80).

       01  CONTACT-RECORD-SAVE     PIC X(185).
       01  SSI-VALUE               PIC X(81).
       01  SSI-PTR                 PIC 9(4) BINARY.


       PROCEDURE DIVISION.

      *=================================================================
       MAIN.
      *=================================================================
           PERFORM GET-INPUT
           EVALUATE FUNCTION UPPER-CASE(CLIENT-REQUEST)
             WHEN SPACE
               MOVE "Please read the instructions" TO STATUS-MESSAGE
             WHEN "ADD"
               PERFORM WRITE-CONTACT
             WHEN "CHANGE"
               PERFORM CHANGE-CONTACT
             WHEN "INQUIRE"
               PERFORM READ-CONTACT
             WHEN "DELETE"
              PERFORM DELETE-CONTACT
           END-EVALUATE
           PERFORM PUT-OUTPUT
           STOP RUN
           .

      *=================================================================
       GET-INPUT.
      *-----------------------------------------------------------------
      * This paragraph retrieves the input string from STDIN.
      * STDIN is provided by the web server.
      *
      * The length of the STDIN stream is passed in the environment
      * variable CONTENT-LENGTH.
      *=================================================================
           DISPLAY "REQUEST_METHOD" UPON ENVIRONMENT-NAME
           ACCEPT  REQUEST-METHOD   FROM ENVIRONMENT-VALUE
           IF REQUEST-METHOD = "POST"
               DISPLAY "CONTENT_LENGTH" UPON ENVIRONMENT-NAME
               ACCEPT  CONTENT-LENGTH-X FROM ENVIRONMENT-VALUE
               MOVE FUNCTION NUMVAL(CONTENT-LENGTH-X) TO CONTENT-LENGTH
               IF CONTENT-LENGTH > 0
                   MOVE SPACE TO STDIN-RECORD
                   OPEN INPUT STDIN
                   READ STDIN
                   CLOSE STDIN
                   INITIALIZE CONTACT-RECORD
                   PERFORM FILL-CONTACT-RECORD
               END-IF
           END-IF
           .

      *=================================================================
       FILL-CONTACT-RECORD.
      *-----------------------------------------------------------------
      * This section parses STDIN-RECORD placing the returned values
      * in their corresponding fields in CONTACT-RECORD.
      *=================================================================
           MOVE 1 TO IN-PTR
           PERFORM UNTIL IN-PTR > CONTENT-LENGTH
               PERFORM DECODE-URL-STRING
               EVALUATE FIELD-NAME
                 WHEN "CLIENT_REQUEST"
                   MOVE FIELD-VALUE    TO CLIENT-REQUEST
                 WHEN "LAST_NAME"
                   MOVE FIELD-VALUE    TO CONTACT-LAST-NAME
                 WHEN "FIRST_NAME"
                   MOVE FIELD-VALUE    TO CONTACT-FIRST-NAME
                 WHEN "STREET"
                   MOVE FIELD-VALUE    TO CONTACT-STREET
                 WHEN "CITY"
                   MOVE FIELD-VALUE    TO CONTACT-CITY
                 WHEN "PHONE"
                   MOVE FUNCTION NUMVAL(FIELD-VALUE) TO CONTACT-PHONE
                 END-EVALUATE
           END-PERFORM
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
      * Input:     STDIN-RECORD
      *            CONTENT-LENGTH  - length of STDIN-RECORD
      * Output:    FIELD-NAME
      *            FIELD-VALUE
      *=================================================================
           INSPECT STDIN-RECORD REPLACING ALL "+" BY SPACE
           UNSTRING STDIN-RECORD DELIMITED ALL "=" OR "&"
               INTO    FIELD-NAME
                       TEMP-VALUE
               POINTER IN-PTR
           END-UNSTRING
           INSPECT TEMP-VALUE REPLACING ALL "+" BY SPACE
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
                   MOVE CHAR-DECODED
                     TO TEMP-VALUE (TEMP-PTR + 1:1)
                   ADD 1 TO TEMP-PTR
               END-IF
           END-PERFORM
           .

      *=================================================================
       CONVERT-TO-CHARACTER.
      *-----------------------------------------------------------------
      * This paragraph converts an encoded character to it's ASCII
      * value. The 2 digits are converted to their decimal
      * equivalents and then used to create the actual character.
      *
      * Note: compiler must be set to NOTRUNC for this to work otherwise
      * the results are unpredictable for values of characters
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
       WRITE-CONTACT.
      *=================================================================
           OPEN I-O CONTACT
           IF CONTACT-IO-OK
               WRITE CONTACT-RECORD
               IF CONTACT-IO-OK
                   MOVE "Record added"     TO STATUS-MESSAGE
               ELSE
                   MOVE "Record not added" TO STATUS-MESSAGE
               END-IF
               CLOSE CONTACT
           ELSE
               MOVE "Error opening contact file" TO STATUS-MESSAGE
           END-IF
           .

      *=================================================================
       READ-CONTACT.
      *=================================================================
           OPEN INPUT CONTACT
           IF CONTACT-IO-OK
               READ CONTACT
               IF CONTACT-IO-OK
                   MOVE "Record retrieved" TO STATUS-MESSAGE
               ELSE
                   MOVE "Record not found" TO STATUS-MESSAGE
               END-IF
               CLOSE CONTACT
           ELSE
               MOVE "Error opening contact file" TO STATUS-MESSAGE
           END-IF
           .

      *=================================================================
       CHANGE-CONTACT.
      *=================================================================
           OPEN I-O CONTACT
           IF CONTACT-IO-OK
               MOVE CONTACT-RECORD TO CONTACT-RECORD-SAVE
               READ CONTACT LOCK
               IF CONTACT-IO-OK
                   MOVE CONTACT-RECORD-SAVE TO CONTACT-RECORD
                   REWRITE CONTACT-RECORD
               END-IF
               IF CONTACT-IO-OK
                   MOVE "Record changed" TO STATUS-MESSAGE
               ELSE
                   MOVE "Unable to change record" TO STATUS-MESSAGE
               END-IF
               CLOSE CONTACT
           ELSE
               MOVE "Error opening contact file" TO STATUS-MESSAGE
           END-IF
           .

      *=================================================================
       DELETE-CONTACT.
      *=================================================================
           OPEN I-O CONTACT
           IF CONTACT-IO-OK
               DELETE CONTACT
               IF CONTACT-IO-OK
                   MOVE "Record deleted" TO STATUS-MESSAGE
               ELSE
                   MOVE "Unable to delete record" TO STATUS-MESSAGE
               END-IF
               CLOSE CONTACT
           ELSE
               MOVE "Error opening contact file" TO STATUS-MESSAGE
           END-IF
           .

      *=================================================================
       PUT-OUTPUT.
      *-----------------------------------------------------------------
      * This paragraph creates #include files for each of the pieces
      * to be merged with the base document. It then redirects the
      * client to the base document. The #includes are resolved by
      * the web server.
      *=================================================================
      * Create #include files used by CGISSI.STM
           MOVE "LAST_NAME.htm"    TO SSI-FILESPEC
           MOVE CONTACT-LAST-NAME  TO SSI-VALUE
           PERFORM WRITE-VALUE-SSI

           MOVE "FIRST_NAME.htm"   TO SSI-FILESPEC
           MOVE CONTACT-FIRST-NAME TO SSI-VALUE
           PERFORM WRITE-VALUE-SSI

           MOVE "STREET.htm"   TO SSI-FILESPEC
           MOVE CONTACT-STREET TO SSI-VALUE
           PERFORM WRITE-VALUE-SSI

           MOVE "CITY.htm"     TO SSI-FILESPEC
           MOVE CONTACT-CITY   TO SSI-VALUE
           PERFORM WRITE-VALUE-SSI

           MOVE "PHONE.htm"    TO SSI-FILESPEC
           MOVE CONTACT-PHONE  TO SSI-VALUE
           PERFORM WRITE-VALUE-SSI

           MOVE "PROMPT.htm"   TO SSI-FILESPEC
           OPEN OUTPUT SSI
           MOVE STATUS-MESSAGE TO SSI-RECORD
           WRITE SSI-RECORD
           CLOSE SSI

      * Redirect client to SSI document
           OPEN OUTPUT STDOUT
           MOVE "Location: CGISSI.STM" TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           MOVE SPACE      TO STDOUT-RECORD
           WRITE STDOUT-RECORD
           CLOSE STDOUT
           .

      *=================================================================
       WRITE-VALUE-SSI.
      *-----------------------------------------------------------------
      * Format a VALUE clause for the input field and save it as a
      * #include file.
      *=================================================================
           OPEN OUTPUT SSI
           PERFORM VARYING SSI-PTR FROM LENGTH OF SSI-VALUE BY -1
             UNTIL SSI-PTR < 1
             OR    SSI-VALUE (SSI-PTR:1) NOT = SPACE
               CONTINUE
           END-PERFORM
           MOVE QUOTE TO SSI-VALUE (SSI-PTR + 1:1)

           INITIALIZE SSI-RECORD
           STRING "VALUE="
                  QUOTE
                  SSI-VALUE DELIMITED SIZE INTO SSI-RECORD
           END-STRING
           WRITE SSI-RECORD
           CLOSE SSI
           .

