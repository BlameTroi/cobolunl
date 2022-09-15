       IDENTIFICATION DIVISION.
      * CA- Realia Specific - may need to change for other compilers.
       PROGRAM-ID. 'P_Winmain'.

      * TITLE - CHAPT08.COB
      * DESCRIPTION - CHAPT08.COB
      *
      * The Windows user interface is provided using a tool called
      * COBOL SP/2 from Flexus international.  60 Day evaluation
      * versions are avalable from Flexus at http://www.flexus.com
      * or by contacting Flexus International:
      *
      * FLEXUS INTERNATIONAL CORPORATION FAX: 610-588-9475
      * P.O. BOX 640		       VOICE: 610-588-9400
      * BANGOR PA 18013-0640		 BBS: 610-863-4740
      * U.S.A. E-MAIL: INFO@FLEXUS.COM
      *
      *
      * This program is meant as a basic example only and not a fully
      * functional system.
      *
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL MASTER-FILE ASSIGN TO "CHAPT08.DAT"
		  ORGANIZATION		 IS INDEXED
		  ACCESS		 IS DYNAMIC
		  FILE STATUS		 IS MASTER-STATUS
		  RECORD KEY		 IS MASTER-HANDLE
		  ALTERNATE RECORD KEY	 IS MASTER-NAME.
       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       01  MASTER-RECORD.
	   03  MASTER-HANDLE	  PIC X(16).
	   03  MASTER-NAME	  PIC X(30).
	   03  MASTER-ADDRESS	  PIC X(50).
	   03  MASTER-CITY	  PIC X(30).
	   03  MASTER-STATE	  PIC XX.
	   03  MASTER-POSTAL-CODE PIC X(10).

       WORKING-STORAGE SECTION.

       COPY "sp2.cpy".

       COPY "CHAPT08.cpy".
      * FLAGS
       01  END-PROGRAM-FLAG	       PIC X VALUE SPACES.
           88  EXIT-PROGRAM            VALUE "Y".
       01  MASTER-STATUS	       PIC XX.
           88  MASTER-SUCCESS          VALUE "00" "05".
           88  MASTER-DUPLICATE        VALUE "22".
           88  MASTER-MISSING          VALUE "23".
           88  MASTER-END              VALUE "10".
      * Work Fields
       01  LAST-HANDLE		       PIC X(16) VALUE LOW-VALUES.
       01  THE-DATE		       PIC 9(6)  VALUE ZEROS.
       01  THE-TIME		       PIC 9(6)  VALUE ZEROS.
      * This linkage section required by CA-Realia.  Can be removed
      * for other compilers.  Remember to change the Procedure Division
      * using statement.
       LINKAGE SECTION.
       01  INSTANCE		       PIC 9(4) BINARY.
       01  PREV-INSTANCE	       PIC 9(4) BINARY.
       01  CMD-LINE		       PIC X(120).
       01  CMD-SHOW		       PIC 9(4) BINARY.

       PROCEDURE DIVISION USING
	   BY CONTENT INSTANCE PREV-INSTANCE
	   BY REFERENCE CMD-LINE
	   BY CONTENT CMD-SHOW.

       MAINLINE.
      ******************
      * MAINLINE LOGIC *
      ******************
	   PERFORM PROC-OPEN-FILE
	   MOVE LOW-VALUES TO CHAPT08-DATA
           MOVE "CHAPT08" TO CHAPT08-NEXT-PANEL
           MOVE "y" TO CHAPT08-NEW-WINDOW
	   MOVE LOW-VALUES TO CHAPT08-FIELDS
	   MOVE LOW-VALUES TO CHAPT08-COLRS
	   MOVE LOW-VALUES TO CHAPT08-TYPES
	   PERFORM OPEN-MASTER-FILE
	   PERFORM PROC-CON-CHAPT08
	   PERFORM PROC-CLOSE-WINDOW
	   PERFORM PROC-CLOSE-FILE
	   PERFORM CLOSE-MASTER-FILE
	   PERFORM PROC-END-SESSION
	   STOP RUN.

       PROC-OPEN-FILE.
      *****************
      * OPEN SP2 FILE *
      *****************
	   MOVE LOW-VALUES TO SP2-FI-DATA
           MOVE "CHAPT08.PAN" TO SP2-FI-NAME
           CALL "SP2" USING SP2-OPEN-FILE SP2-FILE-DEF
	   .

       PROC-CON-CHAPT08.
      ******************
      * CONVERSE PANEL *
      ******************
	   PERFORM WITH TEST AFTER UNTIL EXIT-PROGRAM
             CALL "SP2" USING SP2-CONVERSE-PANEL CHAPT08-CONVERSE-DATA
	     MOVE LOW-VALUE TO CHAPT08-NEW-WINDOW

	     EVALUATE TRUE
		WHEN CHAPT08-KEY = SP2-KEY-CLOSE
		     SET EXIT-PROGRAM TO TRUE
		WHEN CHAPT08-ADD-PB-HIT
		     PERFORM ADD-RECORD
		WHEN CHAPT08-CHANGE-PB-HIT
		     PERFORM UPDATE-RECORD
		WHEN CHAPT08-DELETE-PB-HIT
		     PERFORM DELETE-RECORD
		WHEN CHAPT08-RETRIEVE-PB-HIT
		     PERFORM RETRIEVE-RECORD
		WHEN CHAPT08-NEXT-PB-HIT
		     PERFORM RETRIEVE-NEXT-RECORD
		WHEN CHAPT08-PRIOR-PB-HIT
		     PERFORM RETRIEVE-PRIOR-RECORD
		WHEN OTHER
		     CONTINUE
	     END-EVALUATE

	   END-PERFORM
	   .

       PROC-CLOSE-WINDOW.
      ************************
      * CLOSE CURRENT WINDOW *
      ************************
           CALL "SP2" USING SP2-CLOSE-WINDOW SP2-NULL-PARM
	   .

       PROC-CLOSE-FILE.
      **********************
      * CLOSE CURRENT FILE *
      **********************
           CALL "SP2" USING SP2-CLOSE-FILE SP2-NULL-PARM
	   .

       PROC-END-SESSION.
      *******************
      * END SP2 SESSION *
      *******************
           CALL "SP2" USING SP2-END-SESSION SP2-NULL-PARM
	   .
       ADD-RECORD.
	   PERFORM ASSIGN-HANDLE
	   PERFORM MOVE-SCREEN-TO-RECORD
	   PERFORM WRITE-RECORD
	   .
       UPDATE-RECORD.
	   PERFORM MOVE-SCREEN-TO-RECORD
	   PERFORM REWRITE-RECORD
	   .
       DELETE-RECORD.
	   PERFORM MOVE-SCREEN-TO-RECORD
	   PERFORM DELETE-ACTUAL-RECORD
	   .
       RETRIEVE-RECORD.
	   PERFORM MOVE-SCREEN-TO-RECORD
	   PERFORM READ-RECORD
	   .
       RETRIEVE-NEXT-RECORD.
	   PERFORM MOVE-SCREEN-TO-RECORD
	   PERFORM READ-NEXT-RECORD
	   .
       RETRIEVE-PRIOR-RECORD.
	   PERFORM MOVE-SCREEN-TO-RECORD
	   PERFORM READ-PRIOR-RECORD
	   .
       ASSIGN-HANDLE.
      * if you do not have intrinsic functions, comment out the first
      * part and use the second.
	   MOVE FUNCTION CURRENT-DATE (1:16) TO LAST-HANDLE
      *
      *  non intrinsic version
      *
      *    ACCEPT THE-DATE FROM DATE
      *    IF THE-DATE (1:2) < "98"
      *       MOVE "20" TO LAST-HANDLE (1:2)
      *    ELSE
      *       MOVE "19" TO LAST-HANDLE (1:2)
      *    END-IF
      *    MOVE THE-DATE TO LAST-HANDLE (3:6)
      *    ACCEPT THE-TIME FROM TIME
      *    MOVE THE-TIME TO LAST-HANDLE (9:8)
      *
	   .
       MOVE-SCREEN-TO-RECORD.
	   MOVE CHAPT08-NAME	    TO MASTER-NAME
	   MOVE CHAPT08-ADDRESS     TO MASTER-ADDRESS
	   MOVE CHAPT08-CITY	    TO MASTER-CITY
	   MOVE CHAPT08-STATE	    TO MASTER-STATE
	   MOVE CHAPT08-POSTAL-CODE TO MASTER-POSTAL-CODE
	   MOVE LAST-HANDLE	    TO MASTER-HANDLE
	   .
       WRITE-RECORD.
	   WRITE MASTER-RECORD
	   EVALUATE TRUE
	      WHEN MASTER-DUPLICATE
		   PERFORM SHOW-DUPLICATE-RECORD-MESSAGE
		   PERFORM CLEAR-SCREEN-RECORD
	      WHEN MASTER-SUCCESS
		   PERFORM SHOW-SUCCESS
	      WHEN OTHER
		   PERFORM SHOW-ERROR
	   END-EVALUATE
	   .
       REWRITE-RECORD.
	   REWRITE MASTER-RECORD
	   EVALUATE TRUE
	      WHEN MASTER-SUCCESS
		   PERFORM SHOW-SUCCESS
	      WHEN OTHER
		   PERFORM SHOW-ERROR
	   END-EVALUATE
	   .
       DELETE-ACTUAL-RECORD.
	   DELETE MASTER-FILE
	   EVALUATE TRUE
	      WHEN MASTER-SUCCESS
		   PERFORM SHOW-SUCCESS
	      WHEN OTHER
		   PERFORM SHOW-ERROR
	   END-EVALUATE
	   .
       READ-RECORD.
	   READ MASTER-FILE KEY MASTER-NAME
	   EVALUATE TRUE
	      WHEN MASTER-SUCCESS
		   MOVE MASTER-HANDLE TO LAST-HANDLE
		   PERFORM MOVE-RECORD-TO-SCREEN
	      WHEN OTHER
		   PERFORM SHOW-ERROR
	   END-EVALUATE
	   .
       READ-NEXT-RECORD.
	   START MASTER-FILE KEY GREATER THAN MASTER-NAME
	   EVALUATE TRUE
	      WHEN MASTER-MISSING
	      WHEN MASTER-END
		   PERFORM SHOW-NO-MORE
	      WHEN MASTER-SUCCESS
		   READ MASTER-FILE NEXT RECORD
		   EVALUATE TRUE
		      WHEN MASTER-MISSING
		      WHEN MASTER-END
			   PERFORM SHOW-NO-MORE
		      WHEN MASTER-SUCCESS
			   PERFORM MOVE-RECORD-TO-SCREEN
		      WHEN OTHER
			   PERFORM SHOW-ERROR
		   END-EVALUATE
	      WHEN OTHER
		   PERFORM SHOW-ERROR
	   END-EVALUATE
	   .
       READ-PRIOR-RECORD.
	   START MASTER-FILE KEY NOT LESS THAN MASTER-NAME
	   EVALUATE TRUE
	      WHEN MASTER-MISSING
	      WHEN MASTER-END
		   PERFORM SHOW-NO-MORE
	      WHEN MASTER-SUCCESS
      * read next first to get current record.
		   READ MASTER-FILE NEXT RECORD
		   EVALUATE TRUE
		      WHEN MASTER-MISSING
		      WHEN MASTER-END
			   PERFORM SHOW-NO-MORE
		      WHEN MASTER-SUCCESS
      * now read prior, for some compilers change PRIOR to PREVIOUS
			   READ MASTER-FILE PRIOR RECORD
			   EVALUATE TRUE
			      WHEN MASTER-MISSING
			      WHEN MASTER-END
				   PERFORM SHOW-NO-MORE
			      WHEN MASTER-SUCCESS
				   PERFORM MOVE-RECORD-TO-SCREEN
			      WHEN OTHER
				   PERFORM SHOW-ERROR
			   END-EVALUATE
		      WHEN OTHER
			   PERFORM SHOW-ERROR
		   END-EVALUATE
	      WHEN OTHER
		   PERFORM SHOW-ERROR
	   END-EVALUATE
	   .
       SHOW-DUPLICATE-RECORD-MESSAGE.
	   MOVE LOW-VALUES TO SP2-MS-DATA
           MOVE "b" TO SP2-MS-ICON
           MOVE "Duplicate Record"  TO SP2-MS-TITLE
           MOVE "Duplicate Record!" TO SP2-MS-TEXT
           MOVE "o"                 TO SP2-MS-BUTTON
	   MOVE 1		    TO SP2-MS-LINE-CNT
           CALL "SP2" USING SP2-DISPLAY-MESSAGE SP2-MESSAGE-DATA
	   .
       SHOW-SUCCESS.
	   MOVE LOW-VALUES TO SP2-MS-DATA
           MOVE "o" TO SP2-MS-ICON
           MOVE "Success"                        TO SP2-MS-TITLE
           MOVE "Request processed Successfully" TO SP2-MS-TEXT
           MOVE "o"                              TO SP2-MS-BUTTON
	   MOVE 1				 TO SP2-MS-LINE-CNT
           CALL "SP2" USING SP2-DISPLAY-MESSAGE SP2-MESSAGE-DATA
	   .
       SHOW-ERROR.
	   MOVE LOW-VALUES TO SP2-MS-DATA
           MOVE "b" TO SP2-MS-ICON
           MOVE "File Error"        TO SP2-MS-TITLE
           STRING "File Status Error "
		  MASTER-STATUS
		  DELIMITED BY SIZE
		  INTO SP2-MS-TEXT
	   END-STRING
           MOVE "o"                 TO SP2-MS-BUTTON
	   MOVE 1		    TO SP2-MS-LINE-CNT
           CALL "SP2" USING SP2-DISPLAY-MESSAGE SP2-MESSAGE-DATA
	   .
       SHOW-NO-MORE.
	   MOVE LOW-VALUES TO SP2-MS-DATA
           MOVE "o" TO SP2-MS-ICON
           MOVE "No More Records"                TO SP2-MS-TITLE
           MOVE "No more records that direction" TO SP2-MS-TEXT
           MOVE "o"                              TO SP2-MS-BUTTON
	   MOVE 1				 TO SP2-MS-LINE-CNT
           CALL "SP2" USING SP2-DISPLAY-MESSAGE SP2-MESSAGE-DATA
	   .

       MOVE-RECORD-TO-SCREEN.
	   MOVE MASTER-HANDLE	    TO LAST-HANDLE
	   MOVE MASTER-NAME	    TO CHAPT08-NAME
	   MOVE MASTER-ADDRESS	    TO CHAPT08-ADDRESS
	   MOVE MASTER-CITY	    TO CHAPT08-CITY
	   MOVE MASTER-STATE	    TO CHAPT08-STATE
	   MOVE MASTER-POSTAL-CODE  TO CHAPT08-POSTAL-CODE
	   .

       CLEAR-SCREEN-RECORD.
	   MOVE LOW-VALUES	    TO LAST-HANDLE
	   INITIALIZE		    CHAPT08-FIELDS
	   .
       OPEN-MASTER-FILE.
	   OPEN I-O MASTER-FILE
	   IF NOT MASTER-SUCCESS
	      STOP RUN
	   END-IF
      * file newly created, close and re-open, required for update
      * access by some compilers.
           IF MASTER-STATUS = "05"
	      CLOSE MASTER-FILE
	      OPEN I-O MASTER-FILE
	      IF NOT MASTER-SUCCESS
		 STOP RUN
	      END-IF
	   END-IF
	   .
       CLOSE-MASTER-FILE.
	   CLOSE MASTER-FILE
	   .
