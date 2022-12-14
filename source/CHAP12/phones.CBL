0010   IDENTIFICATION DIVISION.
       PROGRAM-ID. Phones.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PERSONNEL-FILE       ASSIGN TO "PERSFILE.DAT"
                                       LINE SEQUENTIAL.
           SELECT PHONE-LIST           ASSIGN TO "PHONELST.DAT".

       DATA DIVISION.
0020   FILE SECTION.
0030   FD  PERSONNEL-FILE
0040       LABEL RECORDS STANDARD.
0050   01  PERSONNEL-RECORD.
0060       05  INPUT-NAME                       PIC X(20).
0070       05  INPUT-NUMBER                     PIC X(4).
           05  FILLER                           PIC X(56).
0080   FD  PHONE-LIST.
0090   01  LIST-RECORD                          PIC X(80).
0100
0110   WORKING-STORAGE SECTION.
0120   01  INPUT-EOF-STATUS                     PIC XXX VALUE 'NO'.
0130     88  END-OF-FILE                               VALUE 'YES'.
0140
0150   01  COMPUTER-DATE.
0160       05  THIS-YEAR               PIC 9999.
0170       05  THIS-MONTH              PIC 99.
0180       05  THIS-DAY                PIC XX.
0190       05  FILLER                  PIC X(13).
0200
0210   01  MONTH-LIST.
0220       05  FILLER PIC X(30) VALUE 'January   February  March'.
0230       05  FILLER PIC X(30) VALUE 'April     May       June'.
0240       05  FILLER PIC X(30) VALUE 'July      August    September'.
0250       05  FILLER PIC X(30) VALUE 'October   November  December'.
0260   01  MONTH-TABLE REDEFINES MONTH-LIST.
0270       05  MONTH-NAME OCCURS 12 TIMES PIC X(10).
0280
0290   01  ROW-NR                               PIC 99 VALUE 1.
0300   01  COL-NR                               PIC 99 VALUE 1.
0310   01  PHONE-NUMBERS.
0320       05  PHONE-COLUMN OCCURS 3 TIMES.
0330           10  PHONE-ENTRY OCCURS 55 TIMES.
0340               15  PHONE-NAME               PIC X(20).
0350               15  PHONE-NUMBER             PIC X(4).
0360
0370   01  DIRECTORY-TITLE.
0380       05  FILLER                  PIC X(20) VALUE SPACES.
0390       05  FILLER                  PIC X(40) VALUE
0400           'COMPANY TELEPHONE DIRECTORY '.
0410       05  DIR-TITLE-DATE          PIC X(20) VALUE SPACES.
0420   01  DIRECTORY-HEADING.
0430       05  FILLER PIC X(27) VALUE 'NAME               NUMBER  '.
0440       05  FILLER PIC X(27) VALUE 'NAME               NUMBER  '.
0450       05  FILLER PIC X(26) VALUE 'NAME               NUMBER '.
0460   01  DIRECTORY-DETAIL            VALUE SPACES.
0470       05  DETAIL-ENTRY OCCURS 3 TIMES.
0480           10  DET-NAME            PIC X(20).
0490           10  FILLER              PIC X.
0500           10  DET-NUMBER          PIC X(4).
0510           10  FILLER              PIC XX.
0520
0530   PROCEDURE DIVISION.
0540   0000-MAIN-CONTROL.
0550       MOVE FUNCTION CURRENT-DATE TO COMPUTER-DATE.
0560       STRING MONTH-NAME (THIS-MONTH) DELIMITED BY SPACE
0570         '  ' DELIMITED BY SIZE
0580         THIS-YEAR DELIMITED BY SIZE
0590         INTO DIR-TITLE-DATE.
0600
0610       OPEN INPUT  PERSONNEL-FILE
0620            OUTPUT PHONE-LIST.
0630       PERFORM 0600-READ-INPUT.
0640       PERFORM 0100-MAIN-LOOP
0650          UNTIL END-OF-FILE.
0660       CLOSE PERSONNEL-FILE
0670             PHONE-LIST.
0680       STOP RUN.
0690
0700   0100-MAIN-LOOP.
0705       MOVE SPACES TO PHONE-NUMBERS.
0710       PERFORM VARYING COL-NR FROM 1 BY 1
0720          UNTIL COL-NR > 3
0730            OR END-OF-FILE
0740          PERFORM 0120-LOAD-TABLE
0750             VARYING ROW-NR FROM 1 BY 1
0760             UNTIL ROW-NR > 55
0770               OR END-OF-FILE.
0780       WRITE LIST-RECORD FROM DIRECTORY-TITLE
0790          AFTER ADVANCING PAGE.
0800       WRITE LIST-RECORD FROM DIRECTORY-HEADING
0810          AFTER ADVANCING 2 LINES
0820       MOVE SPACES TO LIST-RECORD.
0830       WRITE LIST-RECORD
0840          AFTER ADVANCING 1 LINE.
0850       PERFORM VARYING ROW-NR FROM 1 BY 1
0860          UNTIL ROW-NR > 55
              MOVE SPACES TO DIRECTORY-DETAIL
0870          PERFORM 0140-FORMAT-ENTRY
0880             VARYING COL-NR FROM 1 BY 1
0890             UNTIL COL-NR > 3
0910          WRITE LIST-RECORD FROM DIRECTORY-DETAIL
0920             AFTER ADVANCING 1 LINE
0930       END-PERFORM.
0940
0950   0120-LOAD-TABLE.
0960       MOVE INPUT-NAME TO PHONE-NAME (COL-NR ROW-NR).
0970       MOVE INPUT-NUMBER TO PHONE-NUMBER (COL-NR ROW-NR).
0980       PERFORM 0600-READ-INPUT.
0990   0140-FORMAT-ENTRY.
          IF PHONE-NAME (COL-NR ROW-NR) NOT = SPACES
1000          MOVE ALL ' .' TO DET-NAME (COL-NR)
1010          STRING PHONE-NAME (COL-NR ROW-NR) DELIMITED BY '  '
1020            INTO DET-NAME (COL-NR)
1030          MOVE PHONE-NUMBER (COL-NR ROW-NR) TO DET-NUMBER (COL-NR).
1040
1050   0600-READ-INPUT.
1060      READ PERSONNEL-FILE
1070         AT END
1080            MOVE 'YES' TO INPUT-EOF-STATUS.

