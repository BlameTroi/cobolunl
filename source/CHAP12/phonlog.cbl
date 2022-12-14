0010   IDENTIFICATION DIVISION.
       PROGRAM-ID. Phonelog.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PHONE-LOG-FILE         ASSIGN TO "PHONLOGD.DAT"
                                         LINE SEQUENTIAL.
           SELECT SOLICITOR-SUMMARY-FILE ASSIGN TO "SOLICSUM.DAT".

       DATA DIVISION.
0020   FILE SECTION.
0030   FD  PHONE-LOG-FILE.
0040   01  PHONE-LOG-RECORD.
0050       05  PH-USER-ID             PIC X(8).
0060       05  PH-CALL-DATE.
0070           10  PH-CALL-YR         PIC 9999.
0080           10  PH-CALL-MO         PIC 99.
0090           10  PH-CALL-DA         PIC 99.
0100       05  PH-CALL-LENGTH         PIC 9999.
0110       05  PH-CALL-NUMBER         PIC 9(10).
0120       05  PH-SALES-INFO          PIC X(50).
0130
0140   FD  SOLICITOR-SUMMARY-FILE.
0150   01  SOLICITOR-RECORD.
0160       05  SOL-USER-ID            PIC X(8).
0170       05  SOL-DAYS-ACTIVITY OCCURS 31 TIMES.
0180           10  SOL-CALL-CT        PIC 9(4).
0190           10  SOL-CALL-TIME      PIC 9(5).
0200
0210   WORKING-STORAGE SECTION.
0220   01  INPUT-EOF-STATUS           PIC XXX VALUE 'NO '.
0230       88  END-OF-INPUT           VALUE 'YES'.
0240   01  NR-USERS                   PIC 999 COMP VALUE ZERO.
0245   01  DAY-SUB                    PIC 99  COMP VALUE ZERO.
0250   01  USER-ID-TABLE.
0260       05  USER-ID-ENTRY OCCURS   0 TO 200 TIMES
               DEPENDING ON NR-USERS
0275           ASCENDING KEY USER-ID
0280           INDEXED BY USR-IX USR-IX2.
0290           10  USER-ID            PIC X(8).
0300           10  USER-SUM-NR        PIC 999 COMP.
0310   01  USER-SUMMARY-TALBLE.
0320       05  SUMMARY-ENTRY OCCURS 200 TIMES
0330           INDEXED BY SUM-IX.
0340           10  SUMMARY-DAY OCCURS 31 TIMES.
0350               15  SUM-COUNT      PIC 9(4).
0360               15  SUM-TIME       PIC 9(5).
0370
0380   PROCEDURE DIVISION.
0390   0100-MAIN-CONTROL.
0400       OPEN INPUT PHONE-LOG-FILE.
0410       PERFORM 0500-READ-PHONE-LOG.
0420       PERFORM 0200-INPUT-LOOP
0430          UNTIL END-OF-INPUT.
0440       CLOSE PHONE-LOG-FILE.
0450       OPEN OUTPUT SOLICITOR-SUMMARY-FILE.
0460       PERFORM 0300-WRITE-SUMMARY-RECORDS
0470          VARYING USR-IX FROM 1 BY 1
0480          UNTIL USR-IX > NR-USERS.
0490       CLOSE SOLICITOR-SUMMARY-FILE.
0500       STOP RUN.
0510
0520   0200-INPUT-LOOP.
0530       SEARCH ALL USER-ID-ENTRY
0540          AT END
0550             PERFORM 0250-ADD-USER
0560          WHEN USER-ID (USR-IX) = PH-USER-ID
0570             SET SUM-IX TO USER-SUM-NR (USR-IX).
0580       ADD 1 TO SUM-COUNT (SUM-IX PH-CALL-DA).
0590       ADD PH-CALL-LENGTH TO SUM-TIME (SUM-IX PH-CALL-DA).
0600       PERFORM 0500-READ-PHONE-LOG.
0610
0620   0250-ADD-USER.
0630       PERFORM
0640          VARYING USR-IX FROM 1 BY 1
0650          UNTIL USR-IX > NR-USERS
0660              OR PH-USER-ID < USER-ID (USR-IX)
0670             CONTINUE.
0680       PERFORM
0690        VARYING USR-IX2 FROM NR-USERS BY -1
0700        UNTIL USR-IX2 < USR-IX
0710          MOVE USER-ID (USR-IX2) TO USER-ID (USR-IX2 + 1)
0720          MOVE USER-SUM-NR (USR-IX2) TO USER-SUM-NR (USR-IX2 + 1).
0730       ADD 1 TO NR-USERS.
0740       MOVE PH-USER-ID TO USER-ID (USR-IX).
0750       MOVE NR-USERS TO USER-SUM-NR (USR-IX).
0760       SET SUM-IX TO USER-SUM-NR (USR-IX).
0770       INITIALIZE SUMMARY-ENTRY (SUM-IX).
0780
0790   0300-WRITE-SUMMARY-RECORDS.
0800       MOVE USER-ID (USR-IX) TO SOL-USER-ID.
0810       SET SUM-IX TO USER-SUM-NR (USR-IX).
0820       PERFORM
0830          VARYING DAY-SUB FROM 1 BY 1
0840             UNTIL DAY-SUB > 31
0850         MOVE SUM-COUNT (SUM-IX DAY-SUB) TO SOL-CALL-CT (DAY-SUB)
             IF SOL-CALL-CT (DAY-SUB) > ZERO
                 DISPLAY SOL-USER-ID ", DAY " DAY-SUB ": "
                     SOL-CALL-CT (DAY-SUB)
             END-IF
0860         MOVE SUM-TIME (SUM-IX DAY-SUB) TO SOL-CALL-TIME (DAY-SUB).
0865        
0870       WRITE SOLICITOR-RECORD.
0880
0890   0500-READ-PHONE-LOG.
0900       READ PHONE-LOG-FILE
0910           AT END
0920              MOVE 'YES' TO INPUT-EOF-STATUS.

