000100 IDENTIFICATION DIVISION.
000200*
000300 PROGRAM-ID.    GETNUM.
000400*
000500*        *******************************
000600*        *                             *
000700*        *     Judson D. McClendon     *
000800*        *     Sun Valley Systems      *
000900*        *     329 37th Court N.E.     *
001000*        *     Birmingham, AL 35215    *
001100*        *        205-853-8440         *
001200*        *                             *
001300*        *******************************
001400*
001500 ENVIRONMENT DIVISION.
001600*
001700 CONFIGURATION SECTION.
001800*
001900 INPUT-OUTPUT SECTION.
002000*
002100 FILE-CONTROL.
002200*
002300 I-O-CONTROL.
002400*
002500 DATA DIVISION.
002600*
002700 FILE SECTION.
002800*
002900*
003000 WORKING-STORAGE SECTION.
003100*
003200 77  WS-ESCAPE-FLAG              PIC  9(01)  VALUE 0.
003300 77  WS-ERR-MSG                  PIC  X(30)  VALUE SPACES.
003400 77  WS-HOLD-NBR                 PIC  X(25)  VALUE SPACES.
003500*
003600******************************************************************
003700*                                                                *
003800*            G E T   N U M B E R   W O R K   A R E A             *
003900*                                                                *
004000******************************************************************
004100*
004200 01  NUMBER-WORK-AREA.
004300     03  NW-NBR-ERROR-FLAG       PIC  9(01).
004400     03  NW-WORK-NBR.
004500         05  NW-WORK-CHAR        OCCURS 25 TIMES
004600                                 INDEXED BY NW-WX
004700                                            NW-WLIM.
004800             07  NW-WORK-DIGIT       PIC  9(01).
004900     03  NW-DEC-PLACES           PIC  9(02).
005000     03  NW-BLD-SIGN             PIC S9(01).
005100     03  NW-BLD-NBR              PIC  9(12)V9(06).
005200     03  NW-BLD-NBR-SPLIT        REDEFINES NW-BLD-NBR.
005300         05  NW-BLD-INTEGER          PIC  9(12).
005400         05  NW-BLD-DECIMAL          PIC V9(06).
005500             88  NW-RESULT-INTEGER               VALUE ZERO.
005600         05  NW-BLD-DEC-DIGITS       REDEFINES NW-BLD-DECIMAL.
005700             07  NW-BLD-DEC-DIGIT        OCCURS 6 TIMES
005800                                         INDEXED BY NW-BDX
005900                                                    NW-BDLIM
006000                                             PIC  9(01).
006100     03  NW-EXTRACTED-NBR        PIC S9(12)V9(06).
006200*
006300 SCREEN SECTION.
006400*
006500*
006600*                     I N P U T   S C R E E N
006700*
006800 01  INPUT-SCREEN
006900         FOREGROUND-COLOR 7
007000         BACKGROUND-COLOR 0
007100         BLANK SCREEN.
007200     03  LINE 01  COLUMN 31  VALUE "G E T   N U M B E R".
007300*
007400     03  LINE 03  COLUMN 01  VALUE "Enter Number: ".
007500     03  PIC  X(25)          USING WS-HOLD-NBR.
007600*
007700*
007800*                    O U T P U T   S C R E E N
007900*
008000 01  OUTPUT-SCREEN
008100         FOREGROUND-COLOR 7
008200         BACKGROUND-COLOR 0.
008300*
008400     03  LINE 05  COLUMN 01  VALUE "      Result: ".
008500     03  PIC  -(13).9(06)    FROM  NW-EXTRACTED-NBR.
008600     03           COLUMN 41
008700         PIC  X(30)          FROM  WS-ERR-MSG.
008800/
008900 PROCEDURE DIVISION.
009000*
009100*
009200*             C O N T R O L   S E C T I O N
009300*
009400 000000-CONTROL.
009500*
009600     DISPLAY INPUT-SCREEN.
009700*
009800     PERFORM 000100-PROCESS
009900         UNTIL (WS-ESCAPE-FLAG = 1).
010000*
010100     STOP RUN.
010200*
010300*
010400*                    P R O C E S S
010500*
010600 000100-PROCESS.
010700*
010800     ACCEPT INPUT-SCREEN
010900         ON ESCAPE
011000             MOVE 1 TO WS-ESCAPE-FLAG
011100             EXIT PARAGRAPH.
011200*
011300     MOVE WS-HOLD-NBR TO NW-WORK-NBR.
011400     PERFORM 001000-GET-NBR.
011500*
011600     IF (NW-NBR-ERROR-FLAG = 1)
011700         MOVE "NUMBER INVALID" TO WS-ERR-MSG
011800     ELSE
011900         MOVE SPACES           TO WS-ERR-MSG.
012000     DISPLAY OUTPUT-SCREEN.
012100*
012200******************************************************************
012300*                                                                *
012400*                      G E T   N U M B E R                       *
012500*                                                                *
012600*    CONVERTS A NUMBER IN FREE FORMAT DISPLAY FORM:              *
012700*        FOR EXAMPLE:                                            *
012800*                                                                *
012900*            "999,999,999,999.999999 "                           *
013000*            "-999,999,999,999.999999"                           *
013100*            "              -23.61   "                           *
013200*            "                      4"                           *
013300*            "0                      "                           *
013400*            "    .000001            "                           *
013500*            "0000000000123456789.10-"                           *
013600*            "                       "  BLANK IS VALID = 0       *
013700*                                                                *
013800*    INTO FIXED NUMERIC FORM:                                    *
013900*                                                                *
014000*        PIC S9(12)V9(06)                                        *
014100*                                                                *
014200*                                                                *
014300*    USAGE:  MOVE <FREE FORM NUMBER> TO NW-WORK-NBR.             *
014400*            PERFORM 001000-GET-NBR                              *
014500*               THRU 001000-EXIT.                                *
014600*                                                                *
014700*    RESULT: NW-NBR-ERROR-FLAG = 0 INPUT IS A VALID NUMBER       *
014800*                                1 INPUT NOT A VALID NUMBER      *
014900*                                                                *
015000*       IF NW-NBR-ERROR-FLAG = 0 THEN:                           *
015100*                                                                *
015200*            NW-EXTRACTED-NBR  = NUMBER AS:  PIC S9(12)V9(06)    *
015300*                                                                *
015400*            NW-DEC-PLACES     = NUMBER OF DIGITS TO THE RIGHT   *
015500*                                OF THE DECIMAL POINT (0=NONE)   *
015600*                                                                *
015700*            NW-BLD-SIGN       = +1 OR -1 AS:  PIC S9(01)        *
015800*                                                                *
015900*            NW-BLD-INTEGER    = INTEGER DIGITS AS:  PIC  9(12)  *
016000*                                                                *
016100*            NW-BLD-DECIMAL    = DECIMAL DIGITS AS:  PIC V9(06)  *
016200*                                                                *
016300******************************************************************
016400*
016500 001000-GET-NBR.
016600*
016700     MOVE 0      TO NW-NBR-ERROR-FLAG.
016800     MOVE ZERO   TO NW-EXTRACTED-NBR.
016900     MOVE 0      TO NW-DEC-PLACES.
017000     MOVE ZERO   TO NW-BLD-NBR.
017100     MOVE +1     TO NW-BLD-SIGN.
017200     SET NW-BDX  TO 1.
017300     SET NW-WLIM TO 25.
017400*
017500*  ** LOCATE LEFTMOST DIGIT OF NUMBER **
017600*
017700     SET NW-WX  TO 1.
017800     SEARCH NW-WORK-CHAR
017900         WHEN NW-WORK-CHAR(NW-WX) NOT = SPACE
018000             PERFORM 001010-DECODE-NBR
018100     END-SEARCH.
018200*
018300     IF (NW-WORK-NBR NOT = SPACES)
018400         MOVE 1 TO NW-NBR-ERROR-FLAG
018500     ELSE
018600         COMPUTE NW-EXTRACTED-NBR = NW-BLD-NBR * NW-BLD-SIGN
018700     END-IF.
018800*
018900*
019000*              D E C O D E   N U M B E R
019100*
019200 001010-DECODE-NBR.
019300*
019400     IF (NW-WORK-CHAR(NW-WX) = "-")
019500         MOVE -1    TO NW-BLD-SIGN
019600         MOVE SPACE TO NW-WORK-CHAR(NW-WX)
019700         SET NW-WX UP BY 1
019800     END-IF.
019900*
020000     PERFORM 001020-GET-INTEGER-PART
020100         UNTIL (NW-WX > NW-WLIM).
020200*
020300     SET NW-DEC-PLACES TO NW-BDX.
020400     SUBTRACT 1 FROM NW-DEC-PLACES.
020500*
020600*
020700*     G E T   I N T E G E R   P A R T   O F   N U M B E R
020800*
020900 001020-GET-INTEGER-PART.
021000*
021100     IF (NW-WORK-CHAR(NW-WX) NUMERIC)
021200         IF (NW-BLD-INTEGER > 99999999999)
021300             SET NW-WX TO NW-WLIM
021400         ELSE
021500             COMPUTE NW-BLD-INTEGER =
021600                 NW-BLD-INTEGER * 10 + NW-WORK-DIGIT(NW-WX)
021700             MOVE SPACE TO NW-WORK-CHAR(NW-WX)
021800         END-IF
021900     ELSE
022000         IF (NW-WORK-CHAR(NW-WX) = ".")
022100             MOVE SPACES TO NW-WORK-CHAR(NW-WX)
022200             SET NW-WX UP BY 1
022300             PERFORM 001030-GET-DECIMAL-PART
022400                 UNTIL (NW-WX > NW-WLIM)
022500         ELSE
022600             IF (NW-WORK-CHAR(NW-WX) = ",")
022700                 MOVE SPACE TO NW-WORK-CHAR(NW-WX)
022800             ELSE
022900                 SET NW-WX TO NW-WLIM
023000             END-IF
023100         END-IF
023200     END-IF.
023300*
023400     SET NW-WX UP BY 1.
023500*
023600*
023700*    G E T   D E C I M A L   P A R T   O F   N U M B E R
023800*
023900 001030-GET-DECIMAL-PART.
024000*
024100     IF (NW-WORK-CHAR(NW-WX) NUMERIC)
024200         IF (NW-BDX > 6)
024300             SET NW-WX  TO NW-WLIM
024400         ELSE
024500             MOVE NW-WORK-DIGIT(NW-WX) TO NW-BLD-DEC-DIGIT(NW-BDX)
024600             MOVE SPACES TO NW-WORK-CHAR(NW-WX)
024700             SET NW-BDX UP BY 1
024800         END-IF
024900     ELSE
025000         IF (NW-WORK-CHAR(NW-WX) = "-")
025100             MOVE -1    TO NW-BLD-SIGN
025200             MOVE SPACE TO NW-WORK-CHAR(NW-WX)
025300             SET NW-WX  TO NW-WLIM
025400         ELSE
025500             SET NW-WX  TO NW-WLIM
025600         END-IF
025700     END-IF.
025800*
025900     SET NW-WX UP BY 1.
