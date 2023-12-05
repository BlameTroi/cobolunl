       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MAINPRG2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INPUT-STATE-CODE              PIC XX.
       01  INPUT-LOCATION                PIC X(20).
       01  CONTINUE-PROCESSING           PIC XXX VALUE SPACES.
       01  READ-FUNCTION                 PIC X VALUE 'R'.               (1)
       01  CLOSE-FUNCTION                PIC X VALUE 'C'.               (2)
       01  RETRN-CODE                    PIC XX VALUE ZERO.             (3)
       01  AGENT-INFO.                                                  (4)
           05  AGENT-STATE               PIC XX.
           05  AGENT-TERRITORY           PIC X(20).
           05  AGENT-NAME                PIC X(20).
           05  AGENT-PHONE-NR            PIC X(13).
           05  AGENT-ADDRESS             PIC X(25).

       PROCEDURE DIVISION.
       0100-MAIN.
           DISPLAY 'WELCOME TO THE ACME AGENT LOCATION SYSTEM'.
           PERFORM 0200-MAIN-LOOP THRU 0200-EXIT
               UNTIL CONTINUE-PROCESSING (1:1) = 'N' OR 'n'.
           CALL 'SUBPRG2'
                USING CLOSE-FUNCTION RETRN-CODE AGENT-INFO.             (5)
           STOP RUN.
       0200-MAIN-LOOP.
           DISPLAY 'PLEASE ENTER THE STATE YOU ARE INTERESTED IN'.
           ACCEPT INPUT-STATE-CODE.
           DISPLAY 'PLEASE ENTER THE TOWNSHIP, CITY, TOWN OR COUNTY'
           ACCEPT INPUT-LOCATION.
           MOVE INPUT-STATE-CODE TO AGENT-STATE.
           MOVE INPUT-LOCATION TO AGENT-TERRITORY.
           CALL 'SUBPRG2'
                USING READ-FUNCTION RETRN-CODE AGENT-INFO.              (6)
           IF RETRN-CODE = '01'
                DISPLAY 'WE ARE UNABLE TO LOCATE AN AGENT FOR THAT AREA'
           ELSE
           IF RETRN-CODE = '00'
                DISPLAY 'NAME: ' AGENT-NAME
                DISPLAY 'PHONE NR: ' AGENT-PHONE-NR
                DISPLAY 'ADDRESS: ' AGENT-ADDRESS
           ELSE
                MOVE 'N' TO CONTINUE-PROCESSING
                GO TO 0200-EXIT.
           DISPLAY 'WOULD LIKE TO LOOK UP ANOTHER AGENT? (Y/N)'.
           ACCEPT CONTINUE-PROCESSING.
       0200-EXIT.
           EXIT.
