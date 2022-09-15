       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Arith.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TOTAL-SUPPLY-VALUE           PIC 9(6)V99 VALUE ZERO.
       01  TOTAL-ORDER-COST             PIC 9(6)V99 VALUE ZERO.
       01  WORK-COST                    PIC 9(5)V99.
       01  WORK-COUNT                   PIC 999.
       01  ORDER-RATIO                  PIC 99V99.
       01  ERROR-FLAG                   PIC X VALUE SPACE.
       01  SUPPLY-INFO.
           05  ID-NUMBER                PIC X(6).
           05  COUNT-ON-HAND            PIC 999.
           05  COST-EACH                PIC 999V99.
           05  LAST-REQUESTED-DATE      PIC X(8).
           05  LAST-REQUESTED-COUNT     PIC 999.

       01  SUGGESTED-ORDER.
           05  ORD-ID-NUMBER            PIC XXXXBXX.
           05  FILLER                   PIC XX VALUE SPACES.
           05  ORD-ON-HAND              PIC 999.
           05  FILLER                   PIC XX VALUE SPACES.
           05  ORD-LAST-REQUESTED-DATE  PIC XXXX/XX/XX.
           05  FILLER                   PIC XX VALUE SPACES.
           05  ORD-QUANTITY             PIC 999.
           05  FILLER                   PIC XX VALUE SPACES.
           05  ORD-AMOUNT               PIC ZZ,ZZ9.99.
           05  ORD-FLAG                 PIC X VALUE SPACE.

       01  ITEM1 PIC X(25) VALUE 'A001010050019919980215002'.
       01  ITEM2 PIC X(25) VALUE 'B002010250002419970930010'.
       01  ITEM3 PIC X(25) VALUE 'B002040250259919980115010'.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'ID NUMB AVAIL  LAST USED ORDER      COST'.
           MOVE ITEM1 TO SUPPLY-INFO.
      *    ACCEPT SUPPLY-INFO.
           PERFORM 0100-PROCESS.
           MOVE ITEM2 TO SUPPLY-INFO.
      *    ACCEPT SUPPLY-INFO.
           PERFORM 0100-PROCESS.
           MOVE ITEM3 TO SUPPLY-INFO.
      *    ACCEPT SUPPLY-INFO.
           PERFORM 0100-PROCESS.
           DISPLAY ' '.
           DISPLAY 'CURRENT SUPPLY VALUE: ' TOTAL-SUPPLY-VALUE
                   ', SUGGESTED ORDER COST: ' TOTAL-ORDER-COST.
           IF ERROR-FLAG  NOT = SPACE
              DISPLAY ' * CALL PROGRAMMER'.
           STOP RUN.

       0100-PROCESS.
           MOVE SPACE TO ORD-FLAG.
           MOVE ID-NUMBER TO ORD-ID-NUMBER.
           MOVE COUNT-ON-HAND TO ORD-ON-HAND.
           MOVE LAST-REQUESTED-DATE TO ORD-LAST-REQUESTED-DATE.
           MOVE ZERO TO WORK-COST.
           MULTIPLY COUNT-ON-HAND BY COST-EACH GIVING WORK-COST
               ON SIZE ERROR
                  MOVE '*' TO ORD-FLAG
                              ERROR-FLAG.
           ADD WORK-COST TO TOTAL-SUPPLY-VALUE.
           MOVE ZERO TO ORDER-RATIO ORD-QUANTITY.
           ADD COUNT-ON-HAND LAST-REQUESTED-COUNT
              GIVING WORK-COUNT.
           IF LAST-REQUESTED-DATE > '19980101'
              DIVIDE WORK-COUNT INTO LAST-REQUESTED-COUNT
                  GIVING ORDER-RATIO.
           MULTIPLY ORDER-RATIO BY 2 GIVING ORDER-RATIO.
           IF ORDER-RATIO > .25
              MULTIPLY WORK-COUNT BY ORDER-RATIO
                 GIVING ORD-QUANTITY ROUNDED.
           MULTIPLY ORD-QUANTITY BY COST-EACH
              GIVING WORK-COST
                     ORD-AMOUNT.
           ADD WORK-COST TO TOTAL-ORDER-COST.
           DISPLAY SUGGESTED-ORDER.
