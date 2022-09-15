        IDENTIFICATION DIVISION.
        PROGRAM-ID.  Dates2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  THIS-DATE.
            05  THIS-YEAR             PIC 99.
            05  THIS-MONTH            PIC XX.
            05  THIS-DAY              PIC XX.
        01  FORMAT-MONTH-AND-YEAR.                              
            05  FORMAT-MAY-MONTH.                               
                10  FORMAT-MAY-MONTH-NUM PIC 99.                
            05  FILLER                PIC X VALUE '/'.          
            05  FORMAT-4DIG-YEAR-AN.                            
                10  FORMAT-MAY-CENT   PIC XX VALUE '20'.        
                10  FORMAT-MAY-YEAR   PIC 99.                   
            05  FORMAT-4DIG-YEAR REDEFINES FORMAT-4DIG-YEAR-AN  
                                      PIC 9999.                 

        01  THIS-TIME.
            05  THIS-HOUR             PIC 99.
            05  THIS-MINUTE           PIC XX.
            05  THIS-SECOND           PIC XX.
            05  THIS-SEC-FRACT        PIC XX.
        01  FORMAT-TIME.
            05  FORMAT-HOUR           PIC 99.
            05  FILLER                PIC X VALUE ':'.
            05  FORMAT-MINUTE         PIC XX.
            05                        PIC X VALUE ':'.
            05  FORMAT-SECOND         PIC XX.
            05  FILLER                PIC X VALUE '.'.
            05  FORMAT-SEC-FRACT      PIC XX.
            05  FORMAT-AM-PM          PIC XXX VALUE ' AM'.

        01  DAY-OF-YEAR-INFO.
            05  THIS-DAY-OF-YEAR.
                10  THIS-DOY-YEAR     PIC 99.
                10  THIS-DOY-DAY      PIC XXX.
            05  FORMAT-DAY-OF-YEAR.
                10  FORMAT-CENTURY    PIC XX VALUE '20'.
                10  FORMAT-DOY-YEAR   PIC 99.
                10  FILLER            PIC X VALUE '.'.
                10  FORMAT-DOY-DAY    PIC XXX.

        PROCEDURE DIVISION.
        0000-MAIN.
            ACCEPT THIS-DATE        FROM DATE.
            ACCEPT THIS-TIME        FROM TIME.
            ACCEPT THIS-DAY-OF-YEAR FROM DAY.

            MOVE THIS-MONTH         TO FORMAT-MAY-MONTH.        
            MOVE THIS-YEAR          TO FORMAT-MAY-YEAR.        
            IF FORMAT-MAY-YEAR > 90                             
               MOVE '19'            TO FORMAT-MAY-CENT.         

            MOVE THIS-HOUR          TO FORMAT-HOUR.
            MOVE THIS-MINUTE        TO FORMAT-MINUTE.
            MOVE THIS-SECOND        TO FORMAT-SECOND.
            MOVE THIS-SEC-FRACT     TO FORMAT-SEC-FRACT.
            IF FORMAT-HOUR > 11
                MOVE ' PM'          TO FORMAT-AM-PM.
            IF FORMAT-HOUR = ZERO
                MOVE 12             TO FORMAT-HOUR.
            IF FORMAT-HOUR > 12
                SUBTRACT 12         FROM FORMAT-HOUR.

            MOVE THIS-DOY-YEAR      TO FORMAT-DOY-YEAR.
            MOVE THIS-DOY-DAY       TO FORMAT-DOY-DAY.
            IF FORMAT-DOY-YEAR > 90
                MOVE  '19'          TO FORMAT-CENTURY.

            DISPLAY 'TODAY IS ' THIS-MONTH '/' THIS-DAY '/'
                FORMAT-4DIG-YEAR-AN ' ' FORMAT-TIME.            
            DISPLAY '  (THE DAY OF YEAR IS: '
                FORMAT-DAY-OF-YEAR ')'.

            ADD 1                    TO FORMAT-MAY-MONTH-NUM.   
            IF FORMAT-MAY-MONTH-NUM > 12
                MOVE 1               TO FORMAT-MAY-MONTH-NUM    
                ADD 1                TO FORMAT-4DIG-YEAR.      

            DISPLAY '   NEXT MONTH IS: ' FORMAT-MONTH-AND-YEAR. 
            STOP RUN.
