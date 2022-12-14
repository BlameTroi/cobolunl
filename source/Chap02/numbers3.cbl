        IDENTIFICATION DIVISION.
        PROGRAM-ID.  Numbers3.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  UNSIGN-9-5AN.
            05  UNSIGNED-9-5         PIC 9(5) VALUE ZERO.
        01  UNSIGN-9-5V99AN.
            05  UNSIGNED-9-5V99      PIC 9(5)V99 VALUE ZERO.
        01  NEGATIVE-9-5V99AN.
            05  NEGATIVE-9-5V99      PIC S9(5)V99 VALUE ZERO.
        01  LEAD-SEP-NEG-9-5V99AN.
            05  LEAD-SEP-NEG-9-5V99  PIC S9(5)V99
                            SIGN LEADING SEPARATE VALUE ZERO.
        01  TRAIL-SEP-NEG-9-5V99AN.
            05  TRAIL-SEP-NEG-9-5V99 PIC S9(5)V99
                            SIGN TRAILING SEPARATE VALUE ZERO.
        PROCEDURE DIVISION.
        0000-MAIN.
            DISPLAY 'ENTER 5 NUMERIC DIGITS: '.
            ACCEPT UNSIGNED-9-5.
            DISPLAY 'UNSIGN 9(5) A/N: ' UNSIGN-9-5AN
                     ', NUM: ' UNSIGNED-9-5.
            DISPLAY 'ENTER 9(5)V99 AS 7 NUMERIC DIGITS: '.
            ACCEPT UNSIGNED-9-5V99.
            DISPLAY 'UNSIGN 9(5)V99 A/N: ' UNSIGN-9-5V99AN
                    ', NUM: ' UNSIGNED-9-5V99.
            DISPLAY 'ENTER S9(5)V99 AS 7 DIGITS AND AN OVERPUNCHED SIGN' 
                    ' IF DESIRED: '.  
            ACCEPT NEGATIVE-9-5V99.
            DISPLAY 'NEGATIVE 9(5)V99 A/N: ' NEGATIVE-9-5V99AN
                    ', NUM: ' NEGATIVE-9-5V99.
            DISPLAY 'ENTER S9(5)V99 AS LEADING +/- AND 7DIGITS: '.
            ACCEPT LEAD-SEP-NEG-9-5V99.
            DISPLAY 'LEAD/SEP/NEG 9(5)V99 A/N: ' LEAD-SEP-NEG-9-5V99AN
                    ', NUM: ' LEAD-SEP-NEG-9-5V99.
            DISPLAY 'ENTER S9(5)V99 AS 7 DIGITS AND TRAILING +/-: '.
            ACCEPT TRAIL-SEP-NEG-9-5V99.
            DISPLAY 'TRAIL/SEP/NEG 9(5)V99 A/N: ' TRAIL-SEP-NEG-9-5V99AN
                    ', NUM: ' TRAIL-SEP-NEG-9-5V99.
            STOP RUN.
