        IDENTIFICATION DIVISION.
        PROGRAM-ID.  Numbers2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  UNSIGN-9-5AN.
            05  UNSIGNED-9-5         PIC 9(5) VALUE 123.
        01  UNSIGN-9-5V99AN.
            05  UNSIGNED-9-5V99      PIC 9(5)V99 VALUE 123.4.
        01  NEGATIVE-9-5V99AN.
            05  NEGATIVE-9-5V99      PIC S9(5)V99 VALUE -123.4.
        01  LEAD-SEP-NEG-9-5V99AN.
            05  LEAD-SEP-NEG-9-5V99  PIC S9(5)V99
                            SIGN LEADING SEPARATE VALUE -123.4.
        01  TRAIL-SEP-NEG-9-5V99AN.
            05  TRAIL-SEP-NEG-9-5V99 PIC S9(5)V99
                            SIGN TRAILING SEPARATE VALUE -123.4.
        PROCEDURE DIVISION.
        0000-MAIN.
            DISPLAY 'UNSIGN 9(5) A/N: ' UNSIGN-9-5AN
                    ', NUM: ' UNSIGNED-9-5.
            DISPLAY 'UNSIGN 9(5)V99 A/N: ' UNSIGN-9-5V99AN
                    ', NUM: ' UNSIGNED-9-5V99.
            DISPLAY 'NEGATIVE 9(5)V99 A/N: ' NEGATIVE-9-5V99AN
                    ', NUM: ' NEGATIVE-9-5V99.
            DISPLAY 'LEAD/SEP/NEG 9(5)V99 A/N: ' LEAD-SEP-NEG-9-5V99AN
                    ', NUM: ' LEAD-SEP-NEG-9-5V99.
            DISPLAY 'TRAIL/SEP/NEG 9(5)V99 A/N: ' TRAIL-SEP-NEG-9-5V99AN
                    ', NUM: ' TRAIL-SEP-NEG-9-5V99.
            STOP RUN.
