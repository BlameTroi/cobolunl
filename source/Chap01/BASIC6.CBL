        IDENTIFICATION DIVISION.
        PROGRAM-ID.  Basic6.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  MY-NAME                PIC X(10) VALUE SPACES.
        77  MY-ID-NR               PIC 99999 VALUE ZERO.
        PROCEDURE DIVISION.
        0000-MAIN.
            DISPLAY 'Enter Your Name Please: '.
            ACCEPT MY-NAME.
            DISPLAY 'Enter Your ID Number Please: '.
            ACCEPT MY-ID-NR.
            DISPLAY 'Hello there,' ' my name is: ' MY-NAME
               ', and ' WITH NO ADVANCING.
            DISPLAY 'my ID number is: '  MY-ID-NR.
            STOP RUN.
