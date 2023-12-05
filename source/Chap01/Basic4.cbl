        IDENTIFICATION DIVISION.
        PROGRAM-ID.  Basic4.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  MY-NAME                PIC X(10) VALUE 'Jon'.
        77  MY-ID-NR               PIC 99999 VALUE 855.
        PROCEDURE DIVISION.
        0000-MAIN.
            DISPLAY 'Hello there,' ' my name is: ' MY-NAME
                ', and ' WITH NO ADVANCING.
            DISPLAY 'my ID number is: '  MY-ID-NR.
            STOP RUN.

