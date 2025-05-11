IDENTIFICATION DIVISION.
       PROGRAM-ID. FRAUDCHK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO 'FRAUD.INPUT.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO 'FRAUD.OUTPUT.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  INFILE.
       01  IN-RECORD              PIC X(80).

       FD  OUTFILE.
       01  OUT-RECORD             PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-TRANS-ID            PIC X(10).
       01  WS-AMOUNT-STRING       PIC X(15).
       01  WS-DATE                PIC X(8).
       01  WS-LOCATION            PIC X(20).

       01  WS-AMOUNT-NUMERIC      PIC 9(7)V99 COMP-3.

       01  WS-TEMP-AMOUNT         PIC X(15).

       01  EOF-FLAG               PIC X VALUE 'N'.
           88  END-OF-FILE        VALUE 'Y'.
           88  NOT-END-OF-FILE    VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           OPEN INPUT INFILE
                OUTPUT OUTFILE

           PERFORM UNTIL END-OF-FILE
               READ INFILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM CHECK-AND-WRITE
               END-READ
           END-PERFORM

           CLOSE INFILE OUTFILE
           STOP RUN.

       CHECK-AND-WRITE.
           IF IN-RECORD(1:3) = "TXN"
               MOVE IN-RECORD(1:10)   TO WS-TRANS-ID
               MOVE IN-RECORD(12:15)  TO WS-AMOUNT-STRING
               MOVE IN-RECORD(28:8)   TO WS-DATE
               MOVE IN-RECORD(37:20)  TO WS-LOCATION

               PERFORM CLEAN-AMOUNT

               IF WS-AMOUNT-NUMERIC > 100000.00
                   MOVE IN-RECORD TO OUT-RECORD
                   WRITE OUT-RECORD
               END-IF
           END-IF.

       CLEAN-AMOUNT.
           MOVE WS-AMOUNT-STRING TO WS-TEMP-AMOUNT
           UNSTRING WS-TEMP-AMOUNT DELIMITED BY ALL "."
               INTO WS-TEMP-AMOUNT
           END-UNSTRING
           MOVE FUNCTION NUMVAL(WS-TEMP-AMOUNT) TO WS-AMOUNT-NUMERIC.
