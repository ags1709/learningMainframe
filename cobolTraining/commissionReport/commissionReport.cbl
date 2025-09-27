       IDENTIFICATION DIVISION.
       PROGRAM-ID. commissionReport.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALESFILE ASSIGN TO "SALES.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRINTFILE ASSIGN TO "COMMISSIONREPORT.DAT".

       DATA DIVISION.
       FILE SECTION.
       FD  SALESFILE.

       01 SALESDETAILS.
           88 ENDOFSALES                    VALUE HIGH-VALUES.
           05 SALESPERSON-ID        PIC 9(5).
           05 SALESPERSON-NAME.
              10 LASTNAME           PIC X(20).
              10 FIRSTNAME          PIC X(20).
           05 REGION                PIC X(5).
           05 YEARLYSALES           PIC 9(6).
           05 GENDER                PIC X.

       FD  PRINTFILE.
       01  PRINT-LINE PIC X(132).

       WORKING-STORAGE SECTION.
       01 WS-FIELDS.
           05 WS-COMMISSION-RATE    PIC V99 VALUE .05.
           05 WS-COMMISSION-AMT     PIC 9(8) COMP-3.
           05 WS-TOTAL-COMMISSION   PIC 9(10) COMP-3.

       01  HEADING-LINE1.
           05 FILLER                PIC X(2)        VALUE SPACES.
           05 FILLER                PIC X(5)        VALUE 'FIRST'.
           05 FILLER                PIC X(11)       VALUE SPACES.
           05 FILLER                PIC X(4)        VALUE 'LAST'.
           05 FILLER                PIC X(12)       VALUE SPACES.
           05 FILLER                PIC X(10)       VALUE 'COMMISSION'.
           05 FILLER                PIC X(10)       VALUE SPACES.
           05 FILLER                PIC X(10)       VALUE 'COMMISSION'.
           05 FILLER                PIC X(66)       VALUE SPACES.

       01  HEADING-LINE2.
           05 FILLER                PIC X(2)        VALUE SPACES.
           05 FILLER                PIC X(4)        VALUE 'NAME'.
           05 FILLER                PIC X(12)       VALUE SPACES.
           05 FILLER                PIC X(4)        VALUE 'NAME'.
           05 FILLER                PIC X(12)       VALUE SPACES.
           05 FILLER                PIC X(4)        VALUE 'RATE'.
           05 FILLER                PIC X(16)       VALUE SPACES.
           05 FILLER                PIC X(6)        VALUE 'AMOUNT'.
           05 FILLER                PIC X(66)       VALUE SPACES.

       01  HEADING-LINE3.
           05 FILLER                PIC X(2)        VALUE SPACES.
           05 FILLER                PIC X(10)       VALUE '----------'.
           05 FILLER                PIC X(6)        VALUE SPACES.
           05 FILLER                PIC X(10)       VALUE '----------'.
           05 FILLER                PIC X(6)        VALUE SPACES.
           05 FILLER                PIC X(10)       VALUE '----------'.
           05 FILLER                PIC X(10)       VALUE SPACES.
           05 FILLER                PIC X(10)       VALUE '----------'.
           05 FILLER                PIC X(66)       VALUE SPACES.

       01  DETAIL-LINE.
           05 FILLER                PIC X(2)        VALUE SPACES.
           05 COMM-FIRSTNAME        PIC X(15).
           05 FILLER                PIC X(1)        VALUE SPACES.
           05 COMM-LASTNAME         PIC X(15).
           05 FILLER                PIC X(3)        VALUE SPACES.
           05 COMM-RATE             PIC .99.
           05 FILLER                PIC X           VALUE '%'.
           05 FILLER                PIC X(13)       VALUE SPACES.
           05 COMM-AMOUNT           PIC $$,$$$,$$$.
       
       01 TOTAL-COMM-LINE.
           05 FILLER                PIC X(32)       VALUE SPACES.
           05 FILLER                PIC X(20)       VALUE
                 "Total Commissions: ".
           05 TOTAL-COMMISSION      PIC $$$,$$$,$$$.
           05 FILLER                PIC X(73)       VALUE SPACES.

       PROCEDURE DIVISION.

       0050-CREATE-COMMISSION-REPORT.
           OPEN INPUT SALESFILE.
           OPEN OUTPUT PRINTFILE.
           PERFORM 0100-PROCESS-RECORDS.
           PERFORM 0200-STOP-RUN.

        0100-PROCESS-RECORDS.
           PERFORM 0110-WRITE-HEADING-LINE.

           READ SALESFILE
              AT END
                 SET ENDOFSALES TO TRUE
           END-READ.

           PERFORM UNTIL ENDOFSALES
      *       *> Skip blank lines / empty records
      *       (extra newline at the end)
              IF NOT (FIRSTNAME = SPACES AND LASTNAME = SPACES AND
               YEARLYSALES = ZERO)
                 MOVE SPACES TO DETAIL-LINE
                 COMPUTE WS-COMMISSION-AMT = WS-COMMISSION-RATE * 
                 YEARLYSALES
                 ADD WS-COMMISSION-AMT TO WS-TOTAL-COMMISSION
                 MOVE FIRSTNAME           TO COMM-FIRSTNAME
                 MOVE LASTNAME            TO COMM-LASTNAME
                 MOVE WS-COMMISSION-RATE  TO COMM-RATE
                 MOVE WS-COMMISSION-AMT   TO COMM-AMOUNT
                 PERFORM 0120-WRITE-DETAIL-LINE
              END-IF

              READ SALESFILE
                 AT END
                    SET ENDOFSALES TO TRUE
              END-READ
           END-PERFORM.

           PERFORM 0130-WRITE-TOTAL-LINE.


       
       0110-WRITE-HEADING-LINE.
           MOVE HEADING-LINE1 TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
           MOVE HEADING-LINE2 TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
           MOVE HEADING-LINE3 TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

       0120-WRITE-DETAIL-LINE.
           MOVE DETAIL-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

       0130-WRITE-TOTAL-LINE.
           MOVE WS-TOTAL-COMMISSION TO TOTAL-COMMISSION.
           MOVE TOTAL-COMM-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 2 LINES.

       0200-STOP-RUN.
           CLOSE SALESFILE.
           CLOSE PRINTFILE.
           STOP RUN.

       END PROGRAM commissionReport.

