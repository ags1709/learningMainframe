       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMMISSIONREPORT.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALESFILE ASSIGN TO "SALES.DAT" 
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRINT-FILE ASSIGN TO "SALESREPORT.DAT"

       DATA DIVISION.
       FILE SECTION.
       FD  SALESFILE.

       01 SALESDETAILS.
          88 ENDOFSALES                    VALUE HIGH-VALUES.
       05 SALESPERSON-ID        PIC 9(5).
          05 SALESPERSON-NAME.
             10 LASTNAME        PIC X(20).
             10 FIRSTNAME       PIC X(20).
          05 REGION             PIC X(5).
          05 YEARLYSALES        PIC 9(6).
          05 GENDER             PIC X.

       FD  PRINT-FILE.

       01 PRINT-LINE            PIC X(132).

       WORKING-STORAGE SECTION.
       01 WS-FIELDS.
          05 WS-TOTAL-SALES     PIC 9(10) COMP-3
                                           VALUE ZEROES.
          05 WS-COMMISION-RATE  PIC V99    VALUE .03.

       PROCEDURE DIVISION.