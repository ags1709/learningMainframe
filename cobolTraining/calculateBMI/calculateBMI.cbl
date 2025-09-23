       IDENTIFICATION DIVISION.
       PROGRAM-ID. calculateBMI.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WEIGHT PIC 999.
       01 HEIGHT PIC 999.
       01 BMI PIC 999V99.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Please enter your weight in kg: ".
           ACCEPT WEIGHT.
           DISPLAY "Please enter your height in cm: ".
           ACCEPT HEIGHT.
           COMPUTE BMI = WEIGHT / (HEIGHT / 100)**2.
           DISPLAY "Your BMI is: ", BMI.

           STOP RUN.
       END PROGRAM calculateBMI.


