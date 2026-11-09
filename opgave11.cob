       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opgave11.
       
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                 SELECT INPUT_BANK ASSIGN TO "Banker.txt"
                 ORGANIZATION IS LINE SEQUENTIAL.

                 SELECT INPUT_TRANSAKTION ASSIGN
                  TO "TransaktionerMini.txt"
                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           
           FILE SECTION.
                  FD INPUT_BANK.
                  01 BANK.
                  COPY "data_copybooks/bank.cpy".

                  FD INPUT_TRANSAKTION.
                  01 TRANSAKTION.
                  COPY "data_copybooks/transaktion.cpy".

       
       WORKING-STORAGE SECTION.
               01 WS_OUTPUT_DISPLAY PIC X(100) VALUE SPACES.
       PROCEDURE DIVISION.
           Display "initial commit"

       STOP RUN.
