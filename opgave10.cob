       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opgave10.
       
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                 SELECT INPUT_BANK ASSIGN TO "Banker.txt"
                 ORGANIZATION IS LINE SEQUENTIAL.

                 SELECT INPUT_TRANSAKTION ASSIGN
                  TO "TransaktionerMini.txt"
                 ORGANIZATION IS LINE SEQUENTIAL.

                 SELECT OUTPUT_KONTOUDSKRIFT
                  ASSIGN TO "Kontoudskrifter.txt"
                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           
           FILE SECTION.
                  FD INPUT_BANK.
                  01 BANK.
                  COPY "data_copybooks/bank.cpy".

                  FD INPUT_TRANSAKTION.
                  01 TRANSAKTION.
                  COPY "data_copybooks/transaktion.cpy".

                  FD OUTPUT_KONTOUDSKRIFT.
                  01 KONTOUDSKRIFT.
                  05 NY_LINJE PIC X(100)   VALUE SPACES.
       
       WORKING-STORAGE SECTION.
      * Account ID manipulation
           01 CURRENT_KONTO_ID PIC X(15) VALUE SPACES.
           01 PREVIOUS_KONTO_ID PIC X(15) VALUE SPACES.
      * Bank array specification.
              78 BANK_ARRAY_SIZE VALUE 100.
                  01 BANK_ARRAY OCCURS BANK_ARRAY_SIZE TIMES.
                   COPY "data_copybooks/bank.cpy".
      
      * End of File flags
      * Transaktioner.txt
                  01 EOF_TRANSAKTION
                  PIC X VALUE "N".
                  88 EOF_TRANSAKTION_REACHED VALUE "Y".
      * Banker.txt
                  01   EOF_BANK
                  PIC X VALUE "N".
                  88 EOF_BANK_REACHED VALUE "Y".
      
      * Arithmetic representation of balance:
                  01 BELØB_DECIMAL PIC S9(11)V99.
      * Output variables
                  01 VALUTA_DKK PIC X(3) VALUE "DKK".

      * Data manipulation variables.
                  01 IDX        PIC 9(3) VALUE ZEROES.
                  01 IDX_OUT    PIC 9(3) VALUE ZEROES.
                  01 IDX_BANK   PIC 9(3) VALUE ZEROS.
                  
                  01 CURRENT_CHAR PIC X(1) VALUE SPACES.
                  01 PREVIOUS_CHAR PIC X(1) VALUE SPACES.

                  01 KONTOUDSKRIFT_CLEAN PIC X(100) VALUE SPACES.

                  01 BELØB_DECIMAL_DKK PIC S9(11)V99 VALUE ZEROES.
                  01 BELØB_DKK_FORMAT PIC ZZZZZZZZZ9.99 VALUE SPACES.

       PROCEDURE DIVISION.
       OPEN INPUT INPUT_BANK.
           PERFORM READ_ALL_BANKS.
       CLOSE INPUT_BANK.
           
      * Begin loop of Transactions
       OPEN INPUT INPUT_TRANSAKTION.
       OPEN OUTPUT OUTPUT_KONTOUDSKRIFT.
       PERFORM UNTIL EOF_TRANSAKTION_REACHED
       READ INPUT_TRANSAKTION INTO TRANSAKTION
       AT END
       SET EOF_TRANSAKTION_REACHED TO TRUE
       NOT AT END
      * Update current account
       MOVE SPACES TO PREVIOUS_KONTO_ID
       MOVE CURRENT_KONTO_ID TO PREVIOUS_KONTO_ID
       MOVE SPACES TO CURRENT_KONTO_ID
       MOVE KONTO_ID OF TRANSAKTION TO CURRENT_KONTO_ID
      
      * DISPLAY PREVIOUS_KONTO_ID

      * Is this a new account?

       IF CURRENT_KONTO_ID EQUAL PREVIOUS_KONTO_ID
      * If same account
              PERFORM WRITE_TRANSAKTION
              PERFORM CONVERT_VALUTA





      * Same account as last transaction.
       ELSE
           PERFORM NEW_LINE
      *     Write top string
           PERFORM WRITE_FILE_TOP
      *     Write name and adress of customer.
           PERFORM WRITE_KUNDE_INFO
           PERFORM NEW_LINE
      *     Write bank info.
           PERFORM WRITE_BANK_INFO
           PERFORM NEW_LINE
      *     Write account nr.
           PERFORM WRITE_KONTO_ID
           PERFORM NEW_LINE
           PERFORM WRITE_TRANSAKTION_HEADER
    
       END-READ
       END-PERFORM.
       

      * Register
       
       CLOSE INPUT_TRANSAKTION.
       CLOSE OUTPUT_KONTOUDSKRIFT.

       
       STOP RUN.
       
       WRITE_BANK_INFO.
       MOVE 1 TO IDX_BANK.
       PERFORM VARYING IDX_BANK FROM 1 BY 1 
       UNTIL IDX_BANK > BANK_ARRAY_SIZE
       IF REG_NR OF TRANSAKTION EQUAL REG_NR OF BANK_ARRAY(IDX_BANK)
      * Registreringsnummer
       MOVE SPACES TO KONTOUDSKRIFT
       STRING "Registreringsnummer: " DELIMITED BY SIZE 
       REG_NR OF BANK_ARRAY(IDX_BANK) DELIMITED BY SIZE 
       INTO KONTOUDSKRIFT
       PERFORM CLEAN_WRITE

      * Bank-navn
       STRING "Bank: " DELIMITED BY SIZE 
       BANK_ADRESSE OF BANK_ARRAY(IDX_BANK) DELIMITED BY SIZE 
       INTO KONTOUDSKRIFT
       PERFORM CLEAN_WRITE
      * Bank-adresse
       STRING "Bankadresse: " DELIMITED BY SIZE 
       BANK_ADRESSE OF BANK_ARRAY(IDX_BANK) DELIMITED BY SIZE 
       INTO KONTOUDSKRIFT
       PERFORM CLEAN_WRITE
      * Telefon
       STRING "Telefon: " DELIMITED BY SIZE 
       TELEFON OF BANK_ARRAY(IDX_BANK) DELIMITED BY SIZE 
       INTO KONTOUDSKRIFT
       PERFORM CLEAN_WRITE
      * Email
       STRING "Registreringsnummer: " DELIMITED BY SIZE 
       EMAIL OF BANK_ARRAY(IDX_BANK) DELIMITED BY SIZE 
       INTO KONTOUDSKRIFT
       PERFORM CLEAN_WRITE
      * End-if
       END-IF
       END-PERFORM.
       MOVE 1 TO IDX_BANK.
       EXIT.



      * READ ALL BANKS into array of size 100.
       READ_ALL_BANKS.
      * Reset IDX (mainly for safety).
       MOVE 0 to IDX_BANK.
      * Loop banks into BANK_ARRAY
       PERFORM UNTIL EOF_BANK_REACHED
           READ INPUT_BANK INTO BANK
           AT END
               SET EOF_BANK_REACHED TO TRUE
           NOT AT END
              ADD 1 TO IDX_BANK
              MOVE BANK TO BANK_ARRAY(IDX_BANK)
              MOVE SPACES TO BANK
           END-READ
       END-PERFORM.
      * Reset IDX and EOF_BANK.
       MOVE 0 TO IDX_BANK.
       MOVE "N" TO EOF_BANK.
       EXIT.
      


       CLEAN_ROW.
       MOVE 1 TO IDX.
       MOVE 1 TO IDX_OUT.
       MOVE SPACES TO CURRENT_CHAR.
       MOVE SPACES TO PREVIOUS_CHAR.
       PERFORM VARYING IDX FROM 1 BY 1 UNTIL
       IDX > LENGTH OF KONTOUDSKRIFT
      * Update current char
       MOVE KONTOUDSKRIFT(IDX:1) TO CURRENT_CHAR
      * Evaluate current char and previous char
       IF CURRENT_CHAR NOT = SPACE OR PREVIOUS_CHAR NOT = SPACE
              MOVE CURRENT_CHAR TO KONTOUDSKRIFT_CLEAN(IDX_OUT:1)
       ADD 1 TO IDX_OUT
       END-IF
       MOVE CURRENT_CHAR TO PREVIOUS_CHAR
       END-PERFORM.
       MOVE SPACES TO KONTOUDSKRIFT
       MOVE KONTOUDSKRIFT_CLEAN TO KONTOUDSKRIFT
       MOVE SPACES TO KONTOUDSKRIFT_CLEAN
       EXIT.

       NEW_LINE.
       MOVE SPACES TO KONTOUDSKRIFT.
       WRITE KONTOUDSKRIFT.
       EXIT.

       WRITE_FILE_TOP.
       MOVE SPACES TO KONTOUDSKRIFT.
       MOVE "----------------------------------------------------------"
       TO NY_LINJE.
       WRITE KONTOUDSKRIFT.
       EXIT.

       WRITE_KUNDE_INFO.
       MOVE SPACES TO KONTOUDSKRIFT.
       STRING "Kunde: " DELIMITED BY SIZE
       NAVN OF TRANSAKTION DELIMITED BY SIZE
       INTO KONTOUDSKRIFT.
       WRITE KONTOUDSKRIFT.
       MOVE SPACES TO KONTOUDSKRIFT.
       STRING "Adresse: " DELIMITED BY SIZE
       ADRESSE OF TRANSAKTION DELIMITED BY SIZE
       INTO KONTOUDSKRIFT.
       WRITE KONTOUDSKRIFT.
       MOVE SPACES TO KONTOUDSKRIFT.
       EXIT.



       WRITE_KONTO_ID.
       MOVE SPACES TO KONTOUDSKRIFT
       STRING "Kontoudskrift for kontonr.: " DELIMITED BY SIZE
       KONTO_ID OF TRANSAKTION DELIMITED BY SIZE INTO KONTOUDSKRIFT
       WRITE KONTOUDSKRIFT
       MOVE SPACES TO KONTOUDSKRIFT
       EXIT.

       WRITE_TRANSAKTION_HEADER.
       MOVE SPACES TO KONTOUDSKRIFT
       STRING "Dato " DELIMITED BY SIZE
       "Tidspunkt " DELIMITED BY SIZE
       "Transaktionstype " DELIMITED BY SIZE
       "Beløb " DELIMITED BY SIZE
       "(DKK) " DELIMITED BY SIZE
       "Beløb (valuta) " DELIMITED BY SIZE
       "Valutakode " DELIMITED BY SIZE
       "Butik " DELIMITED BY SIZE
       INTO KONTOUDSKRIFT
       WRITE KONTOUDSKRIFT
       MOVE SPACES TO KONTOUDSKRIFT
       EXIT.

       WRITE_TRANSAKTION.
       MOVE SPACES TO KONTOUDSKRIFT
       STRING TIDSPUNKT OF TRANSAKTION DELIMITED BY SIZE
       TRANSAKTIONSTYPE OF TRANSAKTION DELIMITED BY SIZE
       BELØB_RAW OF TRANSAKTION  DELIMITED BY SIZE
       VALUTA OF TRANSAKTION  DELIMITED BY SIZE
       BUTIK OF TRANSAKTION DELIMITED BY SIZE
       INTO KONTOUDSKRIFT
       PERFORM CLEAN_WRITE
       EXIT.


       

       CLEAN_WRITE.
       PERFORM CLEAN_ROW.
       WRITE KONTOUDSKRIFT.
       MOVE SPACES TO KONTOUDSKRIFT.


       CONVERT_VALUTA.
       MOVE Zeros TO BELØB_DECIMAL_DKK
       MOVE ZEROS TO BELØB_DECIMAL
       MOVE ZEROS TO BELØB_DKK_FORMAT
       IF VALUTA OF TRANSAKTION = "EUR"
       MOVE BELØB_RAW TO BELØB_DECIMAL
       COMPUTE BELØB_DECIMAL_DKK = BELØB_DECIMAL * 7.5
       DISPLAY "Fremmed valuta:"
       DISPLAY BELØB_RAW
       DISPLAY BELØB_DECIMAL
       DISPLAY "Konverteret til DKK:"
       MOVE BELØB_DECIMAL_DKK TO BELØB_DKK_FORMAT
       DISPLAY BELØB_DKK_FORMAT
       END-IF
       EXIT.
