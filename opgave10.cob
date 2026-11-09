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
                  05 NY_LINJE PIC X(110)   VALUE SPACES.
       
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
                  01 BELØB_FORMAT_ONE PIC +ZZZZZZZZZ9.99 VALUE SPACES.
                  01 BELØB_FORMAT_TWO PIC +ZZZZZZZZZ9.99 VALUE SPACES.
                  01 VALUTA_DKK PIC X(3) VALUE "DKK".
                  01 VALUTA_OTHER PIC X(3) VALUE SPACES.

      * Data manipulation variables.
                  01 IDX        PIC 9(3) VALUE ZEROES.
                  01 IDX_OUT    PIC 9(3) VALUE ZEROES.
                  01 IDX_BANK   PIC 9(3) VALUE ZEROS.
                  
                  01 CURRENT_CHAR PIC X(1) VALUE SPACES.
                  01 PREVIOUS_CHAR PIC X(1) VALUE SPACES.

                  01 KONTOUDSKRIFT_CLEAN PIC X(100) VALUE SPACES.

                  01 BELØB_DECIMAL_DKK PIC S9(11)V99 VALUE ZEROES.

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
      * If foreign currency:
              PERFORM CONVERT_VALUTA
      * If same account
              PERFORM WRITE_TRANSAKTION





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
      * If foreign currency:
              PERFORM CONVERT_VALUTA
      * If same account
              PERFORM WRITE_TRANSAKTION
    
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
       BELØB_FORMAT_ONE  DELIMITED BY SIZE
       VALUTA_DKK DELIMITED BY SIZE
       BELØB_FORMAT_TWO DELIMITED BY SIZE
       VALUTA_OTHER  DELIMITED BY SIZE
       BUTIK OF TRANSAKTION DELIMITED BY SIZE
       INTO KONTOUDSKRIFT
       PERFORM CLEAN_WRITE
       EXIT.


       

       CLEAN_WRITE.
       PERFORM CLEAN_ROW.
       WRITE KONTOUDSKRIFT.
       MOVE SPACES TO KONTOUDSKRIFT.


       CONVERT_VALUTA.
      * Wipe arithmetic variables
       MOVE Zeros TO BELØB_DECIMAL_DKK
       MOVE ZEROS TO BELØB_DECIMAL
      * Wipe number output variables.
       MOVE ZEROS TO BELØB_FORMAT_ONE
       MOVE ZEROS TO BELØB_FORMAT_TWO
      * Wipe foreign currency variable.
       MOVE SPACES TO VALUTA_OTHER
       IF VALUTA OF TRANSAKTION = "DKK"
       DISPLAY "yay"
      * Convert to number
              MOVE BELØB_RAW TO BELØB_DECIMAL
      * Format for printing.
            MOVE BELØB_DECIMAL TO BELØB_FORMAT_ONE
            DISPLAY BELØB_RAW
            DISPLAY BELØB_DECIMAL
            DISPLAY BELØB_FORMAT_ONE
      * If not DKK
       ELSE
      * CONVERT TO DKK:
      * Move to arithmetic data type.
       MOVE BELØB_RAW TO BELØB_DECIMAL
       
      * If foreign currency = EUR
       IF VALUTA OF TRANSAKTION = "EUR"
      * Perform conversion.
       COMPUTE BELØB_DECIMAL = BELØB_DECIMAL * 7.5
       END-IF
      * If foreign currency = USD
       IF VALUTA OF TRANSAKTION = "USD"
       DISPLAY "nay"
       COMPUTE BELØB_DECIMAL = BELØB_DECIMAL * 6.8
       END-IF

      * Now what conversion is achieved, we want to move it to the
      * FIRST column (the DKK column).
       MOVE BELØB_DECIMAL TO BELØB_FORMAT_ONE

      * Move relevant data into FOREIGN output values.
       MOVE VALUTA OF TRANSAKTION TO VALUTA_OTHER
       MOVE BELØB_RAW OF TRANSAKTION TO BELØB_FORMAT_TWO
       
       END-IF
       EXIT.
