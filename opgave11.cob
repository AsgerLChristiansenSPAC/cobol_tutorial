       Identification Division.
       Program-Id. Opgave11.
       
       Environment Division.
           input-output section.
               File-control.
                 Select input-bank assign to "Banker.txt"
                 Organization is line sequential.
                 
                 Select input-transaction assign to
                 "transactionerMini.txt"
                 Organization is line sequential.

       Data division.
      
           File Section.
                  Fd input-bank.
                  01 bank.
                  Copy "data_copybooks/bank.cpy".

                  Fd input-transaction.
                  01 transaction.
                  Copy "data_copybooks/transaction.cpy".

       
       Working-Storage Section.
               
      * Account ID manipulation
           01 current-account-id pic x(15) value spaces.
           01 previous-account-id pic x(15) value spaces.

      * End of File flags

      * transactioner.txt
                  01   eof-transaction
                  pic x value "N".
                  88 eof-transaction-reached value "Y".
      
      * Arrays
           01 account-array occurs 200 times.
           05 account-id pic x(15) value spaces.
           05 account-total pic s9(11)v99.
           05 account-total-format pic +zzzzzzzzz9.99 value zeros.


      * Arithmetic representation of transaction:
           01 ws-amount-decimal pic s9(11)v99.
           01 ws-income-decimal pic s9(11)v99.
           01 ws-expenses-decimal pic s9(11)v99.

      * Output variables
                  01 ws-amount-format-1 pic +zzzzzzzzz9.99 value zeros.
                  01 ws-amount-format-2 pic +zzzzzzzzz9.99 value zeros.
                  01 ws-currency-dkk pic X(3) value "DKK".
                  01 ws-currency-other pic X(3) value spaces.
                  
                  01 ws-total-income pic +zzzzzzzzz9.99    value spaces.
                  01 ws-total-expense pic +zzzzzzzzz9.99 value spaces.
                  01 ws-account-total pic +zzzzzzzzz9.99 value spaces.

      * Data manipulation variables.
                  01 idx-account    pic 9(3) value zeroes.
      
      * for paragraph clean-write
                  01 idx-clean    pic 9(3) value  zeroes.
                  01 current-char pic X(1) value spaces.
                  01 previous-char pic X(1) value spaces.



       PROCEDURE DIVISION.
       
      * Begin loop of Transactions
       Open input input-transaction.
      * Initialize index variables
       Move 0 to idx-account.

      * Main transaction loop
       PERFORM until eof-transaction-reached
           Read input-transaction into transaction 
       AT END
           Set eof-transaction-reached to True
       NOT AT END
      * increment index variables
           add 1 to idx-account
      *    input KONTO-id into array
           Move account-id of transaction to 
           account-array(idx-account)
           
           Perform convert-transaction-currency
           
           Perform update-totals
      * Add totals to array totals
       END-READ
       END-PERFORM.
      * What can be reused?
      * Probably the code related to reading and computing TOTALs.

      * TASK PART 1:  SALDO PER CUStoMER-ID
      * We need an *array* of customer ID's.
      * That array should also contain a name and a Saldo.

      * So. we need to:
      * Read the transaction file as before.
      * Extract relevant information into the array.
      * Somewhere along the way do 
      * transaction-currency-convert and update TOTALs,
      * like in the previous script.

      * And then, as proof of concept, we need to DISPLAY the three
      * highest TOTALs.

       STOP RUN.
      * PARAGRAPHS to IMPORT:

      * Convert transaction-currency
       convert-transaction-currency.
      * Wipe arithmetic variables
       move zeros to ws-amount-decimal
      * Wipe number output variables.
       move zeros to ws-amount-format-1
       move zeros to ws-amount-format-2
      * Wipe foreign currency variable.
       move spaces to ws-currency-other
       if transaction-currency of transaction = "DKK"
      * Convert to number
              move amount-raw to ws-amount-decimal
      * Format for printing.
            move ws-amount-decimal to ws-amount-format-1
      * if not DKK
           else
      * CONVERT to DKK:
      * move to arithmetic data type.
       move amount-raw to ws-amount-decimal
       
      * if foreign currency = EUR
       if transaction-currency of transaction = "EUR"
      * perform conversion.
       compute ws-amount-decimal = ws-amount-decimal * 7.5
       end-if
      * if foreign currency = USD
       if transaction-currency OF transaction = "USD"
       compute ws-amount-decimal = ws-amount-decimal *  6.8
       end-if

      * Now what conversion is achieved, we want to move it to the
      * FIRST column (the DKK column).
       move    ws-amount-decimal to ws-amount-format-1

      * move relevant data into FOREIGN output values.
       move    transaction-currency OF transaction to ws-currency-other
       move    amount-raw OF transaction to ws-amount-format-2
       
       end-if
       EXIT.


      * Update TOTALs.
      
       update-totals.
      * if not new account.

      * Should be run after currency conversion, meaning BELØB DECIMAL
      * should contain DKK.
      * Add to TOTAL if positive:
       if ws-amount-decimal is greater than 0
           COMPUTE ws-income-decimal =
            ws-income-decimal + ws-amount-decimal
       end-if.
      * if negative.
       if ws-amount-decimal is less than 0
       COMPUTE ws-expenses-decimal = ws-expenses-decimal +
       ws-amount-decimal
       end-if.
       EXIT.





      * REMAINING to-DO:

      * TASK PART 2: PRINT PER MONTH
      * We need an array of months. It doesn't seem like we have
      * data for multiple years here.
      * Should most likely be updated every transaction
      * in the same manner.

      * TASK PART 3: PRINT PER SHOP
      * Same stuff. A shop array that is filled regularly.

      * Task part 4: Ranking.

      * top 3 customer IDs (technically part 1)
      * top 5 shops.
