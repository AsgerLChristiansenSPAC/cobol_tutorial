       Identification Division.
       Program-Id. Opgave11.
       
       Environment Division.
           input-output section.
               File-control.
                 Select input-bank assign to "Banker.txt"
                 Organization is line sequential.
                 
                 Select input-transaction assign to
                 "transaktioner.txt"
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
           
           *> Integer division
              *> Account ID manipulation
           01 current-account-id pic x(15) value spaces.
           01 previous-account-id pic x(15) value spaces.

              *> End of File flags

              *> transactioner.txt
                  01   eof-transaction
                  pic x value "N".
                  88 eof-transaction-reached value "Y".
      
           *> Arrays
          
           01 idx-account    pic 9(6) value zeroes.
           
           78 account-array-size value 10000.
           01 account-array occurs account-array-size times.
           05 account-id pic x(15) value spaces.
           05 customer-name pic x(30) value spaces.
           05 account-total pic s9(11)v99.
           05 account-total-format pic +zzzzzzzzz9.99 value zeros.
           
           01 idx-ranking pic 9(1) value 0.
           01 ranking-array occurs 3 times.
               copy "data_copybooks/account_ranking.cpy".


           *> Arithmetic representation of transaction:
           01 ws-amount-decimal pic s9(11)v99.
           01 ws-income-decimal pic s9(11)v99.
           01 ws-expenses-decimal pic s9(11)v99.

              *> Output variables
                  01 ws-amount-format-1 pic +zzzzzzzzz9.99 value zeros.
                  01 ws-amount-format-2 pic +zzzzzzzzz9.99 value zeros.
                  01 ws-currency-dkk pic X(3) value "DKK".
                  01 ws-currency-other pic X(3) value spaces.
                  
                  01 ws-total-income pic +zzzzzzzzz9.99    value spaces.
                  01 ws-total-expense pic +zzzzzzzzz9.99 value spaces.
                  01 ws-account-total pic +zzzzzzzzz9.99 value spaces.

              *> Data manipulation variables.
      
              *> for paragraph clean-write
                  01 idx-clean    pic 9(3) value  zeroes.
                  01 current-char pic X(1) value spaces.
                  01 previous-char pic X(1) value spaces.



       PROCEDURE DIVISION.
       
              *> Begin loop of Transactions
       Open input input-transaction.
              *> Initialize index variables
       Move 0 to idx-account.

              *> Main transaction loop
           display "Beginning transaction loop..."
       PERFORM until eof-transaction-reached
           Read input-transaction into transaction 
       AT END
           Set eof-transaction-reached to True
       NOT AT END

       
              *> Update current account
           MOVE SPACES TO previous-account-id
           MOVE current-account-id TO previous-account-id
           MOVE SPACES TO current-account-id
           MOVE account-id OF transaction TO current-account-id
           
              *> Is this a new account?
           IF current-account-id EQUAL previous-account-id
               perform update-account-row
               
              
              *> Do anything else that needs doing regardless of accounts.
           else
               *> increment index-account, cause its a new account.
               add 1 to idx-account
               
               move account-id of transaction to 
               account-id of account-array(idx-account)
               move 50000 to account-total of account-array(idx-account)
               perform update-account-row
               
           
       END-READ
       END-PERFORM.
              *> What can be reused?
              *> Probably the code related to reading and computing TOTALs.

              *> TASK PART 1:  SALDO PER CUStoMER-ID
              *> We need an         *>array        *> of customer ID's.
              *> That array should also contain a name and a Saldo.

              *> So. we need to:
              *> Read the transaction file as before.
              *> Extract relevant information into the array.
              *> Somewhere along the way do 
              *> transaction-currency-convert and update TOTALs,
              *> like in the previous script.

              *> And then, as proof of concept, we need to DISPLAY the three
              *> highest TOTALs.
           close input-transaction
       display "reading transactions done, beginning ranking..."
       perform update-account-ranking.
       STOP RUN.
              *> PARAGRAPHS to IMPORT:
       


       convert-transaction-currency.
              *> Wipe arithmetic variables
               move zeros to ws-amount-decimal

              *> Wipe number output variables.
               move zeros to ws-amount-format-1
               move zeros to ws-amount-format-2

              *> Wipe foreign currency variable.
               move spaces to ws-currency-other

               if transaction-currency of transaction = "DKK"
               *> Convert to number
               move amount-raw to ws-amount-decimal
               *> Format for printing.
               move ws-amount-decimal to ws-amount-format-1
               *> if not DKK
               else
                   *> CONVERT to DKK:
                   *> move to arithmetic data type.
               move amount-raw to ws-amount-decimal
       
              *> if foreign currency = EUR
               if transaction-currency of transaction = "EUR"
              *> perform conversion.
               compute ws-amount-decimal = ws-amount-decimal         *> 7.5
               end-if
              *> if foreign currency = USD
               if transaction-currency of transaction = "USD"
               compute ws-amount-decimal = ws-amount-decimal         *>  6.8
               end-if

              *> Now what conversion is achieved, we want to move it to the
              *> FIRST column (the DKK column).
               move    ws-amount-decimal to ws-amount-format-1

              *> move relevant data into FOREIGN output values.
               move    transaction-currency of transaction
               to ws-currency-other
               
               move    amount-raw of transaction
               to ws-amount-format-2
       
               end-if
               EXIT.
       

       update-account-ranking.
           move 1 to idx-account.
           perform varying idx-account from 1 by 1 until 
           idx-account > ACCOUNT-ARRAY-SIZE
           if account-id of account-array(idx-account)
           not equal spaces
           *> If (idx) account has higher total than first place
           if account-total of account-array(idx-account)
           greater than
           account-total of ranking-array(1)
           
           *> Move second place to third. Overwrite third.
           move 2 to idx-ranking
           perform cascade-ranks-down

           *> Move first place to second. Overwrite second.
           move 1 to idx-ranking
           perform cascade-ranks-down

           
           *> Move current account to first place.
           move account-id of account-array(idx-account)
           to account-id of ranking-array(1)
           move customer-name of account-array(idx-account)
           to customer-name of ranking-array(1)

           move account-total of account-array(idx-account)
           to account-total of ranking-array(1)

           else
               *> If higher than second place
               if account-total of account-array(idx-account)
               greater than
               account-total of ranking-array(idx-ranking)

               *> Move second place to third. Overwrite third.
               move 2 to idx-ranking
               perform cascade-ranks-down

                  *> Move current account to second place.
                  move account-id of account-array(idx-account)
                  to account-id of ranking-array(2)
                   move customer-name of account-array(idx-account)
                   to customer-name of ranking-array(2)

                  move account-total of account-array(idx-account)
                  to account-total of ranking-array(2)

               else
                   
                   *> If higher than third place
                   if account-total of account-array(idx-account)
                   greater than
                   account-total of ranking-array(idx-ranking)

                   *> Move current-account into third place
                    move account-id of account-array(idx-account)
                   to account-id of ranking-array(3)
                   move customer-name of account-array(idx-account)
                   to customer-name of ranking-array(3)

                    move account-total of account-array(idx-account)
                   to account-total of ranking-array(3)
                   else
                   *> If not greater than any rank, see if there are
                   *> empty ranks to fill.        
                      perform replace-empty-rank
                   end-if
               end-if
           end-if
           end-if

           end-perform
           display "Current ranking:"
           display "First: " account-id of ranking-array(1)
           customer-name of ranking-array(1)
            account-total of ranking-array(1)
           display "Second: " account-id of ranking-array(2)
           customer-name of ranking-array(2)
            account-total of ranking-array(2)
           display "Third: " account-id of ranking-array(3)
           customer-name of ranking-array(3)
            account-total of ranking-array(3)
       exit.
       
       
       replace-empty-rank.
           
           move 1 to idx-ranking
           *> Replace empty slot if second slot empty.
           if account-id of ranking-array(idx-ranking) equal spaces
           move account-id of account-array(idx-account)
           to account-id of ranking-array(idx-ranking)
                   move customer-name of account-array(idx-account)
                   to customer-name of ranking-array(idx-ranking)
           move account-total of account-array(idx-account)
           to account-total of ranking-array(idx-ranking)
           else
               *> Replace empty slot if second slot empty.
               move 2 to idx-ranking
               if account-id of ranking-array(idx-ranking) equal spaces
               
               move account-id of account-array(idx-account)
               to account-id of ranking-array(idx-ranking)
                   move customer-name of account-array(idx-account)
                   to customer-name of ranking-array(idx-ranking)
    
               move account-total of account-array(idx-account)
               to account-total of ranking-array(idx-ranking)
               else
                   *> Replace empty slot if third slot empty.
                   move 3 to idx-ranking
                   if account-id of ranking-array(idx-ranking)
                   equal spaces
                   
                   move account-id of account-array(idx-account)
                   to account-id of ranking-array(idx-ranking)
                   move customer-name of account-array(idx-account)
                   to customer-name of ranking-array(idx-ranking)
        
                   move account-total of account-array(idx-account)
                   to account-total of ranking-array(idx-ranking)
                   
                   end-if
           end-if
       end-if
       exit.


       cascade-ranks-down.
           if idx-ranking < 3
           *> Move currently indexed rank one down.
                  move account-id of ranking-array(idx-ranking)
                  to account-id of ranking-array(idx-ranking + 1)
                  move customer-name of ranking-array(idx-ranking)
                  to customer-name of ranking-array(idx-ranking + 1)
                  
                  
                  move account-total of ranking-array(idx-ranking)
                  to account-total of ranking-array(idx-ranking + 1)

           end-if
           
       exit.

       update-account-row.
           
           move customer-name of transaction to
           customer-name of account-array(idx-account)
           Perform convert-transaction-currency
              *> Once currency conversion is done, 
              *> add ws-amount-decimal to
              *> current account in the array.
           compute account-total of account-array(idx-account) =
           account-total of account-array(idx-account) 
           + ws-amount-decimal
       exit.


       
              *> Update TOTALs.
      
       update-totals.
              *> if not new account.

              *> Should be run after currency conversion, meaning BELØB DECIMAL
              *> should contain DKK.
              *> Add to TOTAL if positive:
       if ws-amount-decimal is greater than 0
           COMPUTE ws-income-decimal =
            ws-income-decimal + ws-amount-decimal
       end-if.
              *> if negative.
       if ws-amount-decimal is less than 0
       COMPUTE ws-expenses-decimal = ws-expenses-decimal +
       ws-amount-decimal
       end-if.
       EXIT.





              *> REMAINING to-DO:

              *> TASK PART 2: PRINT PER MONTH
              *> We need an array of months. It doesn't seem like we have
              *> data for multiple years here.
              *> Should most likely be updated every transaction
              *> in the same manner.

              *> TASK PART 3: PRINT PER SHOP
              *> Same stuff. A shop array that is filled regularly.

              *> Task part 4: Ranking.

              *> top 3 customer IDs (technically part 1)
              *> top 5 shops.

