       Identification Division.
       Program-Id. Opgave12.
       

       Environment Division.
           input-output section.
               File-control.
                 Select input-sanction assign to "SanctionList.txt"
                 Organization is line sequential.

                 Select input-customer assign to "KundeOplysninger.txt"
                 Organization is line sequential.

       Data division.
      
           File Section.
                  Fd input-sanction.
                  01 sanction.
                  Copy "data_copybooks/sanction.cpy".

                  Fd input-customer.
                  01 customer.
                  Copy "data_copybooks/customer.cpy".
       
       Working-Storage Section.
      * Dummy variables for functions in development.
           *> Sanctions array
           78 sanction-array-size value 200.
           01 sanction-array occurs SANCTION-ARRAY-SIZE times
           indexed by sanction-idx.
           copy "data_copybooks/sanction.cpy".

      *    Variable definition values.
           78 string-i-max value 40.
           78 levenshtein-i-dim value 41.
           78 string-j-max value 40.
           78 levenshtein-j-dim value 41.
           
      * Working storage
           01 ws-len-i pic 9(4).
           01 ws-levenshtein-i-dim pic 9(4).
           01 ws-len-j pic 9(4).
           01 ws-levenshtein-j-dim pic 9(4).
           01 ws-longer-len pic 9(4).
           01 ws-levenshtein-distance pic 9(4).
           01 ws-relative-levenshtein pic 9(4).
      
           01 ws-string-i pic x(string-i-max) value spaces.
           01 ws-string-j pic x(string-j-max) value spaces.
           
           01 char-idx-name-i pic 9(2) value zeros.
           01 char-idx-name-j pic 9(2) value zeros.

           01 ws-levenshtein-min-value pic 9(2) value zeros.
           
      * initialize levenshtein matrix.

           01 levenshtein-matrix.
               05 levenshtein-row occurs levenshtein-i-dim times
               indexed by levenshtein-idx-i.
             10 levenshtein-column occurs levenshtein-j-dim
              times indexed by levenshtein-idx-j.
                       15 levenshtein-value pic 9(2) value zeros.

       PROCEDURE DIVISION.
           *> Initialize levenshtein matrix

           move "John Smith" to ws-string-i.
           move "Smith John" to ws-string-j.

      *     MOVE "kattepine" TO WS-STRING-i.
      *     move "catnip and stuff" to ws-string-j.
           PERFORM calculate-length-i.
           DISPLAY "Actual Length: " ws-len-i
           PERFORM calculate-length-j.
           DISPLAY "Actual Length: " ws-len-j
           compute ws-levenshtein-i-dim = ws-len-i + 1.
           compute ws-levenshtein-j-dim = ws-len-j + 1.
           display "Matrix dimensions: "
           display "i : " ws-levenshtein-i-dim.
           display "j : " ws-levenshtein-j-dim.

           perform calculate-longer-length.
           perform init-levenshtein-matrix.
           perform levenshtein-outer-loop.
           perform display-levenshtein-matrix.
           move levenshtein-value (ws-levenshtein-i-dim,
                                   ws-levenshtein-j-dim)
           to ws-levenshtein-distance.
           display "Levenshtein distance: " ws-levenshtein-distance.
           perform relative-levenshtein-match.
           display "With longer string length " ws-longer-len
                    ", relative levenshtein distance is: "
                    ws-relative-levenshtein "%."



           stop run.

           calculate-length-i.
           move function reverse(ws-string-i) to ws-string-i
           inspect ws-string-i tallying ws-len-i for leading spaces.
           subtract ws-len-i from 
           function length (ws-string-i) giving ws-len-i.
        *> Re-initialize ws-string-i.
           move function reverse(ws-string-i) to ws-string-i
           exit.
        
           calculate-length-j.
           move function reverse(ws-string-j) to ws-string-j
           inspect ws-string-j tallying ws-len-j for leading spaces
           subtract ws-len-j 
           from function length (ws-string-j) giving ws-len-j
        *> Re-initialize ws-string-j.
           move function reverse(ws-string-j) to ws-string-j
           
           exit.

           calculate-longer-length.
           
           *> Move longer string length to working storage
           if ws-len-i greater than ws-len-j
               move ws-len-i to ws-longer-len
           end-if

           if ws-len-j greater than ws-len-i
               move ws-len-j to ws-longer-len
           end-if
           
           if ws-len-i equal ws-len-j
               move ws-len-i to ws-longer-len
           end-if

           exit.

           levenshtein-outer-loop.
           
           perform varying levenshtein-idx-i from 2 by 1 until
           levenshtein-idx-i greater than ws-levenshtein-i-dim
           
               perform levenshtein-inner-loop
           end-perform.
           exit.
           
           levenshtein-inner-loop.
           perform varying levenshtein-idx-j from 2 by 1 until
           levenshtein-idx-j greater than ws-levenshtein-j-dim
           

           compute char-idx-name-i = levenshtein-idx-i - 1
           compute char-idx-name-j = levenshtein-idx-j - 1
           
           if ws-string-i(char-idx-name-i: 1)
           equal ws-string-j(char-idx-name-j: 1)
           *> If letters equal.
           move
           levenshtein-value (levenshtein-idx-i - 1, 
                               levenshtein-idx-j - 1)
           to levenshtein-value (levenshtein-idx-i,
                                 levenshtein-idx-j)
           
           else
           *> if letters not equal
           perform compute-levenshtein
               move ws-levenshtein-min-value
               to levenshtein-value (levenshtein-idx-i,
                                     levenshtein-idx-j)
           end-if
           end-perform.
           exit.
           
           compute-levenshtein.
           compute ws-levenshtein-min-value = 1 + function min(
            levenshtein-value(levenshtein-idx-i - 1,
                              levenshtein-idx-j - 1),

            levenshtein-value(levenshtein-idx-i,
                              levenshtein-idx-j - 1),

            levenshtein-value(levenshtein-idx-i - 1,
                              levenshtein-idx-j)

           )
           exit.
           
           relative-levenshtein-match.
           compute ws-relative-levenshtein = 
                   (1 - ws-levenshtein-distance / ws-longer-len) * 100
               
           exit.

           init-levenshtein-matrix.
           *> Initialize first row and column of levenshtein-matrix.
           

           perform varying levenshtein-idx-i from 0 by 1 until 
           levenshtein-idx-i greater than ws-levenshtein-i-dim - 1
               move levenshtein-idx-i
               to levenshtein-value (levenshtein-idx-i + 1, 1)
               

           end-perform.
           
           perform varying levenshtein-idx-j from 0 by 1 until 
           levenshtein-idx-j greater than ws-levenshtein-j-dim - 1
               move levenshtein-idx-j
               to levenshtein-value (1, levenshtein-idx-j + 1)

           end-perform.
           exit.
           
           display-levenshtein-matrix.
           PERFORM VARYING levenshtein-idx-i FROM 1 BY 1
           UNTIL levenshtein-idx-i > ws-levenshtein-i-dim

           PERFORM VARYING levenshtein-idx-j FROM 1 BY 1
           UNTIL levenshtein-idx-j > ws-levenshtein-j-dim

           IF levenshtein-idx-j > 1
            DISPLAY ", " WITH NO ADVANCING
           END-IF

           DISPLAY levenshtein-value(levenshtein-idx-i,
                   levenshtein-idx-j) WITH NO ADVANCING
           END-PERFORM

           DISPLAY " "
           END-PERFORM
           exit.


           *> So the next step: 
           
           *> the architecture around it.
           *> Loading the sanctions list.
           *> Looping through the customer ID.
           *> Doing the levenshtein distance thingy a gajillion times
           *> per customer.
           *> Saving the scores.

