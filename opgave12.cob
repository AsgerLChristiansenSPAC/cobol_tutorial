       Identification Division.
       Program-Id. Opgave12.
       

       Environment Division.
           input-output section.
               File-control.
                 Select input-sanction assign to "SanctionListMini.txt"
                 Organization is line sequential.

                 Select input-customer
                 assign to "KundeOplysningerMini.txt"
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

      * End of file flags
           *> Sanctions eof
           
           
           01   eof-sanction
           pic x value "N".
           88 eof-sanction-reached value "Y".

           01   eof-customer
           pic x value "N".
           88 eof-customer-reached value "Y".
      * Arrays
           
           01 alias-idx pic 9(2) value zeros.
           *> Sanctions array
           78 SANCTION-ARRAY-SIZE value 5.
           *> Set to 201 to combat a weird memory overflow error.
           01 sanction-array occurs SANCTION-ARRAY-SIZE times
           indexed by sanction-idx.
           copy "data_copybooks/sanction.cpy".
           05 alias-count pic 9(2) value zeros.

      *     01 alias-statistics occurs SANCTION-ARRAY-SIZE times
      *     indexed by sanction-idx.

      *    Variable definition values.
           78 string-i-max value 40.
           78 levenshtein-i-dim value 41.
           78 string-j-max value 40.
           78 levenshtein-j-dim value 41.
           78 max-alias value 5.
           
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
           perform load-all-sanctions.
           perform loop-through-customers.
           *> Initialize levenshtein matrix



           stop run.

           calculate-length-i.
           move 0 to ws-len-i.
           move function reverse(ws-string-i) to ws-string-i
           inspect ws-string-i tallying ws-len-i for leading spaces.
           subtract ws-len-i from 
           function length (ws-string-i) giving ws-len-i.
        *> Re-initialize ws-string-i.
           move function reverse(ws-string-i) to ws-string-i
           exit.
        
           calculate-length-j.
           move 0 to ws-len-j.
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


           
           levenshtein-algorithm.
           
                  PERFORM calculate-length-i.
               DISPLAY "Actual Length i : " ws-len-i.
               PERFORM calculate-length-j.
           DISPLAY "Actual Length j : " ws-len-j.
               compute ws-levenshtein-i-dim = ws-len-i + 1.
               compute ws-levenshtein-j-dim = ws-len-j + 1.
                display "Matrix dimensions: "
               display "i : " ws-levenshtein-i-dim.
               display "j : " ws-levenshtein-j-dim.

           

               perform calculate-longer-length.
               perform init-levenshtein-matrix.
               perform levenshtein-outer-loop.
               move levenshtein-value (ws-levenshtein-i-dim,
                                   ws-levenshtein-j-dim)
               to ws-levenshtein-distance.
           display "Levenshtein distance: " ws-levenshtein-distance.
                perform relative-levenshtein-match.
           display "With longer string length " ws-longer-len
                    ", relative levenshtein distance is: "
                    ws-relative-levenshtein "%."
           exit.

           compare-with-sanctions.
               *> For each sanctioned person (comparisons scale
               *> very quickly!!)
               perform varying sanction-idx from 1 by 1
               until sanction-idx greater than SANCTION-ARRAY-SIZE
               
               *> Compare customer-name.
               move customer-name of customer
               to ws-string-i
               move sanction-name of sanction-array(sanction-idx)
               to ws-string-j
               display "Comparing names:"
               display ws-string-i
               display ws-string-j
               perform levenshtein-algorithm

               end-perform.
           exit.

           loop-through-customers.

           open input input-customer.
              display "Reading customers..."
           perform until eof-customer-reached
           read input-customer into customer
           AT END
               set eof-customer-reached to True
           NOT AT END
           perform compare-with-sanctions

           END-READ
           end-perform.
           close input-customer.
           exit.
               *> Loop through the sanction array.
               
               *> Compare name with name.
               *> Extract absolute levenshtein score.
               *> Compute relative levenshtein score.
               *> Display if fishy.

               *> Compare name with each alias.
               *> Repeat steps taken above

               *> Compare birthdate with birthdate.
               *> Repeat steps taken above

               *> Simply check if country is equal.
               *> Display if fishy.

           
               *> MVP solution: If there is anything fishy, display it.
               
               *> What data structures do we need?
               *> Basically... none? Let's try coding it display-
               *> oriented.


           load-all-sanctions.
           *> Load all sanctions into sanction-array.
           open input input-sanction.
              display "Reading sanctions..."
           perform until eof-sanction-reached
           read input-sanction into sanction
           AT END
               set eof-sanction-reached to True
           NOT AT END
           *> Move sanction into sanction array
           move sanction 
           to sanction-array(sanction-idx)

           perform varying alias-idx from 1 by 1 until alias-idx 
           greater
           than max-alias 
           
           move function trim(
            alias of sanction-array(sanction-idx, alias-idx)) 
           to alias of sanction-array(sanction-idx, alias-idx) 

           if alias  of sanction-array (sanction-idx, alias-idx)
           not equal "-"

           add 1 to alias-count of sanction-array (sanction-idx)

           end-if
           
           end-perform
           add 1 to sanction-idx
           END-READ
           end-perform.

              close input-sanction.
           exit.
