       Identification Division.
       Program-Id. Opgave12.
       

       Environment Division.
           input-output section.
               File-control.
                 Select input-sanction assign to "SanctionList.txt"
                 Organization is line sequential.

                 Select input-customer
                 assign to "KundeOplysningerMini.txt"
                 Organization is line sequential.

                 
                 SELECT output-sanction
                  ASSIGN TO 
                   "SanktionsRapport.txt"
                 ORGANIZATION IS LINE SEQUENTIAL.

       Data division.
      
           File Section.
                  Fd input-sanction.
                  01 sanction.
                  Copy "data_copybooks/sanction.cpy".

                  Fd input-customer.
                  01 customer.
                  Copy "data_copybooks/customer.cpy".

                  FD output-sanction.
                  01 sanction-report.
                  05 report-line PIC X(150)   VALUE SPACES.
       
       Working-Storage Section.
      * Dummy variables for functions in development.

                  01 report-line-CLEAN PIC X(150) VALUE SPACES.
                  01 idx-clean pic 9(3) value zeros.
                  01 idx-out pic 9(3) value zeros.
                  01 current-char pic x(1) value spaces.
                  01 previous-char pic x(1) value spaces.
      * Best alias variables.
           01 ws-best-alias-match pic 9(1)V99.
           01 ws-best-alias-idx pic 9(2) value zeros.

           01 ws-decimal-display pic 9.99.
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
           78 SANCTION-ARRAY-SIZE value 200.
           *> Set to 201 to combat a weird memory overflow error.
           01 sanction-array occurs SANCTION-ARRAY-SIZE times
           indexed by sanction-idx.
           copy "data_copybooks/sanction.cpy".
           05 alias-count pic 9(2) value zeros.
           05 birthdate-format pic X(6) value spaces.


      *    Variable definition values.
           78 string-i-max value 40.
           78 levenshtein-i-dim value 41.
           78 string-j-max value 40.
           78 levenshtein-j-dim value 41.
           78 max-alias value 5.
      
      *    Relative levenshtein
           01 levenshtein-info.
           05 name-levenshtein pic 9(1)V99.
           05 aliases occurs MAX-ALIAS times.
           10 alias-levenshtein pic 9(1)V99.
           05 birthdate-levenshtein pic 9(1)V99.
           05 country-levenshtein pic 9(1)V99.
      * Weights
           01 weights.
           05 name-weight pic 9(1)V99 value 0.25.
           05 ws-alias-weight pic 9(1)V99 value 0.05.
           05 birthdate-weight pic 9(1)V99 value 0.30.
           05 country-weight pic 9(1)V99 value 0.20.
      *    Final match variables
           01 match-info.
           05 name-match pic 9(1)V99.
           05 alias-matches occurs MAX-ALIAS times.
           10 alias-match pic 9(1)V99.
           05 birthdate-match pic 9(1)V99.
           05 country-match pic 9(1)V99.
           05 total-match pic 9(1)V99.


      * Working storage
           01 ws-len-i pic 9(4).
           01 ws-levenshtein-i-dim pic 9(4).
           01 ws-len-j pic 9(4).
           01 ws-levenshtein-j-dim pic 9(4).
           01 ws-longer-len pic 9(4).
           01 ws-levenshtein-distance pic 9(4).
           01 ws-relative-levenshtein pic 9(1)V99.
      
           01 ws-string-i pic x(string-i-max) value spaces.
           01 ws-string-j pic x(string-j-max) value spaces.
           
           01 char-idx-name-i pic 9(2) value zeros.
           01 char-idx-name-j pic 9(2) value zeros.

           01 ws-levenshtein-min-value pic 9(2) value zeros.
           01 ws-year-full pic x(4) value spaces.
           01 ws-year-format pic x(4) value spaces.
           01 ws-month-format pic x(2) value spaces.
           01 ws-day-format pic x(2) value spaces.
           01 delimiter-char pic x(1) value '-'.
           
      * initialize levenshtein matrix.

           01 levenshtein-matrix.
               05 levenshtein-row occurs levenshtein-i-dim times
               indexed by levenshtein-idx-i.
             10 levenshtein-column occurs levenshtein-j-dim
              times indexed by levenshtein-idx-j.
                       15 levenshtein-value pic 9(2) value zeros.

       PROCEDURE DIVISION.
       
           OPEN OUTPUT output-sanction.
           perform load-all-sanctions.
           perform loop-through-customers.
           *> Initialize levenshtein matrix



           CLOSE output-sanction.
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
                   (1 - ws-levenshtein-distance / ws-longer-len)
               
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
           
           levenshtein-algorithm.
           
                  PERFORM calculate-length-i.
               PERFORM calculate-length-j.
               compute ws-levenshtein-i-dim = ws-len-i + 1.
               compute ws-levenshtein-j-dim = ws-len-j + 1.

           

               perform calculate-longer-length.
               perform init-levenshtein-matrix.
               perform levenshtein-outer-loop.
               move levenshtein-value (ws-levenshtein-i-dim,
                                   ws-levenshtein-j-dim)
               to ws-levenshtein-distance.
                perform relative-levenshtein-match.
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
           perform levenshtein-algorithm
               move ws-relative-levenshtein to name-levenshtein
               *> Compare customer-name with aliases.
           
               *> re-initialize best match
               move 0 to ws-best-alias-idx
               move 0.00 to ws-best-alias-match
           perform varying alias-idx from 1 by 1
               until alias-idx greater than alias-count of 
               sanction-array(sanction-idx)

               move customer-name of customer
               to ws-string-i
               move alias of sanction-array(sanction-idx, alias-idx)
               to ws-string-j
               perform levenshtein-algorithm
               move ws-relative-levenshtein
               to alias-levenshtein(alias-idx)

               if alias-levenshtein(alias-idx)
               greater than
               ws-best-alias-match

               move alias-levenshtein(alias-idx)
               to ws-best-alias-match

               move alias-idx to ws-best-alias-idx

               end-if
               

               end-perform

               *> Compare birthdates.
               move birthdate of customer
               to ws-string-i
               move birthdate-format of sanction-array(sanction-idx)
               to ws-string-j
               perform levenshtein-algorithm
               move ws-relative-levenshtein
               to birthdate-levenshtein

               *> Compare countries.
               move country-code of customer
               to ws-string-i
               move country-code of sanction-array(sanction-idx)
               to ws-string-j
               perform levenshtein-algorithm
               move ws-relative-levenshtein
               to country-levenshtein
           
           perform calculate-match-percentage

               if total-match > 0.70
               display "Suspicious person detected."
               move customer to report-line
      *         write sanction-report
               perform CLEAN-WRITE
               move sanction-array(sanction-idx) to report-line
      *         write sanction-report
               perform CLEAN-WRITE
               move "Best matching alias: " to report-line
               
      *         write sanction-report
               perform CLEAN-WRITE
               move alias of sanction-array(sanction-idx,
                                   ws-best-alias-idx)
               to report-line
      *         write sanction-report
               perform CLEAN-WRITE
               move ws-best-alias-match to ws-decimal-display
               move ws-decimal-display
               to report-line

      *         write sanction-report
               perform CLEAN-WRITE
               move "Total match:"
               to report-line
      *         write sanction-report
               perform CLEAN-WRITE

               move total-match to ws-decimal-display
               move ws-decimal-display
               to report-line
      *         write sanction-report
               perform CLEAN-WRITE
               end-if
               end-perform.

               *> Remaining to-do.
               *> Display idx of highest matching alias.
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

           perform format-birthdate
           add 1 to sanction-idx
           END-READ
           end-perform.

              close input-sanction.
           exit.

           format-birthdate.
           
                 unstring birthdate of 
                 sanction-array(sanction-idx)
                 delimited by delimiter-char
               into ws-year-full ws-month-format ws-day-format

               move ws-year-full(3:2) to ws-year-format.

               string 
               ws-day-format delimited by size
               ws-month-format delimited by size
               ws-year-format delimited by size
               into birthdate-format of sanction-array(sanction-idx).
           exit.

           calculate-match-percentage.
           *> Re-initialize total-match.
           move 0.00 to total-match.
      *    Assuming levenshtein values have been saved regularly.
           compute name-match = name-levenshtein * name-weight.
           add name-match to total-match.

           perform varying alias-idx from 1 by 1
           until alias-idx greater than MAX-ALIAS
           perform generate-alias-weight

           compute alias-match (alias-idx) =
            alias-levenshtein (alias-idx) * ws-alias-weight
           *> Add to total match.
            add alias-match (alias-idx) to total-match
           

           
           end-perform.

           
           compute birthdate-match =
           birthdate-levenshtein * birthdate-weight.
           add birthdate-match to total-match.
           
           compute country-match =
           country-levenshtein * name-weight.
           add country-match to total-match.
           

      *    Final match variables
           exit.
           
           generate-alias-weight.
           compute ws-alias-weight = 1 /
           alias-count of sanction-array(sanction-idx).

           exit.
           
           new-line.
           MOVE SPACES TO report-line.
           WRITE report-line.
           EXIT.


           
             CLEAN-ROW.
             MOVE 1 TO IDX-clean
             MOVE 1 TO IDX-OUT.
             MOVE SPACES TO CURRENT-CHAR.
             MOVE SPACES TO PREVIOUS-CHAR.
             PERFORM VARYING idx-clean FROM 1 BY 1 UNTIL
             idx-clean > LENGTH OF report-line
      * Update current char
             MOVE report-line(IDX-clean:1) TO CURRENT-CHAR
      * Evaluate current char and previous char
             IF CURRENT-CHAR NOT = SPACE OR PREVIOUS-CHAR NOT = SPACE
                    MOVE CURRENT-CHAR TO report-line-CLEAN(IDX-OUT:1)
             ADD 1 TO IDX-OUT
             END-IF
             MOVE CURRENT-CHAR TO PREVIOUS-CHAR
             END-PERFORM.
             MOVE SPACES TO report-line
             MOVE report-line-CLEAN TO report-line
             MOVE SPACES TO report-line-CLEAN
             EXIT.

             
           CLEAN-WRITE.
           PERFORM CLEAN-ROW.
           write sanction-report.
           perform new-line.
           exit.
