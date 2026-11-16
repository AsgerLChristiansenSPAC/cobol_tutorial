       identification division.
       Program-id. Opgave12.
       

       environment division.
           input-output section.
               File-control.
                 Select input-sanction assign to "SanctionList.txt"
                 Organization is line sequential.

                 Select input-customer
                 assign to "KundeOplysninger.txt"
                 Organization is line sequential.

                 
                 Select output-sanction
                  assign to 
                   "SanktionsRapport.txt"
                 OrganizatiOn is line Sequential.

       data division.
      
           File Section.
                  Fd input-sanction.
                  01 sanction.
                  Copy "data_copybooks/sanction.cpy".

                  Fd input-customer.
                  01 customer.
                  Copy "data_copybooks/customer.cpy".

                  Fd output-sanction.
                  01 sanction-report.
                  05 report-line pic x(150)   value spaces.
       
       Working-Storage Section.
      * dummy variables for functions in development.

                  01 report-line-clean pic x(150) value spaces.
                  01 idx-clean pic 9(3) value zeros.
                  01 idx-out pic 9(3) value zeros.
                  01 current-char pic x(1) value spaces.
                  01 previous-char pic x(1) value spaces.
      * Best alias variables.
           01 ws-best-alias-match pic 9(1)v99.
           01 ws-best-alias-idx pic 9(2) value zeros.

           01 ws-decimal-display pic 9.99.
           01 ws-decimal-display-2 pic 9.99.
           01 ws-decimal-display-3 pic 9.99.
      * end of file flags
           *> Sanctions eof
           
           
           01   eof-sanction
           pic x value "n".
           88 eof-sanction-reached value "y".

           01   eof-customer
           pic x value "n".
           88 eof-customer-reached value "y".
      *    variable definition values.
           78 string-i-max value 40.
           78 levenshtein-i-dim value 41.
           78 string-j-max value 40.
           78 levenshtein-j-dim value 41.
           78 MAX-ALIAS value 5.
           78 SANCTION-ARRAY-SIZE value 200.
      * arrays
           
           01 alias-idx pic 9(2) value zeros.
           *> Sanctions array
           *> Set to 201 to combat a weird memory overflow error.
           01 sanction-array occurs SANCTION-ARRAY-SIZE times
           indexed by sanction-idx.
           copy "data_copybooks/sanction.cpy".
           05 alias-count pic 9(2) value zeros.
           05 birthdate-format pic x(6) value spaces.


           

           01 relevant-aliases occurs MAX-ALIAS times.
           05 alias pic x(20) value spaces.
           05 alias-levenshtein pic 9(1)v99 value zeros.
           05 alias-match pic 9(1)v99 value zeros.
           05 alias-nr pic 9(1) value 0.
           
           01 ws-relevant-alias-count pic 9(1) value 0.
      
      *    relative levenshtein
           01 levenshtein-info.
           05 name-levenshtein pic 9(1)v99.
           05 aliases occurs Max-aliaS times.
           10 alias-levenshtein pic 9(1)v99.
           05 birthdate-levenshtein pic 9(1)v99.
           05 country-levenshtein pic 9(1)v99.
      * Weights
           01 weights.
           05 name-weight pic 9(1)v99 value 0.25.
           05 raw-alias-weight pic 9(1)v99 value 0.25.
           05 ws-alias-weight pic 9(1)v99 value 0.05.
           05 birthdate-weight pic 9(1)v99 value 0.30.
           05 country-weight pic 9(1)v99 value 0.20.
      *    Final match variables
           01 match-info.
           05 name-match pic 9(1)v99.
           05 birthdate-match pic 9(1)v99.
           05 country-match pic 9(1)v99.
           05 total-match pic 9(1)v99.
      *>     05 alias-matches occurs Max-aliaS times.
      *>     10 alias-match pic 9(1)v99.


      * Working storage
           01 ws-len-i pic 9(4).
           01 ws-levenshtein-i-dim pic 9(4).
           01 ws-len-j pic 9(4).
           01 ws-levenshtein-j-dim pic 9(4).
           01 ws-longer-len pic 9(4).
           01 ws-levenshtein-distance pic 9(4).
           01 ws-relative-levenshtein pic 9(1)v99.
      
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

           01 ws-report-line pic x(150) value spaces.

       Procedure Division.
           open output output-sanction.
           perform load-all-sanctions.
           perform loop-through-customers.
           *> initialize levenshtein matrix



           close output-sanction.
           stop run.

           calculate-length-i.
           move 0 to ws-len-i.
           move function reverse(ws-string-i) to ws-string-i
           inspect ws-string-i tallying ws-len-i for leading spaces.
           subtract ws-len-i from 
           function length (ws-string-i) giving ws-len-i.
        *> re-initialize ws-string-i.
           move function reverse(ws-string-i) to ws-string-i
           exit.
        
           calculate-length-j.
           move 0 to ws-len-j.
           move function reverse(ws-string-j) to ws-string-j
           inspect ws-string-j tallying ws-len-j for leading spaces
           subtract ws-len-j 
           from function length (ws-string-j) giving ws-len-j
        *> re-initialize ws-string-j.
           move function reverse(ws-string-j) to ws-string-j
           
           exit.

           calculate-longer-length.
           
           *> move longer string length to working storage
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
           *> if letters equal.
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
           *> initialize first row and column of levenshtein-matrix.
           

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
           
                  perform calculate-length-i.
               perform calculate-length-j.
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

               *> Loop through aliases
               perform varying alias-idx from 1 by 1
               until alias-idx greater than alias-count of 
               sanction-array(sanction-idx)

               move customer-name of customer
               to ws-string-i
               move alias of sanction-array(sanction-idx, alias-idx)
               to ws-string-j
               perform levenshtein-algorithm
               move ws-relative-levenshtein
               to alias-levenshtein of levenshtein-info(alias-idx)

               if alias-levenshtein of levenshtein-info(alias-idx)
               greater than
               ws-best-alias-match

               move alias-levenshtein of levenshtein-info(alias-idx)
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

               if total-match > 0.50
       *>        display "Suspicious person detected."
       *>        display alias-count of sanction-array (sanction-idx)
       *>        display "results:"
       *>        display birthdate-match
       *>        display country-match
       *>        display "Total match:"
       *>        display total-match
               perform write-report-entry
       
               end-if
           end-perform.

           exit.

           loop-through-customers.

           open input input-customer.
              display "reading customers..."
           perform until eof-customer-reached
           read input-customer into customer
           at end
               set eof-customer-reached to true
           not at end
           perform compare-with-sanctions

           end-read
           end-perform.
           close input-customer.
           exit.


           load-all-sanctions.
           *> load all sanctions into sanction-array.
           open input input-sanction.
              display "reading sanctions..."
           perform until eof-sanction-reached
           read input-sanction into sanction
           at end
               set eof-sanction-reached to true
           not at end
           *> move sanction into sanction array
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
           end-read
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
           *> re-initialize total-match.
           move 0.00 to total-match.
           *> Re-initialize "relevant aliases" count here.
           move 0 to ws-relevant-alias-count


       *> Wrangling RELEVANT aliases:
           perform varying alias-idx from 1 by 1
           until alias-idx greater than
           alias-count of sanction-array(sanction-idx)

           if alias-levenshtein of levenshtein-info (alias-idx) > 0.5
               *> Increment relevant-alias count
               add 1 to ws-relevant-alias-count
               *> Which alias is it?
               move alias-idx
               to alias-nr
               of relevant-aliases (ws-relevant-alias-count)

               move alias of sanction-array (sanction-idx, alias-idx)
               to alias
               of relevant-aliases (ws-relevant-alias-count)


               *> What is the unweighted score?
               move alias-levenshtein of levenshtein-info (alias-idx)
               to alias-levenshtein
               of relevant-aliases (ws-relevant-alias-count)


           end-if

           

           end-perform.
           *> Loop through RELEVANT aliases.
           perform varying alias-idx from 1 by 1
           until alias-idx greater than
           ws-relevant-alias-count
           *> Based on relevant alias count, generate weight.
           perform generate-alias-weight
           
            compute alias-match  of relevant-aliases (alias-idx) =
            alias-levenshtein  of relevant-aliases (alias-idx)
             * ws-alias-weight
                  *> add to total match.
                  
                   add alias-match of relevant-aliases (alias-idx) 
                   to total-match
           end-perform.
           
           
      *    Were there any relevant alias matches?
           if ws-relevant-alias-count > 0
           move 0.25 to name-weight

           compute name-match = name-levenshtein * name-weight
           add name-match to total-match

           else
           *> If no relevant aliases, name accounts for 50% of weight.
           move 0.5 to name-weight
           compute name-match = name-levenshtein * name-weight
           add name-match to total-match

           end-if

           compute birthdate-match =
           birthdate-levenshtein * birthdate-weight.
           add birthdate-match to total-match.
           
           compute country-match =
           country-levenshtein * country-weight.
           add country-match to total-match.
           

      *    Final match variables
           exit.
           
           generate-alias-weight.
           compute ws-alias-weight = raw-alias-weight * 1 /
           ws-relevant-alias-count

           exit.
           
           new-line.
           move spaces to report-line.
           write sanction-report.
           exit.


           
             clean-row.
             move 1 to idx-clean
             move 1 to idx-out.
             move spaces to current-char.
             move spaces to previous-char.
             perform varying idx-clean FrOM 1 By 1 Until
             idx-clean > length OF report-line
      * Update current char
             move report-line(idx-clean:1) to current-char
      * evaluate current char and previous char
             iF current-char not = space Or previous-char not = space
                    move current-char to report-line-clean(idx-out:1)
             add 1 to idx-out
             end-iF
             move current-char to previous-char
             end-perform.
             move spaces to report-line
             move report-line-clean to report-line
             move spaces to report-line-clean
             exit.

             
           clean-write.
           perform clean-row.
           write sanction-report.
           move spaces to ws-report-line.
           move spaces to report-line.
           exit.
           


           write-report-entry.

       
           perform write-report-boundary
           perform write-customer-info
           
           perform new-line
           
           perform write-sanction-info
           perform new-line
           perform write-match-description
           perform write-report-boundary
           perform new-line

           exit.

           write-report-boundary.
           string "-------------------------" delimited by size
           "---------------------------------" delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           exit.


           write-customer-info.
           
           string "Kunde-ID. " delimited by size
           cpr-nr of customer delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           
           string "Kundenavn: " delimited by size
           customer-name of customer delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           
           string "Fødselsdato: " delimited by size
           birthdate of customer delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           
           string "Adresse: " delimited by size
           adress delimited by size
           ", " delimited by size
           country-code of customer delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           perform new-line
           exit.

           write-sanction-info.

           move "Match fundet med:" to report-line
           perform clean-write
           
           string "Sanction-ID: " delimited by size
           sanction-id of
           sanction-array (sanction-idx) delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           
           string "Navn: " delimited by size
           sanction-name of 
           sanction-array (sanction-idx) delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           
       *>    string "Best matchende alias: " delimited by size
       *>    alias of sanction-array(sanction-idx,
       *>                            ws-best-alias-idx)
       *>    delimited by size
       *>    into ws-report-line
       *>    move ws-report-line to report-line
       *>    perform clean-write

           string "Fødselsdato: " delimited by size
           birthdate-format of
            sanction-array (sanction-idx) delimited by size
              into ws-report-line
           move ws-report-line to report-line
              perform clean-write
       
           string "Land: " delimited by size
           country-code 
           of sanction-array (sanction-idx) delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           perform new-line
           exit.

           write-match-description.
           
           
           move "Match-beskrivelse:" to report-line
           perform clean-write

           
           move "Kategori - Uvægtet Match - Vægt - Vægtet Match"
           to report-line
           perform clean-write

           move name-levenshtein to ws-decimal-display
           move name-weight to ws-decimal-display-2
           move name-match to ws-decimal-display-3
           string "- Navn: " delimited by size
           ws-decimal-display delimited by size
           "% " delimited by size
           "- " delimited by size
           ws-decimal-display-2 delimited by size
           "% " delimited by size
           "- " delimited by size 
           ws-decimal-display-3 delimited by size
           "% " delimited by size           
           "(" delimited by size
           customer-name delimited by size
           " og " delimited by size
           sanction-name of sanction-array (sanction-idx)
            delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           
           perform varying alias-idx from 1 by 1 until
           alias-idx greater than ws-relevant-alias-count

           
           move alias-levenshtein of relevant-aliases (alias-idx)
            to ws-decimal-display
           move ws-alias-weight to ws-decimal-display-2
           move alias-match of relevant-aliases (alias-idx)
            to ws-decimal-display-3
           string "- Alias (" delimited by size
           alias-nr of relevant-aliases (alias-idx)
           delimited by size
           "): " 
           delimited by size
           ws-decimal-display delimited by size
           "% " delimited by size
           "- " delimited by size
           ws-decimal-display-2 delimited by size
           "% " delimited by size
           "- " delimited by size 
           ws-decimal-display-3 delimited by size
           "% " delimited by size           
           "(" delimited by size
            customer-name  delimited by size
           " og " delimited by size
           alias of relevant-aliases (alias-idx) delimited by size
           ")" delimited by size
           into ws-report-line


           move ws-report-line to report-line

           perform clean-write
           end-perform
           move birthdate-levenshtein to ws-decimal-display
           move birthdate-weight to ws-decimal-display-2
           move birthdate-match to ws-decimal-display-3
           string "- Fødselsdato: " delimited by size
           ws-decimal-display delimited by size
           "% " delimited by size
           "- " delimited by size
           ws-decimal-display-2 delimited by size
           "% " delimited by size
           "- " delimited by size 
           ws-decimal-display-3 delimited by size
           "% " delimited by size         
           "(" delimited by size
            birthdate of customer  delimited by size
           " og " delimited by size
           birthdate-format of sanction-array (sanction-idx)
            delimited by size
           ")" delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           
           move country-levenshtein to ws-decimal-display
           move country-weight to ws-decimal-display-2
           move country-match to ws-decimal-display-3
           string "- Land: " delimited by size
           ws-decimal-display delimited by size
           "% " delimited by size
           "- " delimited by size
           ws-decimal-display-2 delimited by size
           "% " delimited by size
           "- " delimited by size 
           ws-decimal-display-3 delimited by size
           "% " delimited by size         
           "(" delimited by size
            country-code of customer  delimited by size
           " og " delimited by size
           country-code of sanction-array (sanction-idx)
            delimited by size
           ")" delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           
           move total-match to ws-decimal-display
           string "Samlet match-procent: " delimited by size
            ws-decimal-display delimited by size
           "%" delimited by size
           into ws-report-line
           move ws-report-line to report-line
           perform clean-write
           exit.

