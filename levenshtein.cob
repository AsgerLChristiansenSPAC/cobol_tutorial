       Identification Division.
       Program-Id. Levenshtein.
       

       Environment Division.

       Data division.
      
       
       Working-Storage Section.
           78 WS-NAME-I-LENGTH value 15.
           78 levenshtein-i-dim value 16.
           78 WS-NAME-J-LENGTH value 12.
           78 levenshtein-j-dim value 13.
           01 ws-name-i pic x(ws-name-i-length) value spaces.
           01 ws-name-j pic x(ws-name-j-length) value spaces.
           
           01 char-idx-name-i pic 9(2) value zeros.
           01 char-idx-name-j pic 9(2) value zeros.

           01 ws-levenshtein-min-value pic 9(2) value zeros.

      * initialize levenshtein matrix.
      *     01 levenshtein-idx-i pic x(3) value zeros.
      *     01 levenshtein-idx-j pic x(3) value zeros.

           01 levenshtein-matrix.
               05 levenshtein-row occurs levenshtein-i-dim times
               indexed by levenshtein-idx-i.
             10 levenshtein-column occurs levenshtein-j-dim
              times indexed by levenshtein-idx-j.
                       15 levenshtein-value pic 9(2) value zeros.

       PROCEDURE DIVISION.
           *> Initialize levenshtein matrix
           
           perform init-levenshtein-matrix.
           move "Michael Nielsen" to ws-name-i.
           move "Mikal Nilsen" to ws-name-j.
           perform levenshtein-outer-loop.
           perform display-levenshtein-matrix.
           perform display-levenshtein-distance.

           stop run.
           


           levenshtein-outer-loop.
           
           perform varying levenshtein-idx-i from 2 by 1 until
           levenshtein-idx-i greater than levenshtein-i-dim
           
               perform levenshtein-inner-loop
           end-perform.
           exit.
           
           levenshtein-inner-loop.
           perform varying levenshtein-idx-j from 2 by 1 until
           levenshtein-idx-j greater than levenshtein-j-dim
           

           compute char-idx-name-i = levenshtein-idx-i - 1
           compute char-idx-name-j = levenshtein-idx-j - 1
           
           if ws-name-i(char-idx-name-i: 1)
           equal ws-name-j(char-idx-name-j: 1)
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

           init-levenshtein-matrix.
           *> Initialize first row and column of levenshtein-matrix.
           

           perform varying levenshtein-idx-i from 0 by 1 until 
           levenshtein-idx-i greater than levenshtein-i-dim - 1
               move levenshtein-idx-i
               to levenshtein-value (levenshtein-idx-i + 1, 1)
               

           end-perform.
           
           perform varying levenshtein-idx-j from 0 by 1 until 
           levenshtein-idx-j greater than levenshtein-j-dim - 1
               move levenshtein-idx-j
               to levenshtein-value (1, levenshtein-idx-j + 1)

           end-perform.
           exit.
           
           display-levenshtein-distance.
           display levenshtein-value (LEVENSHTEIN-I-DIM,
                                       levenshtein-j-dim
           )
           exit.
           display-levenshtein-matrix.
           PERFORM VARYING levenshtein-idx-i FROM 1 BY 1
           UNTIL levenshtein-idx-i > levenshtein-i-dim

           PERFORM VARYING levenshtein-idx-j FROM 1 BY 1
           UNTIL levenshtein-idx-j > levenshtein-j-dim

           IF levenshtein-idx-j > 1
            DISPLAY ", " WITH NO ADVANCING
           END-IF

           DISPLAY levenshtein-value(levenshtein-idx-i,
                   levenshtein-idx-j) WITH NO ADVANCING
           END-PERFORM

           DISPLAY " "
           END-PERFORM
           exit.
