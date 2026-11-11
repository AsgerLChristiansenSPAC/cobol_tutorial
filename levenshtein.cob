       Identification Division.
       Program-Id. Levenshtein.
       

       Environment Division.

       Data division.
      
       
       Working-Storage Section.
           01 ws-name-one pic x(10) value spaces.
           01 ws-name-two pic x(10) value spaces.
      
      * initialize levenshtein matrix.
      *     01 levenshtein-idx-i pic x(3) value zeros.
      *     01 levenshtein-idx-j pic x(3) value zeros.

           01 levenshtein-matrix.
               05 levenshtein-row occurs 10 times
               indexed by levenshtein-idx-i.
                  10 levenshtein-column occurs 10 times
               indexed by levenshtein-idx-j.
                       15 levenshtein-value pic 9(2) value zeros.

       PROCEDURE DIVISION.
           move "kitten" to ws-name-one.
           move "sitting" to ws-name-two.
           
           display ws-name-one(levenshtein-idx-i:1)
           display ws-name-two(levenshtein-idx-j: 1)

           display levenshtein-value (levenshtein-idx-i,
                                       levenshtein-idx-j)
           stop run.
