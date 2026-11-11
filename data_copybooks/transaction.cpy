       05 customer-cpr PIC X(15) VALUE SPACES.
       05 customer-name PIC X(30) VALUE SPACES.
       05 adress PIC X(50) VALUE SPACES.
       05 birthday PIC X(10) VALUE SPACES.
      * 105
       05 account-id PIC X(15) VALUE SPACES.
       05 bank-reg-nr PIC X(4) VALUE SPACES.
      * 19 + 105 = 124
       05 amount-raw PIC X(17) VALUE SPACES.
      * 124 + 15 + 2 = 141.
       05 transaction-currency PIC X(3) VALUE SPACES.
       05 transaction-type PIC X(20) VALUE SPACES.
      * 141 + 18 = 159
       05 shop-name PIC X(21) VALUE SPACES.  
       05 datetime PIC X(40) VALUE SPACES.
       