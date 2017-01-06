\ BEGIN-STRUCTURE and friends.
\
\ This code is part of pForth.
\
\ The pForth software code is dedicated to the public domain,
\ and any third party may reproduce, distribute and modify
\ the pForth software code or any derivative works thereof
\ without any compensation or license.  The pForth software
\ code is provided on an "as is" basis without any warranty
\ of any kind, including, without limitation, the implied
\ warranties of merchantability and fitness for a particular
\ purpose and their equivalents under the laws of any jurisdiction.

\ Implementation straight out of the Forth 2012 standard.

: +FIELD  ( n1 n2 "name" -- n1+n2 ; Exec: addr -- 'addr )
    CREATE
        over , +
    DOES>
        @ +
;

: BEGIN-STRUCTURE  ( "<spaces>name" -- struct-sys 0 )
    CREATE
        here 0 0 ,      \ mark stack, lay dummy
    DOES>
        @             \ -- rec-len
;

: END-STRUCTURE  ( struct-sys +n -- ) swap ! ; \ set len

: FIELD:    ( n1 "name" -- n2 )    aligned   1 cells   +field ;
: CFIELD:   ( n1 "name" -- n2 )              1 chars   +field ;
\ : FFIELD:   ( n1 "name" -- n2 )    faligned  1 floats  +field ;
\ : SFFIELD:  ( n1 "name" -- n2 )    sfaligned 1 sfloats +field ;
\ : DFFIELD:  ( n1 "name" -- n2 )    dfaligned 1 dfloats +field ;
