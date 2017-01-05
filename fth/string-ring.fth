\ A ring of buffers to temporarily store S" and friends
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

anew TASK-STRING-RING.FTH

private{

\ Forth 2012 requires at least two buffers of at least 80 characters.
2 constant RING-SIZE

\ An array of RING-SIZE pointers to counted strings.  The strings are
\ dynamically allocated and freed when overwritten.  Initially all
\ pointers must be initialized to null.
ring-size cells buffer: RING-DATA

\ Index in RING-DATA where the next pointer should be stored
0 value RING-INDEX

\ The next index (wraps around)
: NEXT-INDEX ( -- idx ) ring-index 1+ ring-size mod ;

\ ADDR is the address in RING-DATA which corresponds to the current index
: ITEM ( -- addr ) ring-data ring-index cells + ;

\ Copy the sring C-ADDR/U to freshly allocated memory as counted
\ string.
: MAKE-COUNTED-STRING ( c-addr u -- $string )
    dup 255 u> abort" String too long"
    dup 1+ allocate throw       ( c-addr u $string )
    >r r@ place r>              ( $string )
;

: MAYBE-FREE ( addr -- ) ?dup IF free throw THEN ;

\ Store string C-ADDR/U as counted string at the next index.  $STRING
\ is the address of the string.
: ENQUEUE-STRING ( c-addr u -- $string )
    item @ maybe-free                               ( c-addr u )
    make-counted-string                             ( $string )
    dup item !                                      ( $string )
    next-index to ring-index                        ( $string )
;

' enqueue-string is save-string

: INIT ( -- ) ring-data ring-size cells 0 fill ;
init

: CLEANUP ( -- )
    ring-size 0 ?DO
        ring-data i cells + @
        maybe-free
    LOOP
;

}private

: AUTO.INIT ( -- ) auto.init init ;
: AUTO.TERM ( -- ) cleanup auto.term ;

privatize
