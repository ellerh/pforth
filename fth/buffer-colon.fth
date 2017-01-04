\ Implementation of BUFFER: based on ALLOCATE and AUTO.INIT.
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

private{

\ The buffer struct keeps administrative information about a buffer.
0
1 cells +field BUFFER-DATA  \ Pointer to the dynamically allocated memory
1 cells +field BUFFER-SIZE  \ Size of the buffer in bytes
1 cells +field BUFFER-NEXT  \ Relocatable address of next buffer (or 0)
constant /BUFFER  \ Size of the buffer struct

\ The list of all buffers.  Either null (= empty list) or the
\ relocatable address of the first buffer.  The address of the next
\ element of the list is stored in the BUFFER-NEXT field.
0 value BUFFER-LIST

\ Add the buffer BUFFER to BUFFER-LIST
: ADD-BUFFER ( buffer -- )
    buffer-list over buffer-next ! ( buffer )
    use->rel to buffer-list
;

\ Allocate memory and store the address in the BUFFER-DATA field
: BUFFER-ALLOC ( buffer -- )
    dup buffer-size @ allocate throw ( buffer mem )
    swap buffer-data !
;

: BUFFER-FREE ( buffer -- ) buffer-data @ free throw ;

\ Call XT for each buffer in BUFFER-LIST.
: FOR-EACH-BUFFER ( xt -- )
    { xt }
    buffer-list                 ( reloc )
    BEGIN dup WHILE
        rel->use                ( buffer )
        dup >r xt execute r>    ( buffer )
        buffer-next @
    REPEAT
    drop
;

}private

: BUFFER: ( "name" u -- )
    CREATE
        here /buffer allot      ( u buffer )
        tuck buffer-size !      ( buffer )
        dup buffer-alloc        ( buffer )
        add-buffer
    DOES>                       ( buffer )
        buffer-data @           ( a-addr )
;

\ Allocate memory for all buffers
: AUTO.INIT ( -- )
    auto.init
    ['] buffer-alloc for-each-buffer
;

\ Free allocated memory. (Unnecessary?)
: AUTO.TERM ( -- )
    ['] buffer-free for-each-buffer
    auto.term
;

privatize
