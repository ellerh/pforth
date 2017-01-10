\ To test some of the ANS Forth Programming Tools and extension wordset

\ This program was written by Gerry Jackson in 2006, with contributions from
\ others where indicated, and is in the public domain - it can be distributed
\ and/or modified in any way but please retain this notice.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

\ The tests are not claimed to be comprehensive or correct 

\ ------------------------------------------------------------------------------
\ Version 0.13 31 October 2015 More tests on [ELSE] and [THEN]
\              TRAVERSE-WORDLIST etc tests made conditional on the required
\              search-order words being available
\              Calls to COMPARE replaced with S= (in utilities.fth)
\         0.11 25 April Added tests for N>R NR> SYNONYM TRAVERSE-WORDLIST
\              NAME>COMPILE NAME>INTERPRET NAME>STRING
\         0.6  1 April 2012 Tests placed in the public domain.
\              Further tests on [IF] [ELSE] [THEN]
\         0.5  30 November 2009 <TRUE> and <FALSE> replaced with TRUE and FALSE
\         0.4  6 March 2009 ENDIF changed to THEN. {...} changed to T{...}T
\         0.3  20 April 2007 ANS Forth words changed to upper case
\         0.2  30 Oct 2006 updated following GForth test to avoid
\              changing stack depth during a colon definition
\         0.1  Oct 2006 First version released

\ ------------------------------------------------------------------------------
\ The tests are based on John Hayes test program

\ Words tested in this file are:
\     AHEAD [IF] [ELSE] [THEN] CS-PICK CS-ROLL [DEFINED] [UNDEFINED]
\     N>R NR> SYNONYM TRAVERSE-WORDLIST NAME>COMPILE NAME>INTERPRET
\     NAME>STRING
\     

\ Words not tested:
\     .S ? DUMP SEE WORDS
\     ;CODE ASSEMBLER BYE CODE EDITOR FORGET STATE 
\ ------------------------------------------------------------------------------
\ Assumptions, dependencies and notes:
\     - tester.fr (or ttester.fs), errorreport.fth and utilities.fth have been
\       included prior to this file
\     - the Core word set is available and tested
\     - testing TRAVERSE-WORDLIST uses WORDLIST SEARCH-WORDLIST GET-CURRENT
\       SET-CURRENT and FORTH-WORDLIST from the Search-order word set. If any
\       of these are not present these tests will be ignored
\ ------------------------------------------------------------------------------


include? }T{  t_tools.fth

true fp-require-e !

true value verbose

: testing
    verbose IF
	source >in @ /string ." TESTING: " type cr
    THEN
    source nip >in !
; immediate

: -> }T{ ;
: s= compare 0= ;
\ : $" state IF postpone s" else ['] s" execute THEN ; immediate

DECIMAL

\ ------------------------------------------------------------------------------
TESTING AHEAD

T{ : PT1 AHEAD 1111 2222 THEN 3333 ; -> }T
T{ PT1 -> 3333 }T

\ ------------------------------------------------------------------------------
TESTING [IF] [ELSE] [THEN]

T{ TRUE  [IF] 111 [ELSE] 222 [THEN] -> 111 }T
T{ FALSE [IF] 111 [ELSE] 222 [THEN] -> 222 }T

T{ TRUE  [IF] 1     \ Code spread over more than 1 line
             2
          [ELSE]
             3
             4
          [THEN] -> 1 2 }T
T{ FALSE [IF]
             1 2
          [ELSE]
             3 4
          [THEN] -> 3 4 }T

T{ TRUE  [IF] 1 TRUE  [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 1 2 }T
T{ FALSE [IF] 1 TRUE  [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 4 }T
T{ TRUE  [IF] 1 FALSE [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 1 3 }T
T{ FALSE [IF] 1 FALSE [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 4 }T

\ ------------------------------------------------------------------------------
TESTING immediacy of [IF] [ELSE] [THEN]

T{ : PT2 [  0 ] [IF] 1111 [ELSE] 2222 [THEN]  ; PT2 -> 2222 }T
T{ : PT3 [ -1 ] [IF] 3333 [ELSE] 4444 [THEN]  ; PT3 -> 3333 }T
: PT9 BL WORD FIND ;
T{ PT9 [IF]   NIP -> 1 }T
T{ PT9 [ELSE] NIP -> 1 }T
T{ PT9 [THEN] NIP -> 1 }T

\ -----------------------------------------------------------------------------
TESTING [IF] and [ELSE] carry out a text scan by parsing and discarding words
\ so that an [ELSE] or [THEN] in a comment or string is recognised

: PT10 REFILL DROP REFILL DROP ;

T{ 0  [IF]            \ Words ignored up to [ELSE] 2
      [THEN] -> 2 }T
T{ -1 [IF] 2 [ELSE] 3 $" [THEN] 4 PT10 IGNORED TO END OF LINE"
      [THEN]          \ A precaution in case [THEN] in string isn't recognised
   -> 2 4 }T

\ -----------------------------------------------------------------------------
TESTING [ELSE] and [THEN] without a preceding [IF]

\ [ELSE] ... [THEN] acts like a multi-line comment
T{ [ELSE]
11 12 13
[THEN] 14 -> 14 }T

T{ [ELSE] -1 [IF] 15 [ELSE] 16 [THEN] 17 [THEN] 18 -> 18 }T

\ A lone [THEN] is a noop
T{ 19 [THEN] 20 -> 19 20 }T

\ ------------------------------------------------------------------------------
TESTING CS-PICK and CS-ROLL

\ Test PT5 based on example in ANS document p 176.

: ?REPEAT
   0 CS-PICK POSTPONE UNTIL
; IMMEDIATE

VARIABLE PT4

T{ : PT5  ( N1 -- )
      PT4 !
      BEGIN
         -1 PT4 +!
         PT4 @ 4 > 0= ?REPEAT \ Back TO BEGIN if FALSE
         111
         PT4 @ 3 > 0= ?REPEAT
         222
         PT4 @ 2 > 0= ?REPEAT
         333
         PT4 @ 1 =
      UNTIL
; -> }T

T{ 6 PT5 -> 111 111 222 111 222 333 111 222 333 }T


T{ : ?DONE POSTPONE IF 1 CS-ROLL ; IMMEDIATE -> }T  \ Same as WHILE
T{ : PT6
      >R
      BEGIN
         R@
      ?DONE
         R@
         R> 1- >R
      REPEAT
      R> DROP
   ; -> }T

T{ 5 PT6 -> 5 4 3 2 1 }T

: MIX_UP 2 CS-ROLL ; IMMEDIATE  \ CS-ROT

: PT7    ( f3 f2 f1 -- ? )
   IF 1111 ROT ROT         ( -- 1111 f3 f2 )     ( cs: -- orig1 )
      IF 2222 SWAP         ( -- 1111 2222 f3 )   ( cs: -- orig1 orig2 )
         IF                                      ( cs: -- orig1 orig2 orig3 )
            3333 MIX_UP    ( -- 1111 2222 3333 ) ( cs: -- orig2 orig3 orig1 )
         THEN                                    ( cs: -- orig2 orig3 )
         4444        \ Hence failure of first IF comes here and falls through
      THEN                                      ( cs: -- orig2 )
      5555           \ Failure of 3rd IF comes here
   THEN                                         ( cs: -- )
   6666              \ Failure of 2nd IF comes here
;

T{ -1 -1 -1 PT7 -> 1111 2222 3333 4444 5555 6666 }T
T{  0 -1 -1 PT7 -> 1111 2222 5555 6666 }T
T{  0  0 -1 PT7 -> 1111 0    6666 }T
T{  0  0  0 PT7 -> 0    0    4444 5555 6666 }T

: [1CS-ROLL] 1 CS-ROLL ; IMMEDIATE

T{ : PT8
      >R
      AHEAD 111
      BEGIN 222 
         [1CS-ROLL]
         THEN
         333
         R> 1- >R
         R@ 0<
      UNTIL
      R> DROP
   ; -> }T

T{ 1 PT8 -> 333 222 333 }T

\ ------------------------------------------------------------------------------
TESTING [DEFINED] [UNDEFINED]

CREATE DEF1

T{ [DEFINED]   DEF1 -> TRUE  }T
T{ [UNDEFINED] DEF1 -> FALSE }T
T{ [DEFINED]   12345678901234567890 -> FALSE }T
T{ [UNDEFINED] 12345678901234567890 -> TRUE  }T
T{ : DEF2 [DEFINED]   DEF1 [IF] 1 [ELSE] 2 [THEN] ; -> }T
T{ : DEF3 [UNDEFINED] DEF1 [IF] 3 [ELSE] 4 [THEN] ; -> }T
T{ DEF2 -> 1 }T
T{ DEF3 -> 4 }T

\ ------------------------------------------------------------------------------
TESTING N>R NR>

T{ : NTR  N>R -1 NR> ; -> }T
T{ 1 2 3 4 5 6 7 4 NTR -> 1 2 3 -1 4 5 6 7 4 }T
T{ 1 0 NTR -> 1 -1 0 }T
T{ : NTR2 N>R N>R -1 NR> -2 NR> ;
T{ 1 2 2 3 4 5 3 NTR2 -> -1 1 2 2 -2 3 4 5 3 }T
T{ 1 0 0 NTR2 -> 1 -1 0 -2 0 }T

\ ------------------------------------------------------------------------------
\ nyi TESTING SYNONYM
\ nyi 
\ nyi : SYN1 1234 ;
\ nyi T{ SYNONYM NEW-SYN1 SYN1 -> }T
\ nyi T{ NEW-SYN1 -> 1234 }T
\ nyi : SYN2 2345 ; IMMEDIATE
\ nyi T{ SYNONYM NEW-SYN2 SYN2 -> }T
\ nyi T{ NEW-SYN2 -> 2345 }T
\ nyi T{ : SYN3 SYN2 LITERAL ; SYN3 -> 2345 }T

\ nyi \ ------------------------------------------------------------------------------
\ nyi \ These tests require GET-CURRENT SET-CURRENT WORDLIST from the optional
\ nyi \ Search-Order word set. If any of these are not available the tests
\ nyi \ will be ignored
\ nyi 
\ nyi [?UNDEF] WORDLIST \? [?UNDEF] GET-CURRENT \? [?UNDEF] SET-CURRENT
\ nyi \? [?UNDEF] FORTH-WORDLIST
\ nyi 
\ nyi \? TESTING TRAVERSE-WORDLIST NAME>COMPILE NAME>INTERPRET NAME>STRING
\ nyi 
\ nyi \? GET-CURRENT CONSTANT CURR-WL
\ nyi \? WORDLIST CONSTANT TRAV-WL
\ nyi \? : WDCT ( n nt -- n+1 f ) DROP 1+ TRUE ;
\ nyi \? T{ 0 ' WDCT TRAV-WL TRAVERSE-WORDLIST -> 0 }T
\ nyi 
\ nyi \? TRAV-WL SET-CURRENT
\ nyi \? : TRAV1 1 ;
\ nyi \? T{ 0 ' WDCT TRAV-WL TRAVERSE-WORDLIST -> 1 }T
\ nyi \? : TRAV2 2 ; : TRAV3 3 ; : TRAV4 4 ; : TRAV5 5 ; : TRAV6 6 ; IMMEDIATE
\ nyi \? CURR-WL SET-CURRENT
\ nyi \? T{ 0 ' WDCT TRAV-WL TRAVERSE-WORDLIST -> 6 }T  \ Traverse whole wordlist
\ nyi 
\ nyi \ Terminate TRAVERSE-WORDLIST after n words & check it compiles
\ nyi \? : (PART-OF-WL)  ( ct n nt -- ct+1 n-1 )
\ nyi \?    DROP DUP IF SWAP 1+ SWAP 1- THEN DUP
\ nyi \? ;
\ nyi \? : PART-OF-WL  ( n -- ct 0 | ct+1 n-1)
\ nyi \?    0 SWAP ['] (PART-OF-WL) TRAV-WL TRAVERSE-WORDLIST DROP
\ nyi \? ;
\ nyi \? T{ 0 PART-OF-WL -> 0 }T
\ nyi \? T{ 1 PART-OF-WL -> 1 }T
\ nyi \? T{ 4 PART-OF-WL -> 4 }T
\ nyi \? T{ 9 PART-OF-WL -> 6 }T  \ Traverse whole wordlist
\ nyi 
\ nyi \ Testing NAME>.. words require a name token. It will be easier to test them
\ nyi \ if there is a way of obtaining the name token of a given word. To get this we
\ nyi \ need a definition to compare a given name with the result of NAME>STRING.
\ nyi \ The output from NAME>STRING has to be copied into a buffer and converted to a
\ nyi \ known case as different Forth systems may store names as lower, upper or
\ nyi \ mixed case.
\ nyi 
\ nyi \? CREATE UCBUF 32 CHARS ALLOT    \ The buffer
\ nyi 
\ nyi \ Convert string to upper case and save in the buffer.
\ nyi 
\ nyi \? : >UPPERCASE  ( caddr u  -- caddr2 u2 )
\ nyi \?    32 MIN DUP >R UCBUF DUP 2SWAP
\ nyi \?    OVER + SWAP 2DUP U>
\ nyi \?    IF
\ nyi \?       DO          \ ?DO can't be used, as it is a Core Extension word
\ nyi \?          I C@ DUP [CHAR] a [CHAR] z 1+ WITHIN IF 32 INVERT AND THEN
\ nyi \?          OVER C! CHAR+
\ nyi \?       LOOP
\ nyi \?    ELSE
\ nyi \?       2DROP
\ nyi \?    THEN
\ nyi \?    DROP R>
\ nyi \? ;
\ nyi 
\ nyi \ Compare string (caddr u) with name associated with nt
\ nyi \? : NAME?  ( caddr u nt -- caddr u f )   \ f = true for name = (caddr u) string
\ nyi \?    NAME>STRING >UPPERCASE 2OVER S=
\ nyi \? ;
\ nyi 
\ nyi \ The word to be executed by TRAVERSE-WORDLIST
\ nyi \? : GET-NT  ( caddr u 0 nt -- caddr u nt false | caddr u 0 nt ) \ nt <> 0
\ nyi \?    2>R R@ NAME? IF R> R> ELSE 2R> THEN
\ nyi \? ;
\ nyi 
\ nyi \ Get name token of (caddr u) in wordlist wid, return 0 if not present
\ nyi \? : GET-NAME-TOKEN  ( caddr u wid -- nt | 0 )
\ nyi \?    0 ['] GET-NT ROT TRAVERSE-WORDLIST >R 2DROP R>
\ nyi \? ;
\ nyi 
\ nyi \ Test NAME>STRING via TRAVERSE-WORDLIST
\ nyi \? T{ $" ABCDE" TRAV-WL GET-NAME-TOKEN 0= -> TRUE  }T \ Not in wordlist
\ nyi \? T{ $" TRAV4" TRAV-WL GET-NAME-TOKEN 0= -> FALSE }T
\ nyi 
\ nyi \ Test NAME>INTERPRET on a word with interpretation semantics
\ nyi \? T{ $" TRAV3" TRAV-WL GET-NAME-TOKEN NAME>INTERPRET EXECUTE -> 3 }T
\ nyi 
\ nyi \ Test NAME>INTERPRET on a word without interpretation semantics. It is
\ nyi \ difficult to choose a suitable word because:
\ nyi \    - a user cannot define one in a standard system
\ nyi \    - a Forth system may choose to define interpretation semantics for a word
\ nyi \      despite the standard stating they are undefined. If so the behaviour
\ nyi \      cannot be tested as it is 'undefined' by the standard.
\ nyi \ (October 2016) At least one major system, GForth, has defined behaviour for
\ nyi \ all words with undefined interpretation semantics. It is not possible in
\ nyi \ standard Forth to define a word without interpretation semantics, therefore
\ nyi \ it is not possible to have a general test for NAME>INTERPRET returning 0.
\ nyi \ So the following word TIF executes NAME>INTERPRET for all words with
\ nyi \ undefined interpretation semantics in the Core word set, the first one to
\ nyi \ return 0 causes the rest to be skipped. If none return 0 a message is
\ nyi \ displayed to that effect. No system can fail this test!
\ nyi 
\ nyi \? VARIABLE TIF-SKIP
\ nyi \? : TIF  ( "name1 ... namen" -- )  \ TIF = TEST-INTERPRETATION-UNDEFINED
\ nyi \?    BEGIN
\ nyi \?       TIF-SKIP @ IF SOURCE >IN ! DROP EXIT THEN
\ nyi \?       BL WORD COUNT DUP 0= IF 2DROP EXIT THEN  \ End of line
\ nyi \?       FORTH-WORDLIST GET-NAME-TOKEN ?DUP ( -- nt nt | 0 0 )
\ nyi \?       IF
\ nyi \?          NAME>INTERPRET 0= TIF-SKIP ! \ Returning 0 skips further tests
\ nyi \?       THEN
\ nyi \?       0      \ AGAIN is a Core Ext word
\ nyi \?    UNTIL
\ nyi \? ;
\ nyi 
\ nyi \? : TIF?  ( -- )
\ nyi \?    TIF-SKIP @ 0=
\ nyi \?    IF
\ nyi \?       CR ." NAME>INTERPRET returns an execution token for all" CR
\ nyi \?       ." core words with undefined interpretation semantics." CR
\ nyi \?       ." So NAME>INTERPRET returning 0 is untested." CR
\ nyi \?    THEN
\ nyi \? ;
\ nyi 
\ nyi \? 0 TIF-SKIP !
\ nyi \? TIF DUP SWAP DROP
\ nyi \? TIF >R R> R@ ." ; EXIT ['] [CHAR] RECURSE ABORT" DOES> LITERAL POSTPONE
\ nyi \? TIF DO I J LOOP +LOOP UNLOOP LEAVE IF ELSE THEN BEGIN WHILE REPEAT UNTIL
\ nyi \? TIF? 
\ nyi 
\ nyi \ Test NAME>COMPILE
\ nyi \? : N>C  ( caddr u -- )  TRAV-WL GET-NAME-TOKEN NAME>COMPILE EXECUTE ; IMMEDIATE
\ nyi \? T{ : N>C1  ( -- n )  [ $" TRAV2" ] N>C ; N>C1 -> 2 }T          \ Not immediate
\ nyi \? T{ : N>C2  ( -- n )  [ $" TRAV6" ] N>C LITERAL ; N>C2 -> 6 }T  \ Immediate word
\ nyi \? T{ $" TRAV6" TRAV-WL GET-NAME-TOKEN NAME>COMPILE EXECUTE -> 6 }T
\ nyi 
\ nyi \ Test the order of finding words with the same name
\ nyi \? TRAV-WL SET-CURRENT
\ nyi \? : TRAV3 33 ; : TRAV3 333 ; : TRAV7 7 ; : TRAV3 3333 ;
\ nyi \? CURR-WL SET-CURRENT
\ nyi 
\ nyi \? : (GET-ALL)  ( caddr u nt -- [n] caddr u true )
\ nyi \?    DUP >R NAME? IF R@ NAME>INTERPRET EXECUTE ROT ROT THEN
\ nyi \?    R> DROP TRUE
\ nyi \? ; 
\ nyi 
\ nyi \? : GET-ALL  ( caddr u -- i*x )
\ nyi \?    ['] (GET-ALL) TRAV-WL TRAVERSE-WORDLIST 2DROP
\ nyi \? ;
\ nyi 
\ nyi \? T{ $" TRAV3" GET-ALL -> 3333 333 33 3 }T
\ nyi [?ELSE]
\ nyi \? CR CR
\ nyi \? .( Some search-order words not present - TRAVERSE-WORDLIST etc not tested) CR
\ nyi [?THEN]

\ ------------------------------------------------------------------------------

\ TOOLS-ERRORS SET-ERROR-COUNT

\ CR .( End of Programming Tools word tests) CR

}test
