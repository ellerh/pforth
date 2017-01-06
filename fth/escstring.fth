\ S\"
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

\ CHAR is the first character in the string C-ADDR/U.  C-ADDR2/U2 is
\ the rest of the string.
: NEXT-CHAR ( c-addr u -- c-addr2 u2 char ) over c@ >r 1 /string r> ;

: PARSE-HEX-DIGIT ( char -- u )
    dup '0' '9' 1+ within IF '0' - EXIT THEN
    dup 'a' 'g' within IF 'a' - 10 + EXIT THEN
    dup 'A' 'G' within IF 'A' - 10 + EXIT THEN
    true abort" Invalid hex digit in string after \x."
;

: PARSE-\X ( c-addr u -- c-addr' u' )
    dup 2 u< abort" Premature end of string after \x."
    next-char parse-hex-digit >r
    next-char parse-hex-digit r> 4 lshift or c,
;

: PARSE-\ ( c-addr u -- c-addr' u' )
    dup 0= abort" Premature end of string after \."
    next-char CASE
        'a' OF 7 c, ENDOF
        'b' OF 8 c, ENDOF
        'e' OF 27 c, ENDOF
        'f' OF 12 c, ENDOF
        'l' OF 10 c, ENDOF
        'm' OF 13 c, 10 c, ENDOF
        'n' OF 10 c, ENDOF
        'q' OF 34 c, ENDOF
        'r' OF 13 c, ENDOF
        't' OF 9 c, ENDOF
        'v' OF 11 c, ENDOF
        'z' OF 0 c, ENDOF
        '"' OF 34 c, ENDOF
        'x' OF parse-\x ENDOF
        '\' OF 92 c, ENDOF
        true abort" Invalid escape character."
    ENDCASE
;

: PARSE-LOOP ( c-addr u -- c-addr' u' )
    BEGIN dup WHILE
        next-char CASE
	    '\' OF parse-\ ENDOF
	    '"' OF EXIT ENDOF
	    dup c,
        ENDCASE
    REPEAT
;

\ Parse STRING, translating \-escape characters.  Store the translated
\ string after HERE.  U is the length of the string.
\
\ The reason to store it after HERE is that it can be conveniently
\ done with C,.
: PARSE-S\" ( "string" -- u )
    here                                     ( here )
    source >in @ /string parse-loop          ( here c-addr' u' )
    drop source drop - >in !		     ( here )
    here swap -				     ( u )
    dup negate allot
;

}private

: S\" ( "string" -- c-addr u )
    here parse-s\"              ( here u )
    save-string count           ( c-addr u )
    state @
    IF postpone sliteral        (  )
    ELSE                        ( c-addr u )
    THEN
; immediate

privatize
