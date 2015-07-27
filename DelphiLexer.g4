lexer grammar DelphiLexer;

options{
}

@header {
package jp.co.rn.experiment.antlr.impl.delphi;
}
// KEYWORD
AND				:	A N D;
END				:	E N D; 
INTERFACE		:	I N T E R F A C E;
RECORD			:	R E C O R D; 
VAR				:	V A R; 
ARRAY			:	A R R A Y; 
EXCEPT			:	E X C E P T; 
IS				:	I S; 
REPEAT			:	R E P E A T; 
WHILE  			:	W H I L E; 
AS				:	A S; 
EXPORTS			:	E X P O R T S;
LABEL			:	L A B E L; 
RESOURCESTRING  :	R E S O U R C E S T R I N G;
WITH        	:	W I T H; 
ASM				:	A S M; 
FILE			:	F I L E; 
LIBRARY			:	L I B R A R Y;
SET    			:	S E T; 
XOR				:	X O R; 
BEGIN			:	B E G I N; 
FINALIZATION  	:	F I N A L I Z A T I O N;
MOD				:	M O D; 
SHL				:	S H L; 
CASE			:	C A S E; 
FINALLY			:	F I N A L L Y;
NIL				:	N I L; 
SHR				:	S H R; 
CLASS			:	C L A S S; 
FOR				:	F O R; 
NOT				:	N O T; 
STRING			:	S T R I N G; 
CONST			:	C O N S T; 
FUNCTION		:	F U N C T I O N;
OBJECT			:	O B J E C T; 
THEN			:	T H E N; 
CONSTRUCTOR		:	C O N S T R U C T O R;
GOTO			:	G O T O; 
OF				:	O F; 
THREADVAR		:	T H R E A D V A R;
DESTRUCTOR		:	D E S T R U C T O R;
IF				:	I F; 
OR				:	O R; 
TO				:	T O; 
DISPINTERFACE   :	D I S P I N T E R F A C E;
IMPLEMENTATION  :	I M P L E M E N T A T I O N;
PACKED			:	P A C K E D; 
TRY				:	T R Y; 
DIV				:	D I V; 
IN				:	I N; 
PROCEDURE    	:	P R O C E D U R E;
TYPE			:	T Y P E; 
DO				:	D O; 
INHERITED		:	I N H E R I T E D;
PROGRAM			:	P R O G R A M;
UNIT			:	U N I T; 
DOWNTO			:	D O W N T O; 
INITIALIZATION  :	I N I T I A L I Z A T I O N;
PROPERTY		:	P R O P E R T Y;
UNTIL			:	U N T I L; 
ELSE			:	E L S E; 
INLINE			:	I N L I N E; 
RAISE			:	R A I S E; 
USES			:	U S E S; 
DEPERECATED		:	D E P E R E C A T E D;

AT				:	A T; 
ON				:	O N; 
	
// KEYWORD IN CLASS DECL	
PUBLIC			:	P U B L I C; 
PRIVATE			:	P R I V A T E;
PROTECTED		:	P R O T E C T E D;
PUBLISHED		:	P U B L I S H E D;
AUTOMATED		:	A U T O M A T E D;

//----------------------------------------------------------------------------
// OPERATORS
//----------------------------------------------------------------------------
PLUS            :	'+'   ;
MINUS           :	'-'   ;
STAR            :	'*'   ;
SLASH           :	'/'   ;
ASSIGN          :	':='  ;
COMMA           :	','   ;
SEMI            :	';' ;
COLON           :	':'   ;
EQUAL           : 	'='   ;
NOT_EQUAL       :	'<>'  ;
LT              : 	'<'   ;
LE              :	'<='  ;
GE              :	'>='  ;
GT              :	'>'   ;
LPAREN          :	'('   ;
RPAREN          :	')'   ;
LBRACK          :	'['   ; // line_tab[line]
LBRACK2         :	'(.'  ; // line_tab(.line.)
RBRACK          :	']'   ;
RBRACK2         :	'.)'  ;
POINTER         :	'^'   ;
ATMARK          :	'@'   ;
DOT             : 	'.' ;// ('.' {$setType(DOTDOT);})?  ;
DOTDOT          :	 '..' ;

LCURLY          : 	'{' ;
RCURLY          : 	'}' ;
 
// Types
WORD			:	W O R D ;
LONGWORD		:	L O N G W O R D ;
CHAR			:	C H A R;
WIDECHAR		:	W I D E C H A R;
PCHAR			:	P C H A R;
ANSISTRING		:	A N S I S T R I N G;
WIDESTRING		:	W I D E S T R I N G;

VARIANT			:	V A R I A N T;
OLEVARIANT		:	O L E V A R I A N T;

// NUMBERTYPE
INTEGER			:	I N T E G E R;
SHORTINT		:	S H O R T I N T;
SMALLINT		:	S M A L L I N T;
LONGINT			:	L O N G I N T;
INT64			:	I N T '64';
REAL			:	R E A L;
REAL48			:	R E A L '48';
SINGLE			:	S I N G L E;
DOUBLE			:	D O U B L E;
EXTENDED		:	E X T E N D E D;
CURRENCY		:	C U R R E N C Y;
COMP			:	C O M P;

BOOLEAN			:	B O O L E A N;
BYTE			:	B Y T E;

STRING_LITERAL
	: '\'' ('\'\'' | ~('\''))* '\''
	;
	
ASCII
	:	('#' (NUM_INT|HEX)+)+
	|	ESCAPED_SEQUENCE
	;
NUM_INT
	:	('0'..'9')+(('.'('0'..'9')+ (EXPONENT)?)?|EXPONENT)
	;
HEX
	:	'$' ('0'..'9'|'a'..'f'|'A'..'F')*  	
	;

// DIRECTIVE
ABSOLUTE		:	A B S O L U T E; 
EXPORT			:	E X P O R T;  
NAME			:	N A M E;   
STDCALL			:	S T D C A L L; 
ABSTRACT		:	A B S T R A C T; 
EXTERNAL		:	E X T E R N A L; 
NEAR			:	N E A R;   
STRICT			:	S T R I C T;  
ASSEMBLER		:	A S S E M B L E R; 
FAR				:	F A R;   
FINAL			:	F I N A L;   
OPERATOR		:	O P E R A T O R; 
UNSAFE			:	U N S A F E;  
CDECL			:	C D E C L;   
FORWARD			:	F O R W A R D; 
OUT				:	O U T;   
REFERENCE		:	R E F E R E N C E; 
VARARGS			:	V A R A R G S; 
CONTAINS		:	C O N T A I N S; 
HELPER			:	H E L P E R;  
OVERLOAD		:	O V E R L O A D; 
REGISTER		:	R E G I S T E R; 
VIRTUAL			:	V I R T U A L; 
OVERRIDE		:	O V E R R I D E; 
REINTRODUCE		:	R E I N T R O D U C E; 
WINAPI			:	W I N A P I;  
DELAYED			:	D E L A Y E D; 
PACKAGE			:	P A C K A G E; 
REQUIRES		:	R E Q U I R E S; 
DEPRECATED		:	D E P R E C A T E D; 
PASCAL			:	P A S C A L;  
RESIDENT		:	R E S I D E N T; 
PLATFORM		:	P L A T F O R M;
SAFECALL		:	S A F E C A L L; 
DYNAMIC			:	D Y N A M I C; 
LOCAL			:	L O C A L;   
SEALED			:	S E A L E D;  
EXPERIMENTAL	:	E X P E R I M E N T A L; 
MESSAGE			:	M E S S A G E; 
STATIC			:	S T A T I C;
DISPID			:	D I S P I D;

// PROPERTY DIRECTIVE
DEFAULT			:	D E F A U L T; 
IMPLEMENTS		:	I M P L E M E N T S; 
INDEX			:	I N D E X;   
NODEFAULT		:	N O D E F A U L T; 
READ			:	R E A D;   
READONLY		:	R E A D O N L Y; 
STORED			:	S T O R E D;  
WRITE			:	W R I T E;   
WRITEONLY		:	W R I T E O N L Y;

COMMENT
    :   ( '//' ~[\r\n]* '\r'? '\n'
        | '/*' .*? '*/'
        | '(*' .*? '*)'
        | LCURLY .*? RCURLY
        ) -> channel(HIDDEN)//{$channel=HIDDEN;}
    ;

WS  :   ( ' '
        | '\t'
        | '\r'
        | '\n'
        ) -> channel(HIDDEN)//{$channel=HIDDEN;}
    ;

IDENT	:	('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*
    	;

fragment
EXPONENT
	:	('e') ('+'|'-')? ('0'..'9')+
	;

fragment
ESCAPED_SEQUENCE
	:	'^H'//('A'..'Z')
	;

fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');