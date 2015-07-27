parser grammar DelphiParser;

options{
	tokenVocab=DelphiLexer;
}

@header {
package jp.co.rn.experiment.antlr.impl.delphi;
}

goal
	:	program
	|	delphiPackage
	|	library
	|	unit
	;
program		
	:	(PROGRAM identifier (LPAREN identList RPAREN)? SEMI)* programBlock DOT;
unit		
	:	UNIT identifier (portabilityDirective)? SEMI interfaceSection implementationSection initSection DOT;
delphiPackage		
	:	PACKAGE identifier SEMI (requiresClause)? (containsClause)? END DOT
	;
library		
	:	LIBRARY identifier SEMI programBlock DOT
	;
programBlock	
	:	(usesClause)? block
	;
usesClause	
	:	USES identList SEMI
	;
portabilityDirective 	
	:	PLATFORM			
	|	DEPERECATED
	|	LIBRARY
	;
interfaceSection	
	:	INTERFACE (usesClause)? (interfaceDecl)*;
interfaceDecl		
	:	constSection
	|	typeSection
	|	varSection
	|	methodHeading
	;
implementationSection	
	:	IMPLEMENTATION (usesClause)? (implementationDecl)*
	;
block 			
	:	BEGIN stmtList? END
	|	assemblerStatement
	;
exportsStmt		
	:	EXPORTS exportsItem (',' exportsItem)*
	;
exportSpecifier
	:	(INDEX | NAME)? expression
	;
exportsItem		
	:	identifier exportSpecifier*
	;
implementationDecl	
	:	labelDeclSection
	|	constSection
	|	typeSection
	|	varSection
	|	methodHeading
	|	methodImplementation
	|	exportsStmt
	;
labelDeclSection
	:	LABEL (labelId SEMI?)+ SEMI;
constSection
	:	(CONST|RESOURCESTRING) (constantDecl)+;
constantDecl
	:	identifier (COLON type)? EQUAL typedConstant portabilityDirective* SEMI
	;
typeSection
	:	TYPE (typeDecl)+
	;
typeDecl
	:	(identifier EQUAL TYPE? type portabilityDirective? SEMI)
	|	(identifier EQUAL CLASS (LPAREN qualId RPAREN)? SEMI)
	|	(identifier EQUAL DISPINTERFACE SEMI)
	|	(identifier EQUAL INTERFACE (LPAREN qualId RPAREN)? SEMI)
	;
typedConstant
	:	expression
	|	LPAREN (qualId COLON typedConstant SEMI?)+ RPAREN
	|	LPAREN (typedConstant SEMI?)+ RPAREN
	|	LPAREN (typedConstant COMMA?)+ RPAREN
	|	LPAREN RPAREN
	;
type	
	:	enumeratedType
	|	expressionOrRange
	|	arrayType
	|	setType
	|	fileType
	|	recordType
	|	pointerType
	|	procedureType
	|	classOfType
	|	classType
	|	interfaceType
	|	packedType
	|	stringType
	|	simpleType
	|	variantType
	;
recordType
	:	RECORD visibilitySection* variantSection? END
	;
classOfType
	:	CLASS OF qualId
	;
classType
	:	CLASS (LPAREN (qualId COMMA?)+ RPAREN)? visibilitySection* END
	;
packedType
	:	PACKED type
	;
visibilitySection
	:	visibility visibilitySectionContent*
	|	visibilitySectionContent+
	;
visibilitySectionContent
	:	fieldSection
	|	methodOrProperty
	|	constSection
	|	typeSection
	;
visibility
	:	PRIVATE
	|	PROTECTED
	|	PUBLIC
	|	PUBLISHED
	;
fieldSection
	:	(CLASS? VAR)? fieldDecl
	;
methodOrProperty
	:	methodHeading
	|	property
	;
simpleType
	:	ordinalType
	|	realType
	;	
realType
	:	REAL48
	|	REAL
	|	SINGLE
	|	DOUBLE
	|	EXTENDED
	|	CURRENCY
	|	COMP
	;
ordinalType
	:	subrangeType
	|	enumeratedType
	|	ordIdent
	;
ordIdent
	:	SHORTINT
	|	SMALLINT
	|	INTEGER
	|	BYTE
	|	LONGINT
	|	INT64
	|	WORD
	|	BOOLEAN
	|	CHAR
	|	WIDECHAR
	|	LONGWORD
	|	PCHAR
	;
variantType
	:	VARIANT
	|	OLEVARIANT
	;
subrangeType
	:	constExpr DOTDOT constExpr
	;
enumeratedType
	:	LPAREN (enumeratedTypeElement COMMA?)+ RPAREN
	;
enumeratedTypeElement
	:	identifier (EQUAL constExpr)?
	;
stringType
	:	STRING (LBRACK constExpr RBRACK)?
	|	ANSISTRING
	|	WIDESTRING
	;
arrayType
	:	ARRAY (LBRACK (type SEMI?)+ RBRACK)? OF type
	;
fieldDecl
	:	identList COLON type portabilityDirective? SEMI?
	;
variantSection
	:	CASE (identifier COLON)? qualId OF variantGroup+
	;
variantGroup
	:	exprList COLON LPAREN fieldDecl* variantSection? RPAREN SEMI?
	;
setType
	:	SET OF type
	;
fileType
	:	FILE OF qualId
	;
pointerType
	:	POINTER type
	;
procedureType
	:	(PROCEDURE | FUNCTION) (LPAREN (parameter SEMI?)* RPAREN)? (COLON methodReturnType)? directive* (OF OBJECT)? directive*
	;
methodReturnType
	:	type
	;
varSection
	:	VAR (varDecl|methodImplementation)+;
varDecl	
	:	identList COLON type (ABSOLUTE expression | EQUAL typedConstant)? portabilityDirective* SEMI
	;
expression
	:	simpleExpression (relOp simpleExpression)*
	;
simpleExpression
	:	(PLUS|MINUS)? term (addOp term)*
	;
term	
	:	factor (mulOp factor)*
	;
factor
	:	atom
	|	unaryOperator factor
	;
unaryOperator
	:	NOT
	|	PLUS
	|	MINUS
	|	ATMARK
	|	INHERITED
	;
atom
	:	particle 
		(
				DOT identifier
			|	LBRACK exprList RBRACK
			|	POINTER
			|	LPAREN exprList? RPAREN
		)*
	;

particle
	:	number
	|	stringliteral
	|	identifier
	|	NIL
	|	parenthesizedExpression
	|	setLiteral
	|	stringType
	|	FILE
//	|	simpleType // TODO
	|	ASCII
	;

parenthesizedExpression
	:	LPAREN expression RPAREN
	;

setLiteral
	:	LBRACK expressionOrRangeList? RBRACK
	;

expressionOrRangeList
	:	(expressionOrRange COMMA?)+
	;

expressionOrRange
	:	(simpleExpression (DOTDOT simpleExpression)?)
	;
relOp	
	:	GT
	|	LT
	|	GE
	|	LE
	|	EQUAL
	|	NOT_EQUAL
	|	IN
	|	IS
	|	AS
	;
addOp	
	:	PLUS
	|	MINUS
	|	OR
	|	XOR
	;
mulOp	
	:	STAR
	|	SLASH
	|	DIV
	|	MOD
	|	AND
	|	SHL
	|	SHR
	;
setConstructor
	:	LBRACK (setElement (COMMA setElement)*)? RBRACK;
setElement
	:	expression (DOTDOT expression)?;
exprList
	:	expression (COMMA expression)*;
statement
	:	labelId COLON simpleStatement?
	|	simpleStatement
	;
stmtList
	:	(statement|SEMI)+;
simpleStatement
	:	bareInherited
	|	expressionOrAssignment
	|	gotoStmt
	|	block
	|	ifStmt
	|	caseStmt
	|	repeatStmt
	|	whileStmt
	|	forStmt
	|	withStmt
	|	tryStmt
	|	raiseStmt
	;
bareInherited
	:	INHERITED
	;
expressionOrAssignment
	:	expression
	|	expression ASSIGN expression
	;
gotoStmt
	:	GOTO labelId
	;
ifStmt	
	:	IF expression THEN statement? (ELSE statement?)?
	;
caseStmt
	:	CASE expression OF caseSelector+ (ELSE stmtList?)? END
	;
caseSelector
	:	(expressionOrRange COMMA?)+ COLON statement? SEMI? 
	;
	
caseLabel
	:	constExpr (DOTDOT constExpr)?;
loopStmt
	:	repeatStmt
	|	whileStmt
	|	forStmt;
repeatStmt
	:	REPEAT stmtList? UNTIL expression;
whileStmt
	:	WHILE expression DO statement;
forStmt	
	:	FOR identifier ASSIGN expression (TO|DOWNTO) expression DO statement;
withStmt
	:	WITH exprList DO statement;
tryStmt
	:	tryExceptStmt|tryFinallyStmt
	;
tryExceptStmt
	:	TRY stmtList? EXCEPT (stmtList?|exceptionItem*) (ELSE stmtList?)? END
	;
tryFinallyStmt
	:	TRY stmtList? FINALLY stmtList? END
	;
exceptionItem
	:	ON (identifier COLON)? qualId DO statement? SEMI?
	;
raiseStmt
	:	RAISE expression?;
assemblerStatement
	:	ASM .* END
	;
methodImplementation
	:	methodHeading fancyBlock SEMI
	;
fancyBlock
	:	implementationDecl* block
	;
methodHeading
	:	CLASS? methodType qualId
		(
			(LPAREN (parameter SEMI?)* RPAREN)? (COLON methodReturnType)? directive* SEMI?
			|EQUAL identifier SEMI?
		) 
	|	PROCEDURE REGISTER SEMI?
	;
methodType
	:	PROCEDURE|FUNCTION|CONSTRUCTOR|DESTRUCTOR|OPERATOR
	;
parameter
	:	(VAR | CONST | OUT)? identList (COLON parameterType)? (EQUAL expression)?
	;
parameterType
	:	qualId
	|	STRING
	|	FILE
	|	openArray
	|	type
	;
openArray
	:	ARRAY OF (qualId|STRING|FILE|CONST)
	;
directive
	:	SEMI? CDECL
	|	SEMI? REGISTER
	|	SEMI? DISPID expression
	|	SEMI? DYNAMIC
	|	SEMI? VIRTUAL
	|	SEMI? EXPORT
	|	SEMI? EXTERNAL (expression exportSpecifier*)?
	|	SEMI? NEAR
	|	SEMI? FAR
	|	SEMI? FORWARD
	|	SEMI? MESSAGE expression
	|	SEMI? OVERRIDE
	|	SEMI? OVERLOAD
	|	SEMI? PASCAL
	|	SEMI? REINTRODUCE
	|	SEMI? SAFECALL
	|	SEMI? STDCALL
	|	SEMI? VARARGS
	|	SEMI? LOCAL
	|	SEMI? ABSTRACT
	|	SEMI? ASSEMBLER
	;
initSection
	:	INITIALIZATION stmtList? (FINALIZATION stmtList?)? END
	|	BEGIN stmtList END
	|	END
	;
property
	:	CLASS? PROPERTY identifier (LBRACK (parameter SEMI?)+ RBRACK)? (COLON methodReturnType)? propertyDirective* SEMI;
propertyDirective
	:	SEMI DEFAULT
	|	DEFAULT expression
	|	DISPID expression
	|	IMPLEMENTS (qualId SEMI?)+
	|	INDEX expression
	|	NODEFAULT
	|	READ expression
	|	READONLY
	|	STORED expression
	|	WRITE expression
	|	WRITEONLY
	;
interfaceType
	:	(INTERFACE|DISPINTERFACE) (LPAREN qualId RPAREN)? (LBRACK expression RBRACK)? methodOrProperty* END
	;
requiresClause
	:	REQUIRES identList SEMI;
containsClause 
	:	CONTAINS identList SEMI;
identList 
	:	(identifier COMMA?)+;

qualId	
	:	(identifier DOT)* extendIdentifier
	;
identifier
	:	IDENT
	|	directiveKeywords
	|	propertyDirectiveKeywords
	|	realType
	|	ordIdent
	|	stringType
	;
extendIdentifier
	:	identifier
	;
constExpr
	:	(PLUS|MINUS)? number
	|	stringliteral
	|	identifier
	|	NIL
	;
labelId
	:	identifier;
number	
	:	NUM_INT
	|	HEX
	;
stringliteral
	:	STRING_LITERAL
	;
	
directiveKeywords
	:	ABSOLUTE
	|	EXPORT
	|	NAME
	|	STDCALL
	|	ABSTRACT
	|	EXTERNAL
	|	NEAR
	|	STRICT
	|	ASSEMBLER
	|	FAR
	|	FINAL
	|	OPERATOR
	|	UNSAFE
	|	CDECL
	|	FORWARD
	|	OUT
	|	REFERENCE
	|	VARARGS
	|	CONTAINS
	|	HELPER
	|	OVERLOAD
	|	REGISTER
	|	VIRTUAL
	|	OVERRIDE
	|	REINTRODUCE
	|	WINAPI
	|	DELAYED
	|	PACKAGE
	|	REQUIRES
	|	DEPRECATED
	|	PASCAL
	|	RESIDENT
	|	PLATFORM
	|	SAFECALL
	|	DYNAMIC
	|	LOCAL
	|	SEALED
	|	EXPERIMENTAL
	|	MESSAGE
	|	STATIC
	|	DISPID
	;

propertyDirectiveKeywords
	:	DEFAULT
	|	DISPID
	|	IMPLEMENTS
	|	INDEX
	|	NODEFAULT
	|	READ
	|	READONLY
	|	STORED
	|	WRITE
	|	WRITEONLY
	;