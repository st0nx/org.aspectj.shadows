-- AspectJ Extension Modified for AspectJ-1.1 grammar

--main options
%options ACTION, AN=JavaAction.java, GP=java, 
%options FILE-PREFIX=java, ESCAPE=$, PREFIX=TokenName, OUTPUT-SIZE=125 ,
%options NOGOTO-DEFAULT, SINGLE-PRODUCTIONS, LALR=1 , TABLE, 

--error recovering options.....
%options ERROR_MAPS 

--grammar understanding options
%options first follow
%options TRACE=FULL ,
%options VERBOSE

%options DEFERRED
%options NAMES=MAX
%options SCOPES

--Usefull macros helping reading/writing semantic actions
$Define 
$putCase 
/.    case $rule_number : // System.out.println("$rule_text");  //$NON-NLS-1$
		   ./

$break
/. 
			break ;
./

$readableName 
/.$rule_number=./

-- here it starts really ------------------------------------------
$Terminals

	Identifier

	abstract assert boolean break byte case catch char class 
	continue default do double else extends false final finally float
	for if implements import instanceof int
	interface long native new null package private
	protected public return short static strictfp super switch
	synchronized this throw throws transient true try void
	volatile while

-- AspectJ Extension
	aspect pointcut around before after declare privileged
-- End AspectJ Extension

	IntegerLiteral
	LongLiteral
	FloatingPointLiteral
	DoubleLiteral
	CharacterLiteral
	StringLiteral

	PLUS_PLUS
	MINUS_MINUS
	EQUAL_EQUAL
	LESS_EQUAL
	GREATER_EQUAL
	NOT_EQUAL
	LEFT_SHIFT
	RIGHT_SHIFT
	UNSIGNED_RIGHT_SHIFT
	PLUS_EQUAL
	MINUS_EQUAL
	MULTIPLY_EQUAL
	DIVIDE_EQUAL
	AND_EQUAL
	OR_EQUAL
	XOR_EQUAL
	REMAINDER_EQUAL
	LEFT_SHIFT_EQUAL
	RIGHT_SHIFT_EQUAL
	UNSIGNED_RIGHT_SHIFT_EQUAL
	OR_OR
	AND_AND
	PLUS
	MINUS
	NOT
	REMAINDER
	XOR
	AND
	MULTIPLY
	OR
	TWIDDLE
	DIVIDE
	GREATER
	LESS
	LPAREN
	RPAREN
	LBRACE
	RBRACE
	LBRACKET
	RBRACKET
	SEMICOLON
	QUESTION
	COLON
	COMMA
	DOT
	EQUAL

--    BodyMarker

$Alias

	'++'   ::= PLUS_PLUS
	'--'   ::= MINUS_MINUS
	'=='   ::= EQUAL_EQUAL
	'<='   ::= LESS_EQUAL
	'>='   ::= GREATER_EQUAL
	'!='   ::= NOT_EQUAL
	'<<'   ::= LEFT_SHIFT
	'>>'   ::= RIGHT_SHIFT
	'>>>'  ::= UNSIGNED_RIGHT_SHIFT
	'+='   ::= PLUS_EQUAL
	'-='   ::= MINUS_EQUAL
	'*='   ::= MULTIPLY_EQUAL
	'/='   ::= DIVIDE_EQUAL
	'&='   ::= AND_EQUAL
	'|='   ::= OR_EQUAL
	'^='   ::= XOR_EQUAL
	'%='   ::= REMAINDER_EQUAL
	'<<='  ::= LEFT_SHIFT_EQUAL
	'>>='  ::= RIGHT_SHIFT_EQUAL
	'>>>=' ::= UNSIGNED_RIGHT_SHIFT_EQUAL
	'||'   ::= OR_OR
	'&&'   ::= AND_AND

	'+'    ::= PLUS
	'-'    ::= MINUS
	'!'    ::= NOT
	'%'    ::= REMAINDER
	'^'    ::= XOR
	'&'    ::= AND
	'*'    ::= MULTIPLY
	'|'    ::= OR
	'~'    ::= TWIDDLE
	'/'    ::= DIVIDE
	'>'    ::= GREATER
	'<'    ::= LESS
	'('    ::= LPAREN
	')'    ::= RPAREN
	'{'    ::= LBRACE
	'}'    ::= RBRACE
	'['    ::= LBRACKET
	']'    ::= RBRACKET
	';'    ::= SEMICOLON
	'?'    ::= QUESTION
	':'    ::= COLON
	','    ::= COMMA
	'.'    ::= DOT
	'='    ::= EQUAL
	
$Start
	Goal

$Rules

/. // This method is part of an automatic generation : do NOT edit-modify  
protected void consumeRule(int act) {
  switch ( act ) {
./



Goal ::= '++' CompilationUnit
Goal ::= '--' MethodBody
-- Initializer
Goal ::= '>>' StaticInitializer
Goal ::= '>>' Initializer
-- error recovery
Goal ::= '>>>' Headers
Goal ::= '*' BlockStatements
Goal ::= '*' CatchHeader
-- JDOM
Goal ::= '&&' FieldDeclaration
Goal ::= '||' ImportDeclaration
Goal ::= '?' PackageDeclaration
Goal ::= '+' TypeDeclaration
Goal ::= '/' GenericMethodDeclaration
Goal ::= '&' ClassBodyDeclarations
-- code snippet
Goal ::= '%' Expression
-- completion parser
Goal ::= '~' BlockStatementsopt
/:$readableName Goal:/

Literal -> IntegerLiteral
Literal -> LongLiteral
Literal -> FloatingPointLiteral
Literal -> DoubleLiteral
Literal -> CharacterLiteral
Literal -> StringLiteral
Literal -> null
Literal -> BooleanLiteral
/:$readableName Literal:/
BooleanLiteral -> true
BooleanLiteral -> false
/:$readableName BooleanLiteral:/

-- AspectJ Extension
JavaIdentifier -> 'Identifier'
JavaIdentifier -> AjSimpleName

JavaIdentifierNoAround -> 'Identifier'
JavaIdentifierNoAround -> AjSimpleNameNoAround
-- End AspectJ Extension

-------------------------------------------------------------
-------------------------------------------------------------
--a Type results in both a push of its dimension(s) and its name(s).

Type ::= PrimitiveType
 /.$putCase consumePrimitiveType(); $break ./
Type -> ReferenceType
/:$readableName Type:/

PrimitiveType -> NumericType
/:$readableName PrimitiveType:/
NumericType -> IntegralType
NumericType -> FloatingPointType
/:$readableName NumericType:/

PrimitiveType -> 'boolean'
PrimitiveType -> 'void'
IntegralType -> 'byte'
IntegralType -> 'short'
IntegralType -> 'int'
IntegralType -> 'long'
IntegralType -> 'char'
/:$readableName IntegralType:/
FloatingPointType -> 'float'
FloatingPointType -> 'double'
/:$readableName FloatingPointType:/

ReferenceType ::= ClassOrInterfaceType
/.$putCase consumeReferenceType();  $break ./
ReferenceType -> ArrayType -- here a push of dimensions is done, that explains the two previous push 0
/:$readableName ReferenceType:/
ClassOrInterfaceType -> Name
/:$readableName Type:/

--
-- These rules have been rewritten to avoid some conflicts introduced
-- by adding the 1.1 features
--
-- ArrayType ::= PrimitiveType '[' ']'
-- ArrayType ::= Name '[' ']'
-- ArrayType ::= ArrayType '[' ']'
--

ArrayType ::= PrimitiveType Dims
ArrayType ::= Name Dims
/:$readableName ArrayType:/

ClassType -> ClassOrInterfaceType
/:$readableName ClassType:/

--------------------------------------------------------------
--------------------------------------------------------------

-- AspectJ Extension
NameOrAj -> AjName
NameOrAj -> Name


AjName -> AjSimpleName
AjName -> AjQualifiedName

AjSimpleName -> AjSimpleNameNoAround
AjSimpleNameNoAround -> 'aspect'
AjSimpleNameNoAround -> 'privileged'
AjSimpleNameNoAround -> 'pointcut'
AjSimpleName -> 'around'
AjSimpleNameNoAround -> 'before'
AjSimpleNameNoAround -> 'after'
AjSimpleNameNoAround -> 'declare'

AjQualifiedName ::= AjName '.' SimpleName
/.$putCase consumeQualifiedName(); $break ./
-- End AspectJ Extension

Name -> SimpleName
Name -> QualifiedName
/:$readableName Name:/

SimpleName -> 'Identifier'
/:$readableName SimpleName:/

QualifiedName ::= Name '.' JavaIdentifier -- AspectJ Extension (was SimpleName) 
/.$putCase consumeQualifiedName(); $break ./
/:$readableName QualifiedName:/

CompilationUnit ::= EnterCompilationUnit PackageDeclarationopt ImportDeclarationsopt TypeDeclarationsopt
/.$putCase consumeCompilationUnit(); $break ./
/:$readableName CompilationUnit:/

EnterCompilationUnit ::= $empty
/.$putCase consumeEnterCompilationUnit(); $break ./
/:$readableName EnterCompilationUnit:/

Headers -> Header
Headers ::= Headers Header
/:$readableName Headers:/

Header -> ImportDeclaration
Header -> PackageDeclaration
Header -> ClassHeader
Header -> InterfaceHeader
Header -> StaticInitializer
Header -> MethodHeader
Header -> ConstructorHeader
Header -> FieldDeclaration
Header -> AllocationHeader
Header -> ArrayCreationHeader 
/:$readableName Header:/

CatchHeader ::= 'catch' '(' FormalParameter ')' '{'
/.$putCase consumeCatchHeader(); $break ./
/:$readableName CatchHeader:/

ImportDeclarations -> ImportDeclaration
ImportDeclarations ::= ImportDeclarations ImportDeclaration 
/.$putCase consumeImportDeclarations(); $break ./
/:$readableName ImportDeclarations:/

TypeDeclarations -> TypeDeclaration
TypeDeclarations ::= TypeDeclarations TypeDeclaration
/.$putCase consumeTypeDeclarations(); $break ./
/:$readableName TypeDeclarations:/

PackageDeclaration ::= PackageDeclarationName ';'
/.$putCase  consumePackageDeclaration(); $break ./
/:$readableName PackageDeclaration:/

PackageDeclarationName ::= 'package' Name
/.$putCase  consumePackageDeclarationName(); $break ./
/:$readableName PackageDeclarationName:/

ImportDeclaration -> SingleTypeImportDeclaration
ImportDeclaration -> TypeImportOnDemandDeclaration
/:$readableName ImportDeclaration:/

SingleTypeImportDeclaration ::= SingleTypeImportDeclarationName ';'
/.$putCase consumeSingleTypeImportDeclaration(); $break ./
/:$readableName SingleTypeImportDeclaration:/
			  
SingleTypeImportDeclarationName ::= 'import' Name
/.$putCase consumeSingleTypeImportDeclarationName(); $break ./
/:$readableName SingleTypeImportDeclarationName:/
			  
TypeImportOnDemandDeclaration ::= TypeImportOnDemandDeclarationName ';'
/.$putCase consumeTypeImportOnDemandDeclaration(); $break ./
/:$readableName TypeImportOnDemandDeclaration:/

TypeImportOnDemandDeclarationName ::= 'import' Name '.' '*'
/.$putCase consumeTypeImportOnDemandDeclarationName(); $break ./
/:$readableName TypeImportOnDemandDeclarationName:/

TypeDeclaration -> ClassDeclaration
TypeDeclaration -> InterfaceDeclaration
-- this declaration in part of a list od declaration and we will
-- use and optimized list length calculation process 
-- thus we decrement the number while it will be incremend.....
TypeDeclaration ::= ';' 
/. $putCase consumeEmptyTypeDeclaration(); $break ./
/:$readableName TypeDeclaration:/

--18.7 Only in the LALR(1) Grammar

Modifiers ::= Modifier
Modifiers ::= Modifiers Modifier
/:$readableName Modifiers:/

Modifier -> 'public' 
Modifier -> 'protected'
Modifier -> 'private'
Modifier -> 'static'
Modifier -> 'abstract'
Modifier -> 'final'
Modifier -> 'native'
Modifier -> 'synchronized'
Modifier -> 'transient'
Modifier -> 'volatile'
Modifier -> 'strictfp'
/:$readableName Modifier:/

-- AspectJ Extension
--New AspectJ Productions

-- two declarations are visible outside of aspects
Header -> PointcutDeclaration
Header -> BasicAdviceDeclaration
Header -> AroundDeclaration
Header -> DeclareDeclaration
Header -> InterTypeMethodDeclaration
Header -> InterTypeFieldDeclaration


TypeDeclaration -> AspectDeclaration
Header -> AspectDeclaration

ClassMemberDeclaration -> AspectDeclaration
InterfaceMemberDeclaration -> AspectDeclaration


ClassMemberDeclaration -> PointcutDeclaration
InterfaceMemberDeclaration -> PointcutDeclaration

-- everthing else is only visible inside an aspect
AspectDeclaration ::= AspectHeader AspectBody
/.$putCase consumeAspectDeclaration(); $break ./

AspectHeader ::= AspectHeaderName ClassHeaderExtendsopt ClassHeaderImplementsopt AspectHeaderRest
/.$putCase consumeAspectHeader(); $break ./

AspectHeaderName ::= Modifiersopt 'aspect' 'Identifier'
/.$putCase consumeAspectHeaderName(false); $break ./

AspectHeaderName ::= Modifiersopt 'privileged' Modifiersopt 'aspect' 'Identifier'
/.$putCase consumeAspectHeaderName(true); $break ./


AspectHeaderRest ::= $empty

--[dominates TypePattern] [persingleton() | percflow(PCD) | perthis(PCD) | pertarget(PCD)]
AspectHeaderRest ::= AspectHeaderRestStart PseudoTokens
/.$putCase consumeAspectHeaderRest(); $break ./

AspectHeaderRestStart ::= 'Identifier'
/.$putCase consumePseudoTokenIdentifier(); $break ./


AspectBody ::= '{' AspectBodyDeclarationsopt '}'

AspectBodyDeclarations ::= AspectBodyDeclaration
AspectBodyDeclarations ::= AspectBodyDeclarations AspectBodyDeclaration
/.$putCase consumeClassBodyDeclarations(); $break ./


AspectBodyDeclarationsopt ::= $empty
/.$putCase consumeEmptyClassBodyDeclarationsopt(); $break ./

-- ??? why is NestedType here
AspectBodyDeclarationsopt ::= NestedType AspectBodyDeclarations
/.$putCase consumeClassBodyDeclarationsopt(); $break ./


AspectBodyDeclaration ::= ClassBodyDeclaration
/.$putCase consumeClassBodyDeclarationInAspect(); $break ./


-- pointcuts and advice

PointcutDeclaration ::= PointcutHeader MethodHeaderParameters ';'
/.$putCase consumeEmptyPointcutDeclaration(); $break ./

PointcutDeclaration ::= PointcutHeader MethodHeaderParameters ':' PseudoTokens  ';'
/.$putCase consumePointcutDeclaration(); $break ./

PointcutHeader ::= Modifiersopt 'pointcut'  JavaIdentifier '('
/.$putCase consumePointcutHeader(); $break ./



AspectBodyDeclaration -> AroundDeclaration
AspectBodyDeclaration -> BasicAdviceDeclaration

AroundDeclaration ::= AroundHeader MethodBody
/.$putCase consumeAroundDeclaration(); $break ./

AroundHeader ::= AroundHeaderName MethodHeaderParameters MethodHeaderThrowsClauseopt ':' PseudoTokens
/.$putCase consumeAroundHeader(); $break ./

-- no modifiers are actually allowed on around, but the grammar is happier this way
AroundHeaderName ::= Modifiersopt Type  'around' '(' 
/.$putCase consumeAroundHeaderName(); $break ./

BasicAdviceDeclaration ::= BasicAdviceHeader MethodBody
/.$putCase consumeBasicAdviceDeclaration(); $break ./
/:$readableName AdviceDeclaration:/


BasicAdviceHeader ::= BasicAdviceHeaderName MethodHeaderParameters ExtraParamopt MethodHeaderThrowsClauseopt ':' PseudoTokens
/.$putCase consumeBasicAdviceHeader(); $break ./
/:$readableName AdviceHeader:/


BasicAdviceHeaderName ::= Modifiersopt 'before' '(' 
/.$putCase consumeBasicAdviceHeaderName(false); $break ./
/:$readableName AdviceHeaderName:/

BasicAdviceHeaderName ::= Modifiersopt 'after' '(' 
/.$putCase consumeBasicAdviceHeaderName(true); $break ./
/:$readableName AdviceHeaderName:/

ExtraParamopt ::= 'Identifier' '(' FormalParameter ')'
/.$putCase consumeExtraParameterWithFormal(); $break ./
/:$readableName ExtraParam:/

ExtraParamopt ::= 'Identifier' '(' ')'
/.$putCase consumeExtraParameterNoFormal(); $break ./
/:$readableName ExtraParam:/

-- deprecated, but we're probably stuck with it now
ExtraParamopt ::= 'Identifier'
/.$putCase consumeExtraParameterNoFormal(); $break ./
/:$readableName ExtraParam:/

ExtraParamopt ::= $empty



-- intertype declarations

OnType ::= JavaIdentifier
OnType ::= OnType '.' JavaIdentifier
/.$putCase consumeQualifiedName(); $break ./

AspectBodyDeclaration -> InterTypeMethodDeclaration
AspectBodyDeclaration -> InterTypeConstructorDeclaration
AspectBodyDeclaration -> InterTypeFieldDeclaration

InterTypeMethodDeclaration -> AbstractInterTypeMethodDeclaration
InterTypeMethodDeclaration ::= InterTypeMethodHeader MethodBody 
/.$putCase // set to true to consume a method with a body
  consumeInterTypeMethodDeclaration(true);  $break ./

InterTypeMethodHeader ::= InterTypeMethodHeaderName MethodHeaderParameters MethodHeaderExtendedDims MethodHeaderThrowsClauseopt
/.$putCase consumeInterTypeMethodHeader(); $break ./

InterTypeMethodHeaderName ::= Modifiersopt Type OnType '.' JavaIdentifier '('
/.$putCase consumeInterTypeMethodHeaderName(); $break ./


AbstractInterTypeMethodDeclaration ::= InterTypeMethodHeader ';'
/.$putCase // set to false to consume a method without body
  consumeInterTypeMethodDeclaration(false); $break ./


InterTypeConstructorDeclaration ::= InterTypeConstructorHeader MethodBody 
/.$putCase // set to true to consume a method with a body
  consumeInterTypeConstructorDeclaration();  $break ./

InterTypeConstructorHeader ::= InterTypeConstructorHeaderName MethodHeaderParameters MethodHeaderThrowsClauseopt
/.$putCase consumeInterTypeConstructorHeader(); $break ./

-- using Name instead of OnType to make jikespg happier
InterTypeConstructorHeaderName ::= Modifiersopt Name '.' 'new' '('
/.$putCase consumeInterTypeConstructorHeaderName(); $break ./


InterTypeFieldDeclaration ::= Modifiersopt Type OnType '.' JavaIdentifier InterTypeFieldBody ';'
/.$putCase consumeInterTypeFieldDeclaration(); $break ./

InterTypeFieldBody ::=  $empty
InterTypeFieldBody ::= '=' ForceNoDiet VariableInitializer RestoreDiet




-- declares (more fun than a pcd)
AspectBodyDeclaration -> DeclareDeclaration

DeclareDeclaration ::= DeclareHeader PseudoTokens ';'
/.$putCase consumeDeclareDeclaration(); $break ./

DeclareHeader ::= 'declare' 'Identifier' ':' 
/.$putCase consumeDeclareHeader(); $break ./



-- the joy of pcds
PseudoTokens ::= PseudoToken
PseudoTokens ::= PseudoTokens PseudoToken
/.$putCase consumePseudoTokens(); $break ./

PseudoToken ::= JavaIdentifier
/.$putCase consumePseudoTokenIdentifier(); $break ./

PseudoToken ::= '('
/.$putCase consumePseudoToken("("); $break ./

PseudoToken ::= ')'
/.$putCase consumePseudoToken(")"); $break ./

PseudoToken ::= '.'
/.$putCase consumePseudoToken("."); $break ./

PseudoToken ::= '*'
/.$putCase consumePseudoToken("*"); $break ./

PseudoToken ::= '+'
/.$putCase consumePseudoToken("+"); $break ./

PseudoToken ::= '&&'
/.$putCase consumePseudoToken("&&"); $break ./

PseudoToken ::= '||'
/.$putCase consumePseudoToken("||"); $break ./

PseudoToken ::= '!'
/.$putCase consumePseudoToken("!"); $break ./

PseudoToken ::= ':'
/.$putCase consumePseudoToken(":"); $break ./

PseudoToken ::= ','
/.$putCase consumePseudoToken(","); $break ./

PseudoToken ::= '['
/.$putCase consumePseudoToken("["); $break ./

PseudoToken ::= ']'
/.$putCase consumePseudoToken("]"); $break ./



PseudoToken ::= PrimitiveType
/.$putCase consumePseudoTokenPrimitiveType(); $break ./

PseudoToken ::= Modifier
/.$putCase consumePseudoTokenModifier(); $break ./

PseudoToken ::= Literal
/.$putCase consumePseudoTokenLiteral(); $break ./


PseudoToken ::= 'this'
/.$putCase consumePseudoToken("this", 1, true); $break ./

PseudoToken ::= 'super'
/.$putCase consumePseudoToken("super", 1, true); $break ./


-- special handling for if
PseudoToken ::= 'if' '(' Expression ')'
/.$putCase consumePseudoTokenIf(); $break ./

PseudoToken ::= 'assert'
/.$putCase consumePseudoToken("assert", 1, true); $break ./

PseudoToken ::= 'import'
/.$putCase consumePseudoToken("import", 1, true); $break ./

PseudoToken ::= 'package'
/.$putCase consumePseudoToken("package", 1, true); $break ./

PseudoToken ::= 'throw'
/.$putCase consumePseudoToken("throw", 1, true); $break ./

PseudoToken ::= 'new'
/.$putCase consumePseudoToken("new", 1, true); $break ./

PseudoToken ::= 'do'
/.$putCase consumePseudoToken("do", 1, true); $break ./

PseudoToken ::= 'for'
/.$putCase consumePseudoToken("for", 1, true); $break ./

PseudoToken ::= 'switch'
/.$putCase consumePseudoToken("switch", 1, true); $break ./

PseudoToken ::= 'try'
/.$putCase consumePseudoToken("try", 1, true); $break ./

PseudoToken ::= 'while'
/.$putCase consumePseudoToken("while", 1, true); $break ./

PseudoToken ::= 'break'
/.$putCase consumePseudoToken("break", 1, true); $break ./

PseudoToken ::= 'continue'
/.$putCase consumePseudoToken("continue", 1, true); $break ./

PseudoToken ::= 'return'
/.$putCase consumePseudoToken("return", 1, true); $break ./

PseudoToken ::= 'case'
/.$putCase consumePseudoToken("case", 1, true); $break ./

PseudoToken ::= 'catch'
/.$putCase consumePseudoToken("catch", 0, true); $break ./

PseudoToken ::= 'instanceof'
/.$putCase consumePseudoToken("instanceof", 0, true); $break ./

PseudoToken ::= 'else'
/.$putCase consumePseudoToken("else", 0, true); $break ./

PseudoToken ::= 'extends'
/.$putCase consumePseudoToken("extends", 0, true); $break ./

PseudoToken ::= 'finally'
/.$putCase consumePseudoToken("finally", 0, true); $break ./

PseudoToken ::= 'implements'
/.$putCase consumePseudoToken("implements", 0, true); $break ./

PseudoToken ::= 'throws'
/.$putCase consumePseudoToken("throws", 0, true); $break ./

-- add all other keywords as identifiers
-- End AspectJ Extension


--18.8 Productions from 8: Class Declarations
--ClassModifier ::=
--      'abstract'
--    | 'final'
--    | 'public'
--18.8.1 Productions from 8.1: Class Declarations

ClassDeclaration ::= ClassHeader ClassBody
/.$putCase consumeClassDeclaration(); $break ./
/:$readableName ClassDeclaration:/

ClassHeader ::= ClassHeaderName ClassHeaderExtendsopt ClassHeaderImplementsopt
/.$putCase consumeClassHeader(); $break ./
/:$readableName ClassHeader:/

ClassHeaderName ::= Modifiersopt 'class' JavaIdentifier  -- AspectJ Extension (was 'Identifier')
/.$putCase consumeClassHeaderName(); $break ./
/:$readableName ClassHeaderName:/

ClassHeaderExtends ::= 'extends' ClassType
/.$putCase consumeClassHeaderExtends(); $break ./
/:$readableName ClassHeaderExtends:/

ClassHeaderImplements ::= 'implements' InterfaceTypeList
/.$putCase consumeClassHeaderImplements(); $break ./
/:$readableName ClassHeaderImplements:/

InterfaceTypeList -> InterfaceType
InterfaceTypeList ::= InterfaceTypeList ',' InterfaceType
/.$putCase consumeInterfaceTypeList(); $break ./
/:$readableName InterfaceTypeList:/

InterfaceType ::= ClassOrInterfaceType
/.$putCase consumeInterfaceType(); $break ./
/:$readableName InterfaceType:/

ClassBody ::= '{' ClassBodyDeclarationsopt '}'
/:$readableName ClassBody:/

ClassBodyDeclarations ::= ClassBodyDeclaration
ClassBodyDeclarations ::= ClassBodyDeclarations ClassBodyDeclaration
/.$putCase consumeClassBodyDeclarations(); $break ./
/:$readableName ClassBodyDeclarations:/

ClassBodyDeclaration -> ClassMemberDeclaration
ClassBodyDeclaration -> StaticInitializer
ClassBodyDeclaration -> ConstructorDeclaration
--1.1 feature
ClassBodyDeclaration ::= Diet NestedMethod Block
/.$putCase consumeClassBodyDeclaration(); $break ./
/:$readableName ClassBodyDeclaration:/
Diet ::= $empty
/.$putCase consumeDiet(); $break./
/:$readableName Diet:/

Initializer ::= Diet NestedMethod Block
/.$putCase consumeClassBodyDeclaration(); $break ./
/:$readableName Initializer:/

ClassMemberDeclaration -> FieldDeclaration
ClassMemberDeclaration -> MethodDeclaration
--1.1 feature
ClassMemberDeclaration -> ClassDeclaration
--1.1 feature
ClassMemberDeclaration -> InterfaceDeclaration
/:$readableName ClassMemberDeclaration:/

-- Empty declarations are not valid Java ClassMemberDeclarations.
-- However, since the current (2/14/97) Java compiler accepts them 
-- (in fact, some of the official tests contain this erroneous
-- syntax)

GenericMethodDeclaration -> MethodDeclaration
GenericMethodDeclaration -> ConstructorDeclaration
/:$readableName GenericMethodDeclaration:/

ClassMemberDeclaration ::= ';'
/.$putCase consumeEmptyClassMemberDeclaration(); $break./

--18.8.2 Productions from 8.3: Field Declarations
--VariableModifier ::=
--      'public'
--    | 'protected'
--    | 'private'
--    | 'static'
--    | 'final'
--    | 'transient'
--    | 'volatile'

FieldDeclaration ::= Modifiersopt Type VariableDeclarators ';'
/.$putCase consumeFieldDeclaration(); $break ./
/:$readableName FieldDeclaration:/

VariableDeclarators -> VariableDeclarator 
VariableDeclarators ::= VariableDeclarators ',' VariableDeclarator
/.$putCase consumeVariableDeclarators(); $break ./
/:$readableName VariableDeclarators:/

VariableDeclarator ::= VariableDeclaratorId EnterVariable ExitVariableWithoutInitialization

VariableDeclarator ::= VariableDeclaratorId EnterVariable '=' ForceNoDiet VariableInitializer RestoreDiet ExitVariableWithInitialization
/:$readableName VariableDeclarator:/

EnterVariable ::= $empty
/.$putCase consumeEnterVariable(); $break ./
/:$readableName EnterVariable:/

ExitVariableWithInitialization ::= $empty
/.$putCase consumeExitVariableWithInitialization(); $break ./
/:$readableName ExitVariableWithInitialization:/

ExitVariableWithoutInitialization ::= $empty
/.$putCase consumeExitVariableWithoutInitialization(); $break ./
/:$readableName ExitVariableWithoutInitialization:/

ForceNoDiet ::= $empty
/.$putCase consumeForceNoDiet(); $break ./
/:$readableName ForceNoDiet:/
RestoreDiet ::= $empty
/.$putCase consumeRestoreDiet(); $break ./
/:$readableName RestoreDiet:/

VariableDeclaratorId ::= JavaIdentifier Dimsopt -- AspectJ Extension (was 'Identifier')
/:$readableName VariableDeclaratorId:/

VariableInitializer -> Expression
VariableInitializer -> ArrayInitializer
/:$readableName VariableInitializer:/

--18.8.3 Productions from 8.4: Method Declarations
--MethodModifier ::=
--      'public'
--    | 'protected'
--    | 'private'
--    | 'static'
--    | 'abstract'
--    | 'final'
--    | 'native'
--    | 'synchronized'
--

MethodDeclaration -> AbstractMethodDeclaration
MethodDeclaration ::= MethodHeader MethodBody 
/.$putCase // set to true to consume a method with a body
  consumeMethodDeclaration(true);  $break ./
/:$readableName MethodDeclaration:/

AbstractMethodDeclaration ::= MethodHeader ';'
/.$putCase // set to false to consume a method without body
  consumeMethodDeclaration(false); $break ./
/:$readableName AbstractMethodDeclaration:/

MethodHeader ::= MethodHeaderName MethodHeaderParameters MethodHeaderExtendedDims MethodHeaderThrowsClauseopt
/.$putCase consumeMethodHeader(); $break ./
/:$readableName MethodHeader:/

MethodHeaderName ::= Modifiersopt Type JavaIdentifierNoAround '(' -- AspectJ Extension (was 'Identifier')
/.$putCase consumeMethodHeaderName(); $break ./
/:$readableName MethodHeaderName:/

MethodHeaderParameters ::= FormalParameterListopt ')'
/.$putCase consumeMethodHeaderParameters(); $break ./
/:$readableName MethodHeaderParameters:/

MethodHeaderExtendedDims ::= Dimsopt
/.$putCase consumeMethodHeaderExtendedDims(); $break ./
/:$readableName MethodHeaderExtendedDims:/

MethodHeaderThrowsClause ::= 'throws' ClassTypeList
/.$putCase consumeMethodHeaderThrowsClause(); $break ./
/:$readableName MethodHeaderThrowsClause:/

ConstructorHeader ::= ConstructorHeaderName MethodHeaderParameters MethodHeaderThrowsClauseopt
/.$putCase consumeConstructorHeader(); $break ./
/:$readableName ConstructorHeader:/

ConstructorHeaderName ::=  Modifiersopt 'Identifier' '('
/.$putCase consumeConstructorHeaderName(); $break ./
/:$readableName ConstructorHeaderName:/

-- AspectJ Extension
ConstructorHeaderName ::=  Modifiersopt 'aspect' '('  -- makes aspect harder
/.$putCase consumeConstructorHeaderName(); $break ./
-- End AspectJ Extension

FormalParameterList -> FormalParameter
FormalParameterList ::= FormalParameterList ',' FormalParameter
/.$putCase consumeFormalParameterList(); $break ./
/:$readableName FormalParameterList:/

--1.1 feature
FormalParameter ::= Modifiersopt Type VariableDeclaratorId
/.$putCase // the boolean is used to know if the modifiers should be reset
 	consumeFormalParameter(); $break ./
/:$readableName FormalParameter:/

ClassTypeList -> ClassTypeElt
ClassTypeList ::= ClassTypeList ',' ClassTypeElt
/.$putCase consumeClassTypeList(); $break ./
/:$readableName ClassTypeList:/

ClassTypeElt ::= ClassType
/.$putCase consumeClassTypeElt(); $break ./
/:$readableName ClassType:/

MethodBody ::= NestedMethod '{' BlockStatementsopt '}' 
/.$putCase consumeMethodBody(); $break ./
/:$readableName MethodBody:/

NestedMethod ::= $empty
/.$putCase consumeNestedMethod(); $break ./
/:$readableName NestedMethod:/

--18.8.4 Productions from 8.5: Static Initializers

StaticInitializer ::=  StaticOnly Block
/.$putCase consumeStaticInitializer(); $break./
/:$readableName StaticInitializer:/

StaticOnly ::= 'static'
/.$putCase consumeStaticOnly(); $break ./
/:$readableName StaticOnly:/

--18.8.5 Productions from 8.6: Constructor Declarations
--ConstructorModifier ::=
--      'public'
--    | 'protected'
--    | 'private'
--
--
ConstructorDeclaration ::= ConstructorHeader MethodBody
/.$putCase consumeConstructorDeclaration() ; $break ./ 

-- These rules are added to be able to parse constructors with no body
ConstructorDeclaration ::= ConstructorHeader ';'
/.$putCase consumeInvalidConstructorDeclaration() ; $break ./ 
/:$readableName ConstructorDeclaration:/

-- the rules ExplicitConstructorInvocationopt has been expanded
-- in the rule below in order to make the grammar lalr(1).

ExplicitConstructorInvocation ::= 'this' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(0,ExplicitConstructorCall.This); $break ./

ExplicitConstructorInvocation ::= 'super' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(0,ExplicitConstructorCall.Super); $break ./

--1.1 feature
ExplicitConstructorInvocation ::= Primary '.' 'super' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(1, ExplicitConstructorCall.Super); $break ./

--1.1 feature
ExplicitConstructorInvocation ::= Name '.' 'super' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(2, ExplicitConstructorCall.Super); $break ./

--1.1 feature
ExplicitConstructorInvocation ::= Primary '.' 'this' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(1, ExplicitConstructorCall.This); $break ./

--1.1 feature
ExplicitConstructorInvocation ::= Name '.' 'this' '(' ArgumentListopt ')' ';'
/.$putCase consumeExplicitConstructorInvocation(2, ExplicitConstructorCall.This); $break ./
/:$readableName ExplicitConstructorInvocation:/

--18.9 Productions from 9: Interface Declarations

--18.9.1 Productions from 9.1: Interface Declarations
--InterfaceModifier ::=
--      'public'
--    | 'abstract'
--
InterfaceDeclaration ::= InterfaceHeader InterfaceBody
/.$putCase consumeInterfaceDeclaration(); $break ./
/:$readableName InterfaceDeclaration:/

InterfaceHeader ::= InterfaceHeaderName InterfaceHeaderExtendsopt
/.$putCase consumeInterfaceHeader(); $break ./
/:$readableName InterfaceHeader:/

InterfaceHeaderName ::= Modifiersopt 'interface' JavaIdentifier -- AspectJ Extension (was 'Identifier')
/.$putCase consumeInterfaceHeaderName(); $break ./
/:$readableName InterfaceHeaderName:/

-- This rule will be used to accept inner local interface and then report a relevant error message
InvalidInterfaceDeclaration -> InterfaceHeader InterfaceBody
/:$readableName InvalidInterfaceDeclaration:/

InterfaceHeaderExtends ::= 'extends' InterfaceTypeList
/.$putCase consumeInterfaceHeaderExtends(); $break ./
/:$readableName InterfaceHeaderExtends:/

InterfaceBody ::= '{' InterfaceMemberDeclarationsopt '}' 
/:$readableName InterfaceBody:/

InterfaceMemberDeclarations -> InterfaceMemberDeclaration
InterfaceMemberDeclarations ::= InterfaceMemberDeclarations InterfaceMemberDeclaration
/.$putCase consumeInterfaceMemberDeclarations(); $break ./
/:$readableName InterfaceMemberDeclarations:/

--same as for class members
InterfaceMemberDeclaration ::= ';'
/.$putCase consumeEmptyInterfaceMemberDeclaration(); $break ./
/:$readableName InterfaceMemberDeclaration:/

-- This rule is added to be able to parse non abstract method inside interface and then report a relevent error message
InvalidMethodDeclaration -> MethodHeader MethodBody
/:$readableName InvalidMethodDeclaration:/

InterfaceMemberDeclaration -> ConstantDeclaration
InterfaceMemberDeclaration ::= InvalidMethodDeclaration
/.$putCase ignoreMethodBody(); $break ./
/:$readableName InterfaceMemberDeclaration:/

-- These rules are added to be able to parse constructors inside interface and then report a relevent error message
InvalidConstructorDeclaration ::= ConstructorHeader MethodBody
/.$putCase ignoreInvalidConstructorDeclaration(true);  $break ./

InvalidConstructorDeclaration ::= ConstructorHeader ';'
/.$putCase ignoreInvalidConstructorDeclaration(false);  $break ./
/:$readableName InvalidConstructorDeclaration:/

InterfaceMemberDeclaration -> AbstractMethodDeclaration
InterfaceMemberDeclaration -> InvalidConstructorDeclaration
/:$readableName InterfaceMemberDeclaration:/

--1.1 feature
InterfaceMemberDeclaration -> ClassDeclaration
--1.1 feature
InterfaceMemberDeclaration -> InterfaceDeclaration
/:$readableName InterfaceMemberDeclaration:/

ConstantDeclaration -> FieldDeclaration
/:$readableName ConstantDeclaration:/

ArrayInitializer ::= '{' ,opt '}'
/.$putCase consumeEmptyArrayInitializer(); $break ./
ArrayInitializer ::= '{' VariableInitializers '}'
/.$putCase consumeArrayInitializer(); $break ./
ArrayInitializer ::= '{' VariableInitializers , '}'
/.$putCase consumeArrayInitializer(); $break ./
/:$readableName ArrayInitializer:/

VariableInitializers ::= VariableInitializer
VariableInitializers ::= VariableInitializers ',' VariableInitializer
/.$putCase consumeVariableInitializers(); $break ./
/:$readableName VariableInitializers:/

Block ::= OpenBlock '{' BlockStatementsopt '}'
/.$putCase consumeBlock(); $break ./
/:$readableName Block:/

OpenBlock ::= $empty
/.$putCase consumeOpenBlock() ; $break ./
/:$readableName OpenBlock:/

BlockStatements -> BlockStatement
BlockStatements ::= BlockStatements BlockStatement
/.$putCase consumeBlockStatements() ; $break ./
/:$readableName BlockStatements:/

BlockStatement -> LocalVariableDeclarationStatement
BlockStatement -> Statement
--1.1 feature
BlockStatement -> ClassDeclaration
BlockStatement ::= InvalidInterfaceDeclaration
/.$putCase ignoreInterfaceDeclaration(); $break ./
/:$readableName BlockStatement:/

LocalVariableDeclarationStatement ::= LocalVariableDeclaration ';'
/.$putCase consumeLocalVariableDeclarationStatement(); $break ./
/:$readableName LocalVariableDeclarationStatement:/

LocalVariableDeclaration ::= Type PushModifiers VariableDeclarators
/.$putCase consumeLocalVariableDeclaration(); $break ./

-- 1.1 feature
-- The modifiers part of this rule makes the grammar more permissive. 
-- The only modifier here is final. We put Modifiers to allow multiple modifiers
-- This will require to check the validity of the modifier

LocalVariableDeclaration ::= Modifiers Type PushModifiers VariableDeclarators
/.$putCase consumeLocalVariableDeclaration(); $break ./
/:$readableName LocalVariableDeclaration:/

PushModifiers ::= $empty
/.$putCase consumePushModifiers(); $break ./
/:$readableName PushModifiers:/

Statement -> StatementWithoutTrailingSubstatement
Statement -> LabeledStatement
Statement -> IfThenStatement
Statement -> IfThenElseStatement
Statement -> WhileStatement
Statement -> ForStatement
/:$readableName Statement:/

StatementNoShortIf -> StatementWithoutTrailingSubstatement
StatementNoShortIf -> LabeledStatementNoShortIf
StatementNoShortIf -> IfThenElseStatementNoShortIf
StatementNoShortIf -> WhileStatementNoShortIf
StatementNoShortIf -> ForStatementNoShortIf
/:$readableName Statement:/

StatementWithoutTrailingSubstatement -> AssertStatement
StatementWithoutTrailingSubstatement -> Block
StatementWithoutTrailingSubstatement -> EmptyStatement
StatementWithoutTrailingSubstatement -> ExpressionStatement
StatementWithoutTrailingSubstatement -> SwitchStatement
StatementWithoutTrailingSubstatement -> DoStatement
StatementWithoutTrailingSubstatement -> BreakStatement
StatementWithoutTrailingSubstatement -> ContinueStatement
StatementWithoutTrailingSubstatement -> ReturnStatement
StatementWithoutTrailingSubstatement -> SynchronizedStatement
StatementWithoutTrailingSubstatement -> ThrowStatement
StatementWithoutTrailingSubstatement -> TryStatement
/:$readableName Statement:/

EmptyStatement ::= ';'
/.$putCase consumeEmptyStatement(); $break ./
/:$readableName EmptyStatement:/

LabeledStatement ::= JavaIdentifier ':' Statement -- AspectJ Extension (was 'Identifier')
/.$putCase consumeStatementLabel() ; $break ./
/:$readableName LabeledStatement:/

LabeledStatementNoShortIf ::= JavaIdentifier ':' StatementNoShortIf -- AspectJ Extension (was 'Identifier')
/.$putCase consumeStatementLabel() ; $break ./
/:$readableName LabeledStatement:/

ExpressionStatement ::= StatementExpression ';'
/. $putCase consumeExpressionStatement(); $break ./
ExpressionStatement ::= ExplicitConstructorInvocation
/:$readableName Statement:/

StatementExpression ::= Assignment
StatementExpression ::= PreIncrementExpression
StatementExpression ::= PreDecrementExpression
StatementExpression ::= PostIncrementExpression
StatementExpression ::= PostDecrementExpression
StatementExpression ::= MethodInvocation
StatementExpression ::= ClassInstanceCreationExpression
/:$readableName Expression:/

IfThenStatement ::=  'if' '(' Expression ')' Statement
/.$putCase consumeStatementIfNoElse(); $break ./
/:$readableName IfStatement:/

IfThenElseStatement ::=  'if' '(' Expression ')' StatementNoShortIf 'else' Statement
/.$putCase consumeStatementIfWithElse(); $break ./
/:$readableName IfStatement:/

IfThenElseStatementNoShortIf ::=  'if' '(' Expression ')' StatementNoShortIf 'else' StatementNoShortIf
/.$putCase consumeStatementIfWithElse(); $break ./
/:$readableName IfStatement:/

SwitchStatement ::= 'switch' '(' Expression ')' OpenBlock SwitchBlock
/.$putCase consumeStatementSwitch() ; $break ./
/:$readableName SwitchStatement:/

SwitchBlock ::= '{' '}'
/.$putCase consumeEmptySwitchBlock() ; $break ./

SwitchBlock ::= '{' SwitchBlockStatements '}'
SwitchBlock ::= '{' SwitchLabels '}'
SwitchBlock ::= '{' SwitchBlockStatements SwitchLabels '}'
/.$putCase consumeSwitchBlock() ; $break ./
/:$readableName SwitchBlock:/

SwitchBlockStatements -> SwitchBlockStatement
SwitchBlockStatements ::= SwitchBlockStatements SwitchBlockStatement
/.$putCase consumeSwitchBlockStatements() ; $break ./
/:$readableName SwitchBlockStatements:/

SwitchBlockStatement ::= SwitchLabels BlockStatements
/.$putCase consumeSwitchBlockStatement() ; $break ./
/:$readableName SwitchBlockStatement:/


SwitchLabels -> SwitchLabel
SwitchLabels ::= SwitchLabels SwitchLabel
/.$putCase consumeSwitchLabels() ; $break ./
/:$readableName SwitchLabels:/

SwitchLabel ::= 'case' ConstantExpression ':'
/. $putCase consumeCaseLabel(); $break ./

SwitchLabel ::= 'default' ':'
/. $putCase consumeDefaultLabel(); $break ./
/:$readableName SwitchLabel:/

WhileStatement ::= 'while' '(' Expression ')' Statement
/.$putCase consumeStatementWhile() ; $break ./
/:$readableName WhileStatement:/

WhileStatementNoShortIf ::= 'while' '(' Expression ')' StatementNoShortIf
/.$putCase consumeStatementWhile() ; $break ./
/:$readableName WhileStatement:/

DoStatement ::= 'do' Statement 'while' '(' Expression ')' ';'
/.$putCase consumeStatementDo() ; $break ./
/:$readableName DoStatement:/

ForStatement ::= 'for' '(' ForInitopt ';' Expressionopt ';' ForUpdateopt ')' Statement
/.$putCase consumeStatementFor() ; $break ./
/:$readableName ForStatement:/

ForStatementNoShortIf ::= 'for' '(' ForInitopt ';' Expressionopt ';' ForUpdateopt ')' StatementNoShortIf
/.$putCase consumeStatementFor() ; $break ./
/:$readableName ForStatement:/

--the minus one allows to avoid a stack-to-stack transfer
ForInit ::= StatementExpressionList
/.$putCase consumeForInit() ; $break ./
ForInit -> LocalVariableDeclaration
/:$readableName ForInit:/

ForUpdate -> StatementExpressionList
/:$readableName ForUpdate:/

StatementExpressionList -> StatementExpression
StatementExpressionList ::= StatementExpressionList ',' StatementExpression
/.$putCase consumeStatementExpressionList() ; $break ./
/:$readableName StatementExpressionList:/

-- 1.4 feature
AssertStatement ::= 'assert' Expression ';'
/.$putCase consumeSimpleAssertStatement() ; $break ./

AssertStatement ::= 'assert' Expression ':' Expression ';'
/.$putCase consumeAssertStatement() ; $break ./
/:$readableName AssertStatement:/

BreakStatement ::= 'break' ';'
/.$putCase consumeStatementBreak() ; $break ./

BreakStatement ::= 'break' Identifier ';'
/.$putCase consumeStatementBreakWithLabel() ; $break ./
/:$readableName BreakStatement:/

ContinueStatement ::= 'continue' ';'
/.$putCase consumeStatementContinue() ; $break ./

ContinueStatement ::= 'continue' Identifier ';'
/.$putCase consumeStatementContinueWithLabel() ; $break ./
/:$readableName ContinueStatement:/

ReturnStatement ::= 'return' Expressionopt ';'
/.$putCase consumeStatementReturn() ; $break ./
/:$readableName ReturnStatement:/

ThrowStatement ::= 'throw' Expression ';'
/.$putCase consumeStatementThrow();
$break ./
/:$readableName ThrowStatement:/

SynchronizedStatement ::= OnlySynchronized '(' Expression ')'    Block
/.$putCase consumeStatementSynchronized(); $break ./
/:$readableName SynchronizedStatement:/

OnlySynchronized ::= 'synchronized'
/.$putCase consumeOnlySynchronized(); $break ./
/:$readableName OnlySynchronized:/

TryStatement ::= 'try' TryBlock Catches
/.$putCase consumeStatementTry(false); $break ./
TryStatement ::= 'try' TryBlock Catchesopt Finally
/.$putCase consumeStatementTry(true); $break ./
/:$readableName TryStatement:/

TryBlock ::= Block ExitTryBlock
/:$readableName Block:/

ExitTryBlock ::= $empty
/.$putCase consumeExitTryBlock(); $break ./
/:$readableName ExitTryBlock:/

Catches -> CatchClause
Catches ::= Catches CatchClause
/.$putCase consumeCatches(); $break ./
/:$readableName Catches:/

CatchClause ::= 'catch' '(' FormalParameter ')'    Block
/.$putCase consumeStatementCatch() ; $break ./
/:$readableName CatchClause:/

Finally ::= 'finally'    Block
/:$readableName Finally:/

--18.12 Productions from 14: Expressions

--for source positionning purpose
PushLPAREN ::= '('
/.$putCase consumeLeftParen(); $break ./
/:$readableName (:/
PushRPAREN ::= ')'
/.$putCase consumeRightParen(); $break ./
/:$readableName ):/

Primary -> PrimaryNoNewArray
Primary -> ArrayCreationWithArrayInitializer
Primary -> ArrayCreationWithoutArrayInitializer
/:$readableName Expression:/

PrimaryNoNewArray -> Literal
PrimaryNoNewArray ::= 'this'
/.$putCase consumePrimaryNoNewArrayThis(); $break ./

PrimaryNoNewArray ::=  PushLPAREN Expression PushRPAREN 
/.$putCase consumePrimaryNoNewArray(); $break ./

PrimaryNoNewArray -> ClassInstanceCreationExpression
PrimaryNoNewArray -> FieldAccess
--1.1 feature
PrimaryNoNewArray ::= Name '.' 'this'
/.$putCase consumePrimaryNoNewArrayNameThis(); $break ./
PrimaryNoNewArray ::= Name '.' 'super'
/.$putCase consumePrimaryNoNewArrayNameSuper(); $break ./

--1.1 feature
--PrimaryNoNewArray ::= Type '.' 'class'   
--inline Type in the previous rule in order to make the grammar LL1 instead 
-- of LL2. The result is the 3 next rules.
PrimaryNoNewArray ::= Name '.' 'class'
/.$putCase consumePrimaryNoNewArrayName(); $break ./

PrimaryNoNewArray ::= ArrayType '.' 'class'
/.$putCase consumePrimaryNoNewArrayArrayType(); $break ./

PrimaryNoNewArray ::= PrimitiveType '.' 'class'
/.$putCase consumePrimaryNoNewArrayPrimitiveType(); $break ./

PrimaryNoNewArray -> MethodInvocation
PrimaryNoNewArray -> ArrayAccess
/:$readableName Expression:/
--1.1 feature
--
-- In Java 1.0 a ClassBody could not appear at all in a
-- ClassInstanceCreationExpression.
--

AllocationHeader ::= 'new' ClassType '(' ArgumentListopt ')'
/.$putCase consumeAllocationHeader(); $break ./
/:$readableName AllocationHeader:/

ClassInstanceCreationExpression ::= 'new' ClassType '(' ArgumentListopt ')' ClassBodyopt
/.$putCase consumeClassInstanceCreationExpression(); $break ./
--1.1 feature

ClassInstanceCreationExpression ::= Primary '.' 'new' SimpleName '(' ArgumentListopt ')' ClassBodyopt
/.$putCase consumeClassInstanceCreationExpressionQualified() ; $break ./

--1.1 feature
ClassInstanceCreationExpression ::= ClassInstanceCreationExpressionName 'new' SimpleName '(' ArgumentListopt ')' ClassBodyopt
/.$putCase consumeClassInstanceCreationExpressionQualified() ; $break ./
/:$readableName ClassInstanceCreationExpression:/

ClassInstanceCreationExpressionName ::= Name '.'
/.$putCase consumeClassInstanceCreationExpressionName() ; $break ./
/:$readableName ClassInstanceCreationExpressionName:/

ClassBodyopt ::= $empty --test made using null as contents
/.$putCase consumeClassBodyopt(); $break ./
ClassBodyopt ::= EnterAnonymousClassBody ClassBody
/:$readableName ClassBody:/

EnterAnonymousClassBody ::= $empty
/.$putCase consumeEnterAnonymousClassBody(); $break ./
/:$readableName EnterAnonymousClassBody:/

ArgumentList ::= Expression
ArgumentList ::= ArgumentList ',' Expression
/.$putCase consumeArgumentList(); $break ./
/:$readableName ArgumentList:/

ArrayCreationHeader ::= 'new' PrimitiveType DimWithOrWithOutExprs
/.$putCase consumeArrayCreationHeader(); $break ./

ArrayCreationHeader ::= 'new' ClassOrInterfaceType DimWithOrWithOutExprs
/.$putCase consumeArrayCreationHeader(); $break ./
/:$readableName ArrayCreationHeader:/

ArrayCreationWithoutArrayInitializer ::= 'new' PrimitiveType DimWithOrWithOutExprs
/.$putCase consumeArrayCreationExpressionWithoutInitializer(); $break ./
/:$readableName ArrayCreationWithoutArrayInitializer:/

ArrayCreationWithArrayInitializer ::= 'new' PrimitiveType DimWithOrWithOutExprs ArrayInitializer
/.$putCase consumeArrayCreationExpressionWithInitializer(); $break ./
/:$readableName ArrayCreationWithArrayInitializer:/

ArrayCreationWithoutArrayInitializer ::= 'new' ClassOrInterfaceType DimWithOrWithOutExprs
/.$putCase consumeArrayCreationExpressionWithoutInitializer(); $break ./

ArrayCreationWithArrayInitializer ::= 'new' ClassOrInterfaceType DimWithOrWithOutExprs ArrayInitializer
/.$putCase consumeArrayCreationExpressionWithInitializer(); $break ./

DimWithOrWithOutExprs ::= DimWithOrWithOutExpr
DimWithOrWithOutExprs ::= DimWithOrWithOutExprs DimWithOrWithOutExpr
/.$putCase consumeDimWithOrWithOutExprs(); $break ./
/:$readableName Dimensions:/

DimWithOrWithOutExpr ::= '[' Expression ']'
DimWithOrWithOutExpr ::= '[' ']'
/. $putCase consumeDimWithOrWithOutExpr(); $break ./
/:$readableName Dimension:/
-- -----------------------------------------------

Dims ::= DimsLoop
/. $putCase consumeDims(); $break ./
/:$readableName Dimensions:/
DimsLoop -> OneDimLoop
DimsLoop ::= DimsLoop OneDimLoop
/:$readableName Dimensions:/
OneDimLoop ::= '[' ']'
/. $putCase consumeOneDimLoop(); $break ./
/:$readableName Dimension:/

FieldAccess ::= Primary '.' JavaIdentifier -- AspectJ Extension (was 'Identifier')
/.$putCase consumeFieldAccess(false); $break ./

FieldAccess ::= 'super' '.' JavaIdentifier -- AspectJ Extension (was 'Identifier')
/.$putCase consumeFieldAccess(true); $break ./
/:$readableName FieldAccess:/

MethodInvocation ::= NameOrAj '(' ArgumentListopt ')' -- AspectJ Extension (was Name)
/.$putCase consumeMethodInvocationName(); $break ./

MethodInvocation ::= Primary '.' JavaIdentifier '(' ArgumentListopt ')' -- AspectJ Extension (was 'Identifier')
/.$putCase consumeMethodInvocationPrimary(); $break ./

MethodInvocation ::= 'super' '.' JavaIdentifier '(' ArgumentListopt ')' -- AspectJ Extension (was 'Identifier')
/.$putCase consumeMethodInvocationSuper(); $break ./
/:$readableName MethodInvocation:/

ArrayAccess ::= Name '[' Expression ']'
/.$putCase consumeArrayAccess(true); $break ./
ArrayAccess ::= PrimaryNoNewArray '[' Expression ']'
/.$putCase consumeArrayAccess(false); $break ./
ArrayAccess ::= ArrayCreationWithArrayInitializer '[' Expression ']'
/.$putCase consumeArrayAccess(false); $break ./
/:$readableName ArrayAccess:/

PostfixExpression -> Primary
PostfixExpression ::= NameOrAj -- AspectJ Extension (was Name)
/.$putCase consumePostfixExpression(); $break ./
PostfixExpression -> PostIncrementExpression
PostfixExpression -> PostDecrementExpression
/:$readableName Expression:/

PostIncrementExpression ::= PostfixExpression '++'
/.$putCase consumeUnaryExpression(OperatorIds.PLUS,true); $break ./
/:$readableName PostIncrementExpression:/

PostDecrementExpression ::= PostfixExpression '--'
/.$putCase consumeUnaryExpression(OperatorIds.MINUS,true); $break ./
/:$readableName PostDecrementExpression:/

--for source managment purpose
PushPosition ::= $empty
 /.$putCase consumePushPosition(); $break ./
/:$readableName PushPosition:/

UnaryExpression -> PreIncrementExpression
UnaryExpression -> PreDecrementExpression
UnaryExpression ::= '+' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.PLUS); $break ./
UnaryExpression ::= '-' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.MINUS); $break ./
UnaryExpression -> UnaryExpressionNotPlusMinus
/:$readableName Expression:/

PreIncrementExpression ::= '++' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.PLUS,false); $break ./
/:$readableName PreIncrementExpression:/

PreDecrementExpression ::= '--' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.MINUS,false); $break ./
/:$readableName PreDecrementExpression:/

UnaryExpressionNotPlusMinus -> PostfixExpression
UnaryExpressionNotPlusMinus ::= '~' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.TWIDDLE); $break ./
UnaryExpressionNotPlusMinus ::= '!' PushPosition UnaryExpression
/.$putCase consumeUnaryExpression(OperatorIds.NOT); $break ./
UnaryExpressionNotPlusMinus -> CastExpression
/:$readableName Expression:/

CastExpression ::= PushLPAREN PrimitiveType Dimsopt PushRPAREN InsideCastExpression UnaryExpression
/.$putCase consumeCastExpression(); $break ./
 CastExpression ::= PushLPAREN Name Dims PushRPAREN InsideCastExpression UnaryExpressionNotPlusMinus
/.$putCase consumeCastExpression(); $break ./
-- Expression is here only in order to make the grammar LL1
CastExpression ::= PushLPAREN Expression PushRPAREN InsideCastExpressionLL1 UnaryExpressionNotPlusMinus
/.$putCase consumeCastExpressionLL1(); $break ./
/:$readableName CastExpression:/

InsideCastExpression ::= $empty
/.$putCase consumeInsideCastExpression(); $break ./
/:$readableName InsideCastExpression:/
InsideCastExpressionLL1 ::= $empty
/.$putCase consumeInsideCastExpressionLL1(); $break ./
/:$readableName InsideCastExpression:/

MultiplicativeExpression -> UnaryExpression
MultiplicativeExpression ::= MultiplicativeExpression '*' UnaryExpression
/.$putCase consumeBinaryExpression(OperatorIds.MULTIPLY); $break ./
MultiplicativeExpression ::= MultiplicativeExpression '/' UnaryExpression
/.$putCase consumeBinaryExpression(OperatorIds.DIVIDE); $break ./
MultiplicativeExpression ::= MultiplicativeExpression '%' UnaryExpression
/.$putCase consumeBinaryExpression(OperatorIds.REMAINDER); $break ./
/:$readableName Expression:/

AdditiveExpression -> MultiplicativeExpression
AdditiveExpression ::= AdditiveExpression '+' MultiplicativeExpression
/.$putCase consumeBinaryExpression(OperatorIds.PLUS); $break ./
AdditiveExpression ::= AdditiveExpression '-' MultiplicativeExpression
/.$putCase consumeBinaryExpression(OperatorIds.MINUS); $break ./
/:$readableName Expression:/

ShiftExpression -> AdditiveExpression
ShiftExpression ::= ShiftExpression '<<'  AdditiveExpression
/.$putCase consumeBinaryExpression(OperatorIds.LEFT_SHIFT); $break ./
ShiftExpression ::= ShiftExpression '>>'  AdditiveExpression
/.$putCase consumeBinaryExpression(OperatorIds.RIGHT_SHIFT); $break ./
ShiftExpression ::= ShiftExpression '>>>' AdditiveExpression
/.$putCase consumeBinaryExpression(OperatorIds.UNSIGNED_RIGHT_SHIFT); $break ./
/:$readableName Expression:/

RelationalExpression -> ShiftExpression
RelationalExpression ::= RelationalExpression '<'  ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.LESS); $break ./
RelationalExpression ::= RelationalExpression '>'  ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.GREATER); $break ./
RelationalExpression ::= RelationalExpression '<=' ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.LESS_EQUAL); $break ./
RelationalExpression ::= RelationalExpression '>=' ShiftExpression
/.$putCase consumeBinaryExpression(OperatorIds.GREATER_EQUAL); $break ./
RelationalExpression ::= RelationalExpression 'instanceof' ReferenceType
/.$putCase consumeInstanceOfExpression(OperatorIds.INSTANCEOF); $break ./
/:$readableName Expression:/

EqualityExpression -> RelationalExpression
EqualityExpression ::= EqualityExpression '==' RelationalExpression
/.$putCase consumeEqualityExpression(OperatorIds.EQUAL_EQUAL); $break ./
EqualityExpression ::= EqualityExpression '!=' RelationalExpression
/.$putCase consumeEqualityExpression(OperatorIds.NOT_EQUAL); $break ./
/:$readableName Expression:/

AndExpression -> EqualityExpression
AndExpression ::= AndExpression '&' EqualityExpression
/.$putCase consumeBinaryExpression(OperatorIds.AND); $break ./
/:$readableName Expression:/

ExclusiveOrExpression -> AndExpression
ExclusiveOrExpression ::= ExclusiveOrExpression '^' AndExpression
/.$putCase consumeBinaryExpression(OperatorIds.XOR); $break ./
/:$readableName Expression:/

InclusiveOrExpression -> ExclusiveOrExpression
InclusiveOrExpression ::= InclusiveOrExpression '|' ExclusiveOrExpression
/.$putCase consumeBinaryExpression(OperatorIds.OR); $break ./
/:$readableName Expression:/

ConditionalAndExpression -> InclusiveOrExpression
ConditionalAndExpression ::= ConditionalAndExpression '&&' InclusiveOrExpression
/.$putCase consumeBinaryExpression(OperatorIds.AND_AND); $break ./
/:$readableName Expression:/

ConditionalOrExpression -> ConditionalAndExpression
ConditionalOrExpression ::= ConditionalOrExpression '||' ConditionalAndExpression
/.$putCase consumeBinaryExpression(OperatorIds.OR_OR); $break ./
/:$readableName Expression:/

ConditionalExpression -> ConditionalOrExpression
ConditionalExpression ::= ConditionalOrExpression '?' Expression ':' ConditionalExpression
/.$putCase consumeConditionalExpression(OperatorIds.QUESTIONCOLON) ; $break ./
/:$readableName Expression:/

AssignmentExpression -> ConditionalExpression
AssignmentExpression -> Assignment
/:$readableName Expression:/

Assignment ::= PostfixExpression AssignmentOperator AssignmentExpression
/.$putCase consumeAssignment(); $break ./
/:$readableName Assignment:/

-- this rule is added to parse an array initializer in a assigment and then report a syntax error knowing the exact senario
InvalidArrayInitializerAssignement ::= PostfixExpression AssignmentOperator ArrayInitializer
/:$readableName ArrayInitializerAssignement:/
Assignment ::= InvalidArrayInitializerAssignement
/.$putcase ignoreExpressionAssignment();$break ./

AssignmentOperator ::= '='
/.$putCase consumeAssignmentOperator(EQUAL); $break ./
AssignmentOperator ::= '*='
/.$putCase consumeAssignmentOperator(MULTIPLY); $break ./
AssignmentOperator ::= '/='
/.$putCase consumeAssignmentOperator(DIVIDE); $break ./
AssignmentOperator ::= '%='
/.$putCase consumeAssignmentOperator(REMAINDER); $break ./
AssignmentOperator ::= '+='
/.$putCase consumeAssignmentOperator(PLUS); $break ./
AssignmentOperator ::= '-='
/.$putCase consumeAssignmentOperator(MINUS); $break ./
AssignmentOperator ::= '<<='
/.$putCase consumeAssignmentOperator(LEFT_SHIFT); $break ./
AssignmentOperator ::= '>>='
/.$putCase consumeAssignmentOperator(RIGHT_SHIFT); $break ./
AssignmentOperator ::= '>>>='
/.$putCase consumeAssignmentOperator(UNSIGNED_RIGHT_SHIFT); $break ./
AssignmentOperator ::= '&='
/.$putCase consumeAssignmentOperator(AND); $break ./
AssignmentOperator ::= '^='
/.$putCase consumeAssignmentOperator(XOR); $break ./
AssignmentOperator ::= '|='
/.$putCase consumeAssignmentOperator(OR); $break ./
/:$readableName AssignmentOperator:/

Expression -> AssignmentExpression
/:$readableName Expression:/

ConstantExpression -> Expression
/:$readableName ConstantExpression:/

-- The following rules are for optional nonterminals.
--

PackageDeclarationopt -> $empty 
PackageDeclarationopt -> PackageDeclaration
/:$readableName PackageDeclaration:/

ClassHeaderExtendsopt ::= $empty
ClassHeaderExtendsopt -> ClassHeaderExtends
/:$readableName ClassHeaderExtends:/

Expressionopt ::= $empty
/.$putCase consumeEmptyExpression(); $break ./
Expressionopt -> Expression
/:$readableName Expression:/


---------------------------------------------------------------------------------------
--
-- The rules below are for optional terminal symbols.  An optional comma,
-- is only used in the context of an array initializer - It is a
-- "syntactic sugar" that otherwise serves no other purpose. By contrast,
-- an optional identifier is used in the definition of a break and 
-- continue statement. When the identifier does not appear, a NULL
-- is produced. When the identifier is present, the user should use the
-- corresponding TOKEN(i) method. See break statement as an example.
--
---------------------------------------------------------------------------------------

,opt -> $empty
,opt -> ,
/:$readableName ,:/

ImportDeclarationsopt ::= $empty
/.$putCase consumeEmptyImportDeclarationsopt(); $break ./
ImportDeclarationsopt ::= ImportDeclarations
/.$putCase consumeImportDeclarationsopt(); $break ./
/:$readableName ImportDeclarations:/

TypeDeclarationsopt ::= $empty
/.$putCase consumeEmptyTypeDeclarationsopt(); $break ./
TypeDeclarationsopt ::= TypeDeclarations
/.$putCase consumeTypeDeclarationsopt(); $break ./
/:$readableName TypeDeclarations:/

ClassBodyDeclarationsopt ::= $empty
/.$putCase consumeEmptyClassBodyDeclarationsopt(); $break ./
ClassBodyDeclarationsopt ::= NestedType ClassBodyDeclarations
/.$putCase consumeClassBodyDeclarationsopt(); $break ./
/:$readableName ClassBodyDeclarations:/

Modifiersopt ::= $empty 
/. $putCase consumeDefaultModifiers(); $break ./
Modifiersopt ::= Modifiers 
/.$putCase consumeModifiers(); $break ./ 
/:$readableName Modifiers:/

BlockStatementsopt ::= $empty
/.$putCase consumeEmptyBlockStatementsopt(); $break ./
BlockStatementsopt -> BlockStatements
/:$readableName BlockStatements:/

Dimsopt ::= $empty
/. $putCase consumeEmptyDimsopt(); $break ./
Dimsopt -> Dims
/:$readableName Dimensions:/

ArgumentListopt ::= $empty
/. $putCase consumeEmptyArgumentListopt(); $break ./
ArgumentListopt -> ArgumentList
/:$readableName ArgumentList:/

MethodHeaderThrowsClauseopt ::= $empty
MethodHeaderThrowsClauseopt -> MethodHeaderThrowsClause
/:$readableName MethodHeaderThrowsClause:/

FormalParameterListopt ::= $empty
/.$putcase consumeFormalParameterListopt(); $break ./
FormalParameterListopt -> FormalParameterList
/:$readableName FormalParameterList:/

ClassHeaderImplementsopt ::= $empty
ClassHeaderImplementsopt -> ClassHeaderImplements
/:$readableName ClassHeaderImplements:/

InterfaceMemberDeclarationsopt ::= $empty
/. $putCase consumeEmptyInterfaceMemberDeclarationsopt(); $break ./
InterfaceMemberDeclarationsopt ::= NestedType InterfaceMemberDeclarations
/. $putCase consumeInterfaceMemberDeclarationsopt(); $break ./
/:$readableName InterfaceMemberDeclarations:/

NestedType ::= $empty 
/.$putCase consumeNestedType(); $break./
/:$readableName NestedType:/

ForInitopt ::= $empty
/. $putCase consumeEmptyForInitopt(); $break ./
ForInitopt -> ForInit
/:$readableName ForInit:/

ForUpdateopt ::= $empty
/. $putCase consumeEmptyForUpdateopt(); $break ./
ForUpdateopt -> ForUpdate
/:$readableName ForUpdate:/

InterfaceHeaderExtendsopt ::= $empty
InterfaceHeaderExtendsopt -> InterfaceHeaderExtends
/:$readableName InterfaceHeaderExtends:/

Catchesopt ::= $empty
/. $putCase consumeEmptyCatchesopt(); $break ./
Catchesopt -> Catches
/:$readableName Catches:/

/.	}
} ./

---------------------------------------------------------------------------------------

$names

-- BodyMarker ::= '"class Identifier { ... MethodHeader "'

-- void ::= 'void'

PLUS_PLUS ::=    '++'   
MINUS_MINUS ::=    '--'   
EQUAL_EQUAL ::=    '=='   
LESS_EQUAL ::=    '<='   
GREATER_EQUAL ::=    '>='   
NOT_EQUAL ::=    '!='   
LEFT_SHIFT ::=    '<<'   
RIGHT_SHIFT ::=    '>>'   
UNSIGNED_RIGHT_SHIFT ::=    '>>>'  
PLUS_EQUAL ::=    '+='   
MINUS_EQUAL ::=    '-='   
MULTIPLY_EQUAL ::=    '*='   
DIVIDE_EQUAL ::=    '/='   
AND_EQUAL ::=    '&='   
OR_EQUAL ::=    '|='   
XOR_EQUAL ::=    '^='   
REMAINDER_EQUAL ::=    '%='   
LEFT_SHIFT_EQUAL ::=    '<<='  
RIGHT_SHIFT_EQUAL ::=    '>>='  
UNSIGNED_RIGHT_SHIFT_EQUAL ::=    '>>>=' 
OR_OR ::=    '||'   
AND_AND ::=    '&&'   

PLUS ::=    '+'    
MINUS ::=    '-'    
NOT ::=    '!'    
REMAINDER ::=    '%'    
XOR ::=    '^'    
AND ::=    '&'    
MULTIPLY ::=    '*'    
OR ::=    '|'    
TWIDDLE ::=    '~'    
DIVIDE ::=    '/'    
GREATER ::=    '>'    
LESS ::=    '<'    
LPAREN ::=    '('    
RPAREN ::=    ')'    
LBRACE ::=    '{'    
RBRACE ::=    '}'    
LBRACKET ::=    '['    
RBRACKET ::=    ']'    
SEMICOLON ::=    ';'    
QUESTION ::=    '?'    
COLON ::=    ':'    
COMMA ::=    ','    
DOT ::=    '.'    
EQUAL ::=    '='    

$end
-- need a carriage return after the $end
