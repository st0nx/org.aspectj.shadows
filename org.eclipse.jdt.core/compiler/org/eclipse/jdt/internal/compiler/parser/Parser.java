/* *******************************************************************
 * Copyright (c) 2002,2003 Palo Alto Research Center, Incorporated (PARC).
 *               2004, contributors
 * All rights reserved. 
 * This program and the accompanying materials are made available 
 * under the terms of the Common Public License v1.0 
 * which accompanies this distribution and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html 
 *  
 * Contributors: 
 *     PARC     initial implementation
 *     Adrian Colyer refactored for use in org.eclipse.jdt.core package 
 * ******************************************************************/


package org.eclipse.jdt.internal.compiler.parser;

import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.internal.compiler.CompilationResult;
import org.eclipse.jdt.internal.compiler.ast.AbstractVariableDeclaration;
import org.eclipse.jdt.internal.compiler.ast.Annotation;
import org.eclipse.jdt.internal.compiler.ast.Argument;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.ConstructorDeclaration;
import org.eclipse.jdt.internal.compiler.ast.ExplicitConstructorCall;
import org.eclipse.jdt.internal.compiler.ast.Expression;
import org.eclipse.jdt.internal.compiler.ast.Literal;
import org.eclipse.jdt.internal.compiler.ast.MessageSend;
import org.eclipse.jdt.internal.compiler.ast.MethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeParameter;
//import org.eclipse.jdt.internal.compiler.ast.OperatorExpression;
import org.eclipse.jdt.internal.compiler.ast.OperatorIds;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter;
import org.eclipse.jdt.internal.compiler.problem.ProblemSeverities;
import org.eclipse.jdt.core.compiler.CharOperation;


// AspectJ Extension - this whole class is an AspectJ extension to the parser
public class Parser extends TheOriginalJDTParserClass {
	
	private static final String ASPECTJ_DECLARATION_FACTORY = "org.aspectj.ajdt.internal.compiler.parser.DeclarationFactory";
	private static IDeclarationFactory declarationFactory;
	
	static {
		try{
			initTables(Parser.class);
			declarationFactory = (IDeclarationFactory) Class.forName(ASPECTJ_DECLARATION_FACTORY).newInstance();
		} catch(java.io.IOException ex){
			throw new ExceptionInInitializerError(ex.getMessage());
		} catch (InstantiationException ex) {
			throw new ExceptionInInitializerError(ex.getMessage());
		} catch (IllegalAccessException ex) {
			throw new ExceptionInInitializerError(ex.getMessage());
		} catch (ClassNotFoundException ex) {
			System.err.println("Warning: AspectJ declaration factory class not found on classpath");
			//throw new ExceptionInInitializerError(ex.getMessage());
		}
	}

	public interface IDeclarationFactory {
			MessageSend createProceed(MessageSend m);
			TypeDeclaration createAspect(CompilationResult result);
			void setPrivileged(TypeDeclaration aspectDecl, boolean isPrivileged);
			void setPerClauseFrom(TypeDeclaration aspectDecl, ASTNode pseudoTokens, Parser parser);
			void setDominatesPatternFrom(TypeDeclaration aspectDecl, ASTNode pseudoTokens, Parser parser);
			ASTNode createPseudoTokensFrom(ASTNode[] tokens, CompilationResult result);
			MethodDeclaration createMethodDeclaration(CompilationResult result);
			ConstructorDeclaration createConstructorDeclaration(CompilationResult result);
			MethodDeclaration createPointcutDeclaration(CompilationResult result);
			MethodDeclaration createAroundAdviceDeclaration(CompilationResult result);
			MethodDeclaration createAfterAdviceDeclaration(CompilationResult result);
			MethodDeclaration createBeforeAdviceDeclaration(CompilationResult result);
			ASTNode createPointcutDesignator(Parser parser, ASTNode pseudoTokens);
			void setPointcutDesignatorOnAdvice(MethodDeclaration adviceDecl, ASTNode des);
			void setPointcutDesignatorOnPointcut(MethodDeclaration adviceDecl, ASTNode des);
			void setExtraArgument(MethodDeclaration adviceDeclaration, Argument arg);
			boolean isAfterAdvice(MethodDeclaration adviceDecl);
			void setAfterThrowingAdviceKind(MethodDeclaration adviceDecl);
			void setAfterReturningAdviceKind(MethodDeclaration adviceDecl);
			MethodDeclaration createDeclareDeclaration(CompilationResult result, ASTNode pseudoTokens, Parser parser);
			MethodDeclaration createDeclareAnnotationDeclaration(CompilationResult result, ASTNode pseudoTokens, Annotation annotation, Parser parser);
			MethodDeclaration createInterTypeFieldDeclaration(CompilationResult result, TypeReference onType);
			MethodDeclaration createInterTypeMethodDeclaration(CompilationResult result);
			MethodDeclaration createInterTypeConstructorDeclaration(CompilationResult result);
			void setSelector(MethodDeclaration interTypeDecl, char[] selector);
			void setDeclaredModifiers(MethodDeclaration interTypeDecl, int modifiers);
			void setInitialization(MethodDeclaration itdFieldDecl, Expression initialization);
			void setOnType(MethodDeclaration interTypeDecl, TypeReference onType);
			ASTNode createPseudoToken(Parser parser, String value, boolean isIdentifier);
			ASTNode createIfPseudoToken(Parser parser, Expression expr);
			void setLiteralKind(ASTNode pseudoToken, String string);
			boolean shouldTryToRecover(ASTNode node);
	}
	
//	public final static void initAjTables(Class parserClass)
//		throws java.io.IOException {
//
//		final String prefix = FILEPREFIX;
//		int i = 0;
//		lhsStatic = readTable(parserClass, prefix + (++i) + ".rsc"); //$NON-NLS-1$
//		char[] chars = readTable(parserClass, prefix + (++i) + ".rsc"); //$NON-NLS-1$
//		check_tableStatic = new short[chars.length];
//		for (int c = chars.length; c-- > 0;) {
//			check_tableStatic[c] = (short) (chars[c] - 32768);
//		}
//		asbStatic = readTable(parserClass, prefix + (++i) + ".rsc"); //$NON-NLS-1$
//		asrStatic = readTable(parserClass, prefix + (++i) + ".rsc"); //$NON-NLS-1$
//		symbol_indexStatic = readTable(parserClass, prefix + (++i) + ".rsc"); //$NON-NLS-1$
//		actionStatic = lhsStatic;
//	}

	//positions , dimensions , .... (int stacks)
	protected int aspectIntPtr;
	protected int[] aspectIntStack;

	/* (non-Javadoc)
	 * @see org.eclipse.jdt.internal.compiler.parser.TheOriginalJDTParserClass#initialize()
	 */
	public void initialize() {
		super.initialize();
		aspectIntPtr = -1;
		aspectIntStack = new int[4];
	}
	
	public void initializeScanner(){
		this.scanner = new Scanner(
			false /*comment*/, 
			false /*whitespace*/, 
			this.options.getSeverity(CompilerOptions.NonExternalizedString) != ProblemSeverities.Ignore /*nls*/, 
			this.options.sourceLevel /*sourceLevel*/, 
			this.options.taskTags/*taskTags*/,
			this.options.taskPriorites/*taskPriorities*/,
			this.options.isTaskCaseSensitive/*taskCaseSensitive*/);
	}
	
	
	
//*************New display debugging method
	private static final boolean AJ_DEBUG = false;

	void println(Object o) {
		if (AJ_DEBUG) System.out.println(o);
	}

	private void printStack(Object[] s, int p) {
		List list = Arrays.asList(s);
		System.out.println("  " + list.subList(0, p+1));
	}
	
	private void printStack(int[] s, int p) {
		StringBuffer buf = new StringBuffer("[");
		for (int i=0; i<p+1; i++) {
			if (i > 0) buf.append(", ");
			buf.append(Integer.toString(s[i]));
		}
		buf.append("]");
		System.out.println("  " + buf);
	}
			
	private void printStack(long[] s, int p) {
		StringBuffer buf = new StringBuffer("[");
		for (int i=0; i<p+1; i++) {
			if (i > 0) buf.append(", ");
			buf.append(Long.toString(s[i]));
		}
		buf.append("]");
		System.out.println("  " + buf);
	}
			
	private void printStack(char[][] s, int p) {
		StringBuffer buf = new StringBuffer("[");
		for (int i=0; i<p+1; i++) {
			if (i > 0) buf.append(", ");
			buf.append(new String(s[i]));
		}
		buf.append("]");
		System.out.println("  " + buf);
	}
			
	public void display() {
		if (!AJ_DEBUG) return;
		System.out.print("astStack: ");
		printStack(astStack, astPtr);
		System.out.print("astLengthStack: ");
		printStack(astLengthStack, astLengthPtr);
		
		System.out.print("expressionStack: ");
		printStack(expressionStack, expressionPtr);
		System.out.print("expressionLengthStack: ");
		printStack(expressionLengthStack, expressionLengthPtr);

		System.out.print("identifierStack: ");
		printStack(identifierStack, identifierPtr);
		System.out.print("identifierLengthStack: ");
		printStack(identifierLengthStack, identifierLengthPtr);
		System.out.print("identifierPositionStack: ");
		printStack(identifierPositionStack, identifierPtr);

		
		System.out.print("intStack:");
		printStack(intStack, intPtr);
		System.out.println();
	}	



//************** Overriding behavior for standard Java rules
	protected MethodDeclaration createMethodDeclaration(CompilationResult result) {
		return declarationFactory.createMethodDeclaration(result);
	}
	
	protected ConstructorDeclaration createConstructorDeclaration(CompilationResult result) {
		return declarationFactory.createConstructorDeclaration(result);
	}
	
	protected void consumeMethodInvocationName() {
		super.consumeMethodInvocationName();

		MessageSend m = (MessageSend)expressionStack[expressionPtr];
		if (CharOperation.equals(m.selector, "proceed".toCharArray())) {
			expressionStack[expressionPtr] = declarationFactory.createProceed(m);
		}
	}
	
	protected void consumeToken(int type) {
		currentTokenStart = scanner.startPosition;
		super.consumeToken(type);
		switch (type) {
			case TokenNameaspect :  // pseudo keyword
				//aspectIntPtr = -1; //XXX  If we ever see a bug with aspects nested in aspects,
                //                   // this line is the culprit!
				pushOnAspectIntStack(this.scanner.currentPosition - 1);
				pushOnAspectIntStack(this.scanner.startPosition);
				// deliberate fall through...
			case TokenNameprivileged :  // pseudo keyword
			case TokenNamepointcut :  // pseudo keyword
			case TokenNamebefore :  // pseudo keyword
			case TokenNameafter :  // pseudo keyword
			case TokenNamearound :  // pseudo keyword
			case TokenNamedeclare :  // pseudo keyword
				pushIdentifier();
				scanner.commentPtr = -1;
				break;
		}
	}


//************New AspectJ rules	
	protected void consumeAspectDeclaration() {
	    // AspectDeclaration ::= AspectHeader AspectBody
	    consumeClassDeclaration();
	    //??? post parsing step here
	}
	
	protected void consumeAspectHeader() {
	    // AspectHeader ::= AspectHeaderName ClassHeaderExtendsopt ClassHeaderImplementsopt AspectHeaderRest
		consumeClassHeader();
	}

	protected void consumeAspectHeaderName(boolean isPrivileged) {
		// (isPrivileged == false) -> AspectHeaderName ::= Modifiersopt 'aspect' 'Identifier'
		// (isPrivileged == true) -> AspectHeaderName ::= Modifiersopt 'privileged' Modifiersopt 'aspect' 'Identifier'
		TypeDeclaration aspectDecl = declarationFactory.createAspect(this.compilationUnit.compilationResult);
		if (this.nestedMethod[this.nestedType] == 0) {
			if (this.nestedType != 0) {
				aspectDecl.bits |= ASTNode.IsMemberTypeMASK;
			}
		} else {
			// Record that the block has a declaration for local types
			aspectDecl.bits |= ASTNode.IsLocalTypeMASK;
			markEnclosingMemberWithLocalType();
			blockReal();
		}			

		println("aspect header name: ");
		this.display();

		//highlight the name of the type
		long pos = identifierPositionStack[identifierPtr];
		aspectDecl.sourceEnd = (int) pos;
		aspectDecl.sourceStart = (int) (pos >>> 32);
		aspectDecl.name = identifierStack[identifierPtr--];
		identifierLengthPtr--;

		//compute the declaration source too
		// 'class' and 'interface' push two int positions: the beginning of the class token and its end.
		// we want to keep the beginning position but get rid of the end position
		// it is only used for the ClassLiteralAccess positions.
		aspectDecl.declarationSourceStart = this.aspectIntStack[this.aspectIntPtr--]; 
		this.aspectIntPtr--; // remove the end position of the class token

		// pop the aspect pseudo-token
		eatIdentifier();


		// handle modifiers, only without privileged for now
		if (isPrivileged) {
			pos = eatIdentifier(); // eat the privileged
//			int end = (int) pos;
//		    int start = (int) (pos >>> 32);
		    declarationFactory.setPrivileged(aspectDecl,true);
			//problemReporter().signalError(start, end, "privileged is unimplemented in 1.1alpha1");
		}
		aspectDecl.modifiersSourceStart = intStack[intPtr--];
		aspectDecl.modifiers = intStack[intPtr--];
		if (isPrivileged) {
			aspectDecl.modifiersSourceStart = intStack[intPtr--];
			aspectDecl.modifiers |= intStack[intPtr--];
		}
		if (aspectDecl.modifiersSourceStart >= 0) {
			aspectDecl.declarationSourceStart = aspectDecl.modifiersSourceStart;
		}

		println("modifiers: " + aspectDecl.modifiers);

		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack, 
				(this.expressionPtr -= length) + 1, 
				aspectDecl.annotations = new Annotation[length], 
				0, 
				length); 
		}

		aspectDecl.bodyStart = aspectDecl.sourceEnd + 1;
		pushOnAstStack(aspectDecl);

		listLength = 0; // will be updated when reading super-interfaces
		// recovery
		if (currentElement != null) {
			lastCheckPoint = aspectDecl.bodyStart;
			currentElement = currentElement.add(aspectDecl, 0);
			lastIgnoredToken = -1;
		}

        // Grab the javadoc
        aspectDecl.javadoc = this.javadoc;
        this.javadoc = null;

		this.display();
	}

	protected void consumeAspectHeaderNameWithTypeParameters(boolean isPriviliged) {
		TypeDeclaration typeDecl = (TypeDeclaration)this.astStack[this.astPtr];

		// consume type parameters
		int length = this.genericsLengthStack[this.genericsLengthPtr--];
		this.genericsPtr -= length;
		System.arraycopy(this.genericsStack, this.genericsPtr + 1, typeDecl.typeParameters = new TypeParameter[length], 0, length);

		typeDecl.bodyStart = typeDecl.typeParameters[length-1].declarationSourceEnd + 1;
		
		this.listTypeParameterLength = 0;
		
		if (this.currentElement != null) { // is recovering
			this.lastCheckPoint = typeDecl.bodyStart;
		}
	}
	
	private long eatIdentifier() {
		long pos = identifierPositionStack[identifierPtr];
		identifierPtr--;
		identifierLengthPtr--;
		return pos;
	}

	protected void consumeAspectHeaderRest() {
		//--[dominates TypePattern] [persingleton() | percflow(PCD) | perthis(PCD) | pertarget(PCD)]
		//AspectHeaderRest ::= AspectHeaderRestStart PseudoTokens
		concatNodeLists();
		this.display();
		ASTNode pseudoTokens = popPseudoTokens("{");
		println("pseudo: " + pseudoTokens);

		TypeDeclaration aspectDecl = (TypeDeclaration) astStack[astPtr];
		
		declarationFactory.setDominatesPatternFrom(aspectDecl,pseudoTokens,this);
		declarationFactory.setPerClauseFrom(aspectDecl,pseudoTokens,this);
		// XXX handle dominates
	}
	
	
	protected void consumePointcutDeclaration() {
		consumePointcutDesignatorOnDeclaration();
	}
	
	protected void consumeEmptyPointcutDeclaration() {
		//??? set pcd to non-null
	}
	
	protected void consumePointcutHeader() {
		//PointcutDeclaration ::= Modifiersopt 'pointcut'  JavaIdentifier '('
		
		MethodDeclaration ret = declarationFactory.createPointcutDeclaration(compilationUnit.compilationResult);
		
		//the name
		long pos = identifierPositionStack[identifierPtr];
//		int sourceEnd = (int) pos;
		ret.sourceStart = (int) (pos >>> 32);
		ret.selector = identifierStack[identifierPtr--];
		identifierLengthPtr--;
        
        // Grab the javadoc
		ret.javadoc = this.javadoc;
        this.javadoc = null;
        
		// pop the 'pointcut' keyword
		eatIdentifier();

		// modifiers
		ret.declarationSourceStart = intStack[intPtr--];
		ret.modifiers = intStack[intPtr--];
		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack, 
				(this.expressionPtr -= length) + 1, 
				ret.annotations = new Annotation[length], 
				0, 
				length); 
		}
		
		pushOnAstStack(ret);
	}
	


	protected void consumeAroundDeclaration() {
		// AroundDeclaration ::= AroundHeader MethodBody
		consumeMethodDeclaration(true);
	}

	protected void consumeAroundHeader() {
		consumePointcutDesignatorOnAdvice();
		consumeMethodHeader();
	}

	protected void consumeAroundHeaderName() {
		// AroundHeaderName ::= Modifiersopt Type  'around' '(' 
		
		MethodDeclaration adviceDecl = declarationFactory.createAroundAdviceDeclaration(compilationUnit.compilationResult);
		
		// skip the name of the advice
		long pos = eatIdentifier();
		adviceDecl.sourceStart = (int) (pos >>> 32);
        
		// but put in a placeholder name
        adviceDecl.selector = new char[] {'a','j','c','$','a','d','v','i','c','e'};

		TypeReference returnType = getTypeReference(intStack[intPtr--]);
		
		//modifiers
		adviceDecl.declarationSourceStart = intStack[intPtr--];
		adviceDecl.modifiers = intStack[intPtr--];

		adviceDecl.returnType = returnType;
		
		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack, 
				(this.expressionPtr -= length) + 1, 
				adviceDecl.annotations = new Annotation[length], 
				0, 
				length); 
		}
        
        // Grab the javadoc
        adviceDecl.javadoc = this.javadoc;
        this.javadoc = null;
		
		//XXX get some locations right
		
		pushOnAstStack(adviceDecl);
	}
	
	protected void consumePointcutDesignatorOnAdvice() {
		ASTNode des = popPointcutDesignator("{");
		MethodDeclaration adviceDecl = (MethodDeclaration)astStack[astPtr];
		declarationFactory.setPointcutDesignatorOnAdvice(adviceDecl,des);
		adviceDecl.sourceEnd = 	des.sourceEnd;
		adviceDecl.bodyStart = des.sourceEnd+1;
	}
	
	protected void consumePointcutDesignatorOnDeclaration() {
		ASTNode des = popPointcutDesignator(";");
		MethodDeclaration pcutDecl = (MethodDeclaration)astStack[astPtr];
		declarationFactory.setPointcutDesignatorOnPointcut(pcutDecl,des);
		pcutDecl.sourceEnd = 	des.sourceEnd;
		pcutDecl.bodyStart = des.sourceEnd+1;
		pcutDecl.bodyEnd = endPosition;
		pcutDecl.declarationSourceEnd = flushCommentsDefinedPriorTo(endStatementPosition);
	}
	
	
	protected void consumeBasicAdviceDeclaration() {
		// BasicAdviceDeclaration ::= BasicAdviceHeader MethodBody
		consumeMethodDeclaration(true);
	}

	protected void consumeBasicAdviceHeader() {
		// BasicAdviceHeader ::= BasicAdviceHeaderName MethodHeaderParameters ExtraParamopt MethodHeaderThrowsClauseopt ':' PseudoTokens
		consumePointcutDesignatorOnAdvice();
		
		consumeMethodHeader();
	}
	
	
	protected void consumeBasicAdviceHeaderName(boolean isAfter) {
		// BasicAdviceHeaderName ::= 'before'|'after '(' 
		
		MethodDeclaration adviceDecl =
			(isAfter ? declarationFactory.createAfterAdviceDeclaration(compilationUnit.compilationResult) :
					  declarationFactory.createBeforeAdviceDeclaration(compilationUnit.compilationResult));
		
        // skip the name of the advice
		long pos = eatIdentifier();
		// but give a placeholder selector name
		adviceDecl.selector = new char[] {'a','j','c','$','a','d','v','i','c','e'};
        adviceDecl.sourceStart = (int) (pos >>> 32);
		
		//modifiers
		adviceDecl.declarationSourceStart = intStack[intPtr--];
		adviceDecl.modifiers = intStack[intPtr--];

		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack, 
				(this.expressionPtr -= length) + 1, 
				adviceDecl.annotations = new Annotation[length], 
				0, 
				length); 
		}

        // Grab the javadoc
        adviceDecl.javadoc = this.javadoc;
        this.javadoc = null;
		
		//??? get more locations right
		
		pushOnAstStack(adviceDecl);
	}
	
	protected void consumeExtraParameterWithFormal() {
		Argument arg = (Argument)astStack[astPtr--];
		astLengthPtr--;
		
		declarationFactory.setExtraArgument((MethodDeclaration)astStack[astPtr],arg);
		
		consumeExtraParameterNoFormal();
	}

	
	protected void consumeExtraParameterNoFormal() {
		
		
	    long pos = identifierPositionStack[identifierPtr];
	    int end = (int) pos;
		int start = (int) (pos >>> 32);
	    char[] name = identifierStack[identifierPtr--];
	    identifierLengthPtr--;
	    
	    //System.out.println("extra parameter: " + new String(name));
	    
	    MethodDeclaration adviceDecl = (MethodDeclaration)astStack[astPtr];
	    if (declarationFactory.isAfterAdvice(adviceDecl)) {
	    	//XXX error, extra param makes no sense here
	    }
	    
	    if (CharOperation.equals(name, "throwing".toCharArray())) {
	    	declarationFactory.setAfterThrowingAdviceKind(adviceDecl);
	    } else if (CharOperation.equals(name, "returning".toCharArray())) {
			declarationFactory.setAfterReturningAdviceKind(adviceDecl);
	    } else {
			problemReporter().parseError(
				start, 
				end, 
				currentToken,
				name, 
				String.valueOf(name), 
				new String[] {"throwing", "returning", ":"}); 
	    }
	}

	protected void consumeClassBodyDeclarationInAspect() { }
	

	protected void consumeDeclareDeclaration() {
		concatNodeLists();
		ASTNode tokens = popPseudoTokens(";");
		MethodDeclaration declareDecl = declarationFactory.createDeclareDeclaration(this.compilationUnit.compilationResult,tokens,this);
//		println("parsed declare: " + declare);
		display();
		pushOnAstStack(declareDecl);
	}


	protected void consumeDeclareAnnotation() {
		concatNodeLists();
		ASTNode tokens = popPseudoTokens(";");

		int length;
		Annotation[] annotations = new Annotation[1]; // there should only ever be one for us...
    	if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
    		System.arraycopy(
    			this.expressionStack, 
    			(this.expressionPtr -= length) + 1, 
    			annotations = new Annotation[length], 
    			0, 
    			length); 
    	}

    	MethodDeclaration declareDecl = declarationFactory.createDeclareAnnotationDeclaration(this.compilationUnit.compilationResult,tokens,annotations[0],this);
    	pushOnAstStack(declareDecl);
	}

	protected void consumeDeclareAnnotationHeader() {
		consumePseudoTokenIdentifier();  // name
		consumePseudoTokenIdentifier();  // declare
		swapAstStack();
		consumePseudoTokens();

		consumePseudoToken("@",0,false);
		swapAstStack();
		consumePseudoTokens();
		
		consumePseudoToken(":", 0, false);
		consumePseudoTokens();

		println(">>>>>>>>>>>>>>>>>>>>>>>declare annotation header");
		display();
	}

	protected void consumeDeclareHeader() {
		consumePseudoTokenIdentifier();  // name
		consumePseudoTokenIdentifier();  // declare
		swapAstStack();
		consumePseudoTokens();
		
		consumePseudoToken(":", 0, false);
		consumePseudoTokens();

		println(">>>>>>>>>>>>>>>>>>>>>>>declare header");
		display();
	}

	protected void consumeEnterITDVariable() {
		println("about to consume field");
		this.display();

		long pos = identifierPositionStack[identifierPtr];
		int end = (int) pos;
		int start = (int) (pos >>> 32);
		char[] identifierName = identifierStack[identifierPtr--];
//		int extendedDimension = this.intStack[this.intPtr--];  // XXXX see consumeEnterVariable for what to do with this
		identifierLengthPtr--;

		//	field.name = name;
		//	field.sourceStart = start;
		//	field.sourceEnd = end;
		consumeClassOrInterfaceName();
		TypeReference onType = getTypeReference(0);
		TypeReference returnType = getTypeReference(intStack[intPtr--]);
		this.display();

		int decSourceStart = intStack[intPtr--];
		int fieldModifiers = intStack[intPtr--];

		MethodDeclaration dec = declarationFactory.createInterTypeFieldDeclaration(
				this.compilationUnit.compilationResult,
				onType);
		
		dec.returnType = returnType;
		dec.sourceStart = start;
		dec.sourceEnd = end;
		declarationFactory.setSelector(dec,identifierName);
		dec.declarationSourceStart = decSourceStart;
		declarationFactory.setDeclaredModifiers(dec,fieldModifiers);
//		declarationFactory.setInitialization(dec,initialization);
		
		dec.bodyEnd = endPosition;
//		dec.declarationSourceEnd = flushCommentsDefinedPriorTo(endStatementPosition);

		// Grab the javadoc
        dec.javadoc = this.javadoc;
        this.javadoc = null;

    	// consume annotations
    	int length;
    	if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
    		System.arraycopy(
    			this.expressionStack, 
    			(this.expressionPtr -= length) + 1, 
    			dec.annotations = new Annotation[length], 
    			0, 
    			length); 
    	}

		pushOnAstStack(dec);
		println("consumed field: " + dec);
		this.display();
}
	protected void consumeExitITDVariableWithoutInitializer() {
		MethodDeclaration itdDecl = (MethodDeclaration) this.astStack[this.astPtr];
		declarationFactory.setInitialization(itdDecl,null);
	}
	protected void consumeExitITDVariableWithInitializer() {
		this.expressionLengthPtr--;
		MethodDeclaration itdDecl = (MethodDeclaration) this.astStack[this.astPtr];
		Expression initialization = this.expressionStack[this.expressionPtr--];
		declarationFactory.setInitialization(itdDecl,initialization);
		// we need to update the declarationSourceEnd of the local variable declaration to the
		// source end position of the initialization expression
		itdDecl.declarationSourceEnd = initialization.sourceEnd;
	}

	protected void consumeInterTypeFieldDeclaration() {
		MethodDeclaration dec = (MethodDeclaration) this.astStack[this.astPtr];

		dec.bodyEnd = endPosition;
		dec.declarationSourceEnd = flushCommentsDefinedPriorTo(endStatementPosition);
	}

	protected void consumeInterTypeMethodDeclaration(boolean isNotAbstract) {
		consumeMethodDeclaration(isNotAbstract);
	}

	protected void consumeInterTypeMethodHeader() {
		consumeMethodHeader();		
	}

	protected void consumeInterTypeConstructorDeclaration() {
		consumeMethodDeclaration(true);
	}

	protected void consumeInterTypeConstructorHeader() {
		consumeMethodHeader();		
	}

	protected void consumeInterTypeMethodHeaderName() {
		//InterTypeMethodHeaderName ::= Modifiersopt Type OnType '.' JavaIdentifier '('
		this.display();
		MethodDeclaration md = declarationFactory.createInterTypeMethodDeclaration(
				this.compilationUnit.compilationResult);

		//identifier
		char[] name = identifierStack[identifierPtr];
		long selectorSource = identifierPositionStack[identifierPtr--];
		identifierLengthPtr--;


		//onType
		consumeClassOrInterfaceName();
		TypeReference onType = getTypeReference(0);
		declarationFactory.setOnType(md,onType);

		//type
		md.returnType = getTypeReference(intStack[intPtr--]);

		//modifiers
		md.declarationSourceStart = intStack[intPtr--];
		declarationFactory.setDeclaredModifiers(md,intStack[intPtr--]);

		//highlight starts at selector start
		md.sourceStart = (int) (selectorSource >>> 32);
		pushOnAstStack(md);
		md.sourceEnd = lParenPos;
		md.bodyStart = lParenPos + 1;
		declarationFactory.setSelector(md,name);
		listLength = 0;
		// initialize listLength before reading parameters/throws
 
 		// Grab the javadoc
        md.javadoc = this.javadoc;
        this.javadoc = null;

    	// consume annotations
    	int length;
    	if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
    		System.arraycopy(
    			this.expressionStack, 
    			(this.expressionPtr -= length) + 1, 
    			md.annotations = new Annotation[length], 
    			0, 
    			length); 
    	}

		// recovery
		if (currentElement != null) {
			if (currentElement instanceof RecoveredType
				//|| md.modifiers != 0
				|| (scanner.getLineNumber(md.returnType.sourceStart)
					== scanner.getLineNumber(md.sourceStart))) {
				lastCheckPoint = md.bodyStart;
				currentElement = currentElement.add(md, 0);
				lastIgnoredToken = -1;
			} else {
				lastCheckPoint = md.sourceStart;
				restartRecovery = true;
			}
		}
	}

	// AMC - bad smell, this method duplicates nearly all logic with the one above. This is exactly the
	// same pattern that the JDT parser follows but I don't like it...
	protected void consumeInterTypeMethodHeaderNameWithTypeParameters() {
		//InterTypeMethodHeaderName ::= Modifiersopt Type OnType '.' JavaIdentifier '('
		this.display();
		MethodDeclaration md = declarationFactory.createInterTypeMethodDeclaration(
				this.compilationUnit.compilationResult);

		//identifier
		char[] name = identifierStack[identifierPtr];
		long selectorSource = identifierPositionStack[identifierPtr--];
		identifierLengthPtr--;


		//onType
		consumeClassOrInterfaceName();
		TypeReference onType = getTypeReference(0);
		declarationFactory.setOnType(md,onType);

		//type
		md.returnType = getTypeReference(intStack[intPtr--]);

		// consume type parameters
		int tp_length = this.genericsLengthStack[this.genericsLengthPtr--];
		this.genericsPtr -= tp_length;
		System.arraycopy(this.genericsStack, this.genericsPtr + 1, md.typeParameters = new TypeParameter[tp_length], 0, tp_length);

		//modifiers
		md.declarationSourceStart = intStack[intPtr--];
		declarationFactory.setDeclaredModifiers(md,intStack[intPtr--]);

		//highlight starts at selector start
		md.sourceStart = (int) (selectorSource >>> 32);
		pushOnAstStack(md);
		md.sourceEnd = lParenPos;
		md.bodyStart = lParenPos + 1;
		declarationFactory.setSelector(md,name);
		listLength = 0;
		// initialize listLength before reading parameters/throws
 
 		// Grab the javadoc
        md.javadoc = this.javadoc;
        this.javadoc = null;

    	// consume annotations
    	int length;
    	if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
    		System.arraycopy(
    			this.expressionStack, 
    			(this.expressionPtr -= length) + 1, 
    			md.annotations = new Annotation[length], 
    			0, 
    			length); 
    	}

		// recovery
		if (currentElement != null) {
			if (currentElement instanceof RecoveredType
				//|| md.modifiers != 0
				|| (scanner.getLineNumber(md.returnType.sourceStart)
					== scanner.getLineNumber(md.sourceStart))) {
				lastCheckPoint = md.bodyStart;
				currentElement = currentElement.add(md, 0);
				lastIgnoredToken = -1;
			} else {
				lastCheckPoint = md.sourceStart;
				restartRecovery = true;
			}
		}
	}
	protected void consumeInterTypeConstructorHeaderName() {
		//InterTypeConstructorHeaderName ::= Modifiersopt Name '.' 'new' '('
		this.display();
		MethodDeclaration md = declarationFactory.createInterTypeConstructorDeclaration(
				this.compilationUnit.compilationResult);

		//identifier
//		md.selector = identifierStack[identifierPtr];
//		long selectorSource = identifierPositionStack[identifierPtr--];
////		identifierLengthPtr--;

		//onType
		consumeClassOrInterfaceName();
		TypeReference onType = getTypeReference(0);
		declarationFactory.setOnType(md,onType);

		println("got onType: " + onType);
		this.display();

		intPtr--; // pop new info
		//type
		md.returnType = TypeReference.baseTypeReference(T_void, 0); //getTypeReference(intStack[intPtr--]);

		//modifiers
		md.declarationSourceStart = intStack[intPtr--];
		declarationFactory.setDeclaredModifiers(md,intStack[intPtr--]);
		//md.modifiers = intStack[intPtr--];

		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack, 
				(this.expressionPtr -= length) + 1, 
				md.annotations = new Annotation[length], 
				0, 
				length); 
		}

		//highlight starts at selector start
		//md.sourceStart = (int) (selectorSource >>> 32);
		md.sourceStart = onType.sourceStart;
		pushOnAstStack(md);
		md.sourceEnd = lParenPos;
		md.bodyStart = lParenPos + 1;
		listLength = 0;
		// initialize listLength before reading parameters/throws

		declarationFactory.setSelector(md,
			(new String(CharOperation.concatWith(onType.getTypeName(), '_')) + "_new").toCharArray());
		

		// recovery
		if (currentElement != null) {
			if (currentElement instanceof RecoveredType
				//|| md.modifiers != 0
				//|| (scanner.getLineNumber(md.returnType.sourceStart)
				//	== scanner.getLineNumber(md.sourceStart))
				) {
				//lastCheckPoint = md.bodyStart;
				currentElement = currentElement.add(md, 0);
				lastIgnoredToken = -1;
			} else {
				lastCheckPoint = md.sourceStart;
				restartRecovery = true;
			}
		}
	}

	protected void consumeInterTypeConstructorHeaderNameWithTypeParameters() {
		//InterTypeConstructorHeaderName ::= Modifiersopt Name '.' 'new' '('
		this.display();
		MethodDeclaration md = declarationFactory.createInterTypeConstructorDeclaration(
				this.compilationUnit.compilationResult);

		//identifier
//		md.selector = identifierStack[identifierPtr];
//		long selectorSource = identifierPositionStack[identifierPtr--];
////		identifierLengthPtr--;

		//onType
		consumeClassOrInterfaceName();
		TypeReference onType = getTypeReference(0);
		declarationFactory.setOnType(md,onType);

		println("got onType: " + onType);
		this.display();

		intPtr--; // pop new info
		//type
		md.returnType = TypeReference.baseTypeReference(T_void, 0); //getTypeReference(intStack[intPtr--]);

		// consume type parameters
		int tp_length = this.genericsLengthStack[this.genericsLengthPtr--];
		this.genericsPtr -= tp_length;
		System.arraycopy(this.genericsStack, this.genericsPtr + 1, md.typeParameters = new TypeParameter[tp_length], 0, tp_length);

		//modifiers
		md.declarationSourceStart = intStack[intPtr--];
		declarationFactory.setDeclaredModifiers(md,intStack[intPtr--]);
		//md.modifiers = intStack[intPtr--];

		// consume annotations
		int length;
		if ((length = this.expressionLengthStack[this.expressionLengthPtr--]) != 0) {
			System.arraycopy(
				this.expressionStack, 
				(this.expressionPtr -= length) + 1, 
				md.annotations = new Annotation[length], 
				0, 
				length); 
		}

		//highlight starts at selector start
		//md.sourceStart = (int) (selectorSource >>> 32);
		md.sourceStart = onType.sourceStart;
		pushOnAstStack(md);
		md.sourceEnd = lParenPos;
		md.bodyStart = lParenPos + 1;
		listLength = 0;
		// initialize listLength before reading parameters/throws

		declarationFactory.setSelector(md,
			(new String(CharOperation.concatWith(onType.getTypeName(), '_')) + "_new").toCharArray());
		

		// recovery
		if (currentElement != null) {
			if (currentElement instanceof RecoveredType
				//|| md.modifiers != 0
				//|| (scanner.getLineNumber(md.returnType.sourceStart)
				//	== scanner.getLineNumber(md.sourceStart))
				) {
				//lastCheckPoint = md.bodyStart;
				currentElement = currentElement.add(md, 0);
				lastIgnoredToken = -1;
			} else {
				lastCheckPoint = md.sourceStart;
				restartRecovery = true;
			}
		}
	}



//*********************************************************


	protected void consumePseudoToken(String value) {
		consumePseudoToken(value, 0, false);
	}

	protected void consumePseudoToken(
		String value,
		int popFromIntStack,
		boolean isIdentifier) {
		intPtr -= popFromIntStack;

		int start = currentTokenStart;
		int end = start + value.length() - 1;
		ASTNode tok = declarationFactory.createPseudoToken(this, value, isIdentifier);
		tok.sourceStart = start;
		tok.sourceEnd = end;
		pushOnAstStack(tok);
	}

	protected void consumePseudoTokenIdentifier() {
		long pos = identifierPositionStack[identifierPtr];
		int end = (int) pos;
		int start = (int) (pos >>> 32);
		char[] name = identifierStack[identifierPtr--];
		identifierLengthPtr--;

		ASTNode tok = declarationFactory.createPseudoToken(this, new String(name), true);
		tok.sourceStart = start;
		tok.sourceEnd = end;
		pushOnAstStack(tok);
	}

	protected void consumePseudoTokenIf() {
		//this.display();
		Expression expr = (Expression) expressionStack[expressionPtr--];
		expressionLengthPtr--;
		println("expr: " + expr);

		int start = intStack[intPtr--];
		ASTNode tok = declarationFactory.createIfPseudoToken(this, expr);
		tok.sourceStart = start;
		tok.sourceEnd = this.rParenPos;
		pushOnAstStack(tok);
	}

	protected void consumePseudoTokenLiteral() {
		Literal literal = (Literal) expressionStack[expressionPtr--];
		expressionLengthPtr--;
		//System.out.println("literal: " + new String(literal.source()));

		ASTNode tok = declarationFactory.createPseudoToken(this, new String(literal.source()), false);
		declarationFactory.setLiteralKind(tok,"string");
		tok.sourceStart = literal.sourceStart;
		tok.sourceEnd = literal.sourceEnd;
		pushOnAstStack(tok);
	}

	protected void consumePseudoTokenModifier() {
		//int modifier = modifiers;
		consumePseudoToken(Modifier.toString(modifiers), 0, true);
		modifiers = AccDefault;
	}

	protected void consumePseudoTokenPrimitiveType() {
		TypeReference type = getTypeReference(0);

		ASTNode tok = declarationFactory.createPseudoToken(this, type.toString(), true);
		tok.sourceStart = type.sourceStart;
		tok.sourceEnd = type.sourceEnd;
		pushOnAstStack(tok);
	}

	protected void consumePseudoTokens() {
		optimizedConcatNodeLists();
	}

// Helpers

	protected ASTNode popPointcutDesignator(String terminator) {
		ASTNode tokens = popPseudoTokens(terminator);
		return declarationFactory.createPointcutDesignator(this, tokens);
	}

	protected ASTNode popPseudoTokens(String terminator) {
		consumePseudoToken(terminator);
		consumePseudoTokens();
		//System.out.println("next token is: " + new String(scanner.getCurrentTokenSource()));

		int length = astLengthStack[astLengthPtr--];
		astPtr -= length;

		//arguments
		ASTNode[] tokens = new ASTNode[length];
		System.arraycopy(astStack, astPtr + 1, tokens, 0, length);
		//md.bodyStart = rParenPos+1;
		listLength = 0; // reset listLength after having read all parameters

		return declarationFactory.createPseudoTokensFrom(tokens,this.compilationUnit.compilationResult()); 
			//	new PseudoTokens(tokens, makeSourceContext(this.compilationUnit.compilationResult()));
	}

//	private ISourceContext makeSourceContext(CompilationResult compilationResult) {
//		return new EclipseSourceContext(compilationResult);
//	}


	private void swapAstStack() {
		ASTNode top = astStack[astPtr];
		ASTNode next = astStack[astPtr-1];
		astStack[astPtr] = next;
		astStack[astPtr-1] = top;
	}

//	 This method is part of an automatic generation : do NOT edit-modify  
	protected void consumeRule(int act) {
	  switch ( act ) {
	    case 33 : if (DEBUG) { System.out.println("Type ::= PrimitiveType"); }  //$NON-NLS-1$
			    consumePrimitiveType();  
				break;
	 
	    case 47 : if (DEBUG) { System.out.println("ReferenceType ::= ClassOrInterfaceType"); }  //$NON-NLS-1$
			    consumeReferenceType();   
				break;
	 
	    case 51 : if (DEBUG) { System.out.println("ClassOrInterface ::= Name"); }  //$NON-NLS-1$
			    consumeClassOrInterfaceName();   
				break;
	 
	    case 52 : if (DEBUG) { System.out.println("ClassOrInterface ::= GenericType DOT Name"); }  //$NON-NLS-1$
			    consumeClassOrInterface();   
				break;
	 
	    case 53 : if (DEBUG) { System.out.println("GenericType ::= ClassOrInterface TypeArguments"); }  //$NON-NLS-1$
			    consumeGenericType();   
				break;
	 
	    case 54 : if (DEBUG) { System.out.println("ArrayTypeWithTypeArgumentsName ::= GenericType DOT Name"); }  //$NON-NLS-1$
			    consumeArrayTypeWithTypeArgumentsName();   
				break;
	 
	    case 55 : if (DEBUG) { System.out.println("ArrayType ::= PrimitiveType Dims"); }  //$NON-NLS-1$
			    consumePrimitiveArrayType();   
				break;
	 
	    case 56 : if (DEBUG) { System.out.println("ArrayType ::= Name Dims"); }  //$NON-NLS-1$
			    consumeNameArrayType();   
				break;
	 
	    case 57 : if (DEBUG) { System.out.println("ArrayType ::= ArrayTypeWithTypeArgumentsName Dims"); }  //$NON-NLS-1$
			    consumeGenericTypeNameArrayType();   
				break;
	 
	    case 58 : if (DEBUG) { System.out.println("ArrayType ::= GenericType Dims"); }  //$NON-NLS-1$
			    consumeGenericTypeArrayType();   
				break;
	 
	    case 74 : if (DEBUG) { System.out.println("AjQualifiedName ::= AjName DOT SimpleName"); }  //$NON-NLS-1$
			    consumeQualifiedName();  
				break;
	 
	    case 78 : if (DEBUG) { System.out.println("QualifiedName ::= Name DOT JavaIdentifier"); }  //$NON-NLS-1$
			    consumeQualifiedName();  
				break;
	 
	    case 79 : if (DEBUG) { System.out.println("CompilationUnit ::= EnterCompilationUnit..."); }  //$NON-NLS-1$
			    consumeCompilationUnit();  
				break;
	 
	    case 80 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration"); }  //$NON-NLS-1$
			    consumeInternalCompilationUnit();  
				break;
	 
	    case 81 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration..."); }  //$NON-NLS-1$
			    consumeInternalCompilationUnit();  
				break;
	 
	    case 82 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration..."); }  //$NON-NLS-1$
			    consumeInternalCompilationUnitWithTypes();  
				break;
	 
	    case 83 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= PackageDeclaration..."); }  //$NON-NLS-1$
			    consumeInternalCompilationUnitWithTypes();  
				break;
	 
	    case 84 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= ImportDeclarations..."); }  //$NON-NLS-1$
			    consumeInternalCompilationUnit();  
				break;
	 
	    case 85 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= TypeDeclarations"); }  //$NON-NLS-1$
			    consumeInternalCompilationUnitWithTypes();  
				break;
	 
	    case 86 : if (DEBUG) { System.out.println("InternalCompilationUnit ::= ImportDeclarations..."); }  //$NON-NLS-1$
			    consumeInternalCompilationUnitWithTypes();  
				break;
	 
	    case 87 : if (DEBUG) { System.out.println("InternalCompilationUnit ::="); }  //$NON-NLS-1$
			    consumeEmptyInternalCompilationUnit();  
				break;
	 
	    case 88 : if (DEBUG) { System.out.println("ReduceImports ::="); }  //$NON-NLS-1$
			    consumeReduceImports();  
				break;
	 
	    case 89 : if (DEBUG) { System.out.println("EnterCompilationUnit ::="); }  //$NON-NLS-1$
			    consumeEnterCompilationUnit();  
				break;
	 
	    case 105 : if (DEBUG) { System.out.println("CatchHeader ::= catch LPAREN FormalParameter RPAREN..."); }  //$NON-NLS-1$
			    consumeCatchHeader();  
				break;
	 
	    case 107 : if (DEBUG) { System.out.println("ImportDeclarations ::= ImportDeclarations..."); }  //$NON-NLS-1$
			    consumeImportDeclarations();  
				break;
	 
	    case 109 : if (DEBUG) { System.out.println("TypeDeclarations ::= TypeDeclarations TypeDeclaration"); }  //$NON-NLS-1$
			    consumeTypeDeclarations();  
				break;
	 
	    case 110 : if (DEBUG) { System.out.println("PackageDeclaration ::= PackageDeclarationName SEMICOLON"); }  //$NON-NLS-1$
			     consumePackageDeclaration();  
				break;
	 
	    case 111 : if (DEBUG) { System.out.println("PackageDeclarationName ::= Modifiers package..."); }  //$NON-NLS-1$
			     consumePackageDeclarationNameWithModifiers();  
				break;
	 
	    case 112 : if (DEBUG) { System.out.println("PackageDeclarationName ::= PackageComment package Name"); }  //$NON-NLS-1$
			     consumePackageDeclarationName();  
				break;
	 
	    case 113 : if (DEBUG) { System.out.println("PackageComment ::="); }  //$NON-NLS-1$
			     consumePackageComment();  
				break;
	 
	    case 118 : if (DEBUG) { System.out.println("SingleTypeImportDeclaration ::=..."); }  //$NON-NLS-1$
			    consumeImportDeclaration();  
				break;
	 
	    case 119 : if (DEBUG) { System.out.println("SingleTypeImportDeclarationName ::= import Name"); }  //$NON-NLS-1$
			    consumeSingleTypeImportDeclarationName();  
				break;
	 
	    case 120 : if (DEBUG) { System.out.println("TypeImportOnDemandDeclaration ::=..."); }  //$NON-NLS-1$
			    consumeImportDeclaration();  
				break;
	 
	    case 121 : if (DEBUG) { System.out.println("TypeImportOnDemandDeclarationName ::= import Name DOT..."); }  //$NON-NLS-1$
			    consumeTypeImportOnDemandDeclarationName();  
				break;
	 
	     case 124 : if (DEBUG) { System.out.println("TypeDeclaration ::= SEMICOLON"); }  //$NON-NLS-1$
			    consumeEmptyTypeDeclaration();  
				break;
	 
	    case 128 : if (DEBUG) { System.out.println("Modifiers ::= Modifiers Modifier"); }  //$NON-NLS-1$
			    consumeModifiers2();  
				break;
	 
	    case 141 : if (DEBUG) { System.out.println("Modifier ::= Annotation"); }  //$NON-NLS-1$
			    consumeAnnotationAsModifier();  
				break;
	 
	    case 154 : if (DEBUG) { System.out.println("AspectDeclaration ::= AspectHeader AspectBody"); }  //$NON-NLS-1$
			    consumeAspectDeclaration();  
				break;
	 
	    case 155 : if (DEBUG) { System.out.println("AspectHeader ::= AspectHeaderName ClassHeaderExtendsopt"); }  //$NON-NLS-1$
			    consumeAspectHeader();  
				break;
	 
	    case 158 : if (DEBUG) { System.out.println("AspectHeaderName ::= AspectHeaderName1 TypeParameters"); }  //$NON-NLS-1$
			    consumeAspectHeaderNameWithTypeParameters(false);  
				break;
	 
	    case 159 : if (DEBUG) { System.out.println("AspectHeaderName1 ::= Modifiersopt aspect Identifier"); }  //$NON-NLS-1$
			    consumeAspectHeaderName(false);  
				break;
	 
	    case 160 : if (DEBUG) { System.out.println("AspectHeaderName2 ::= Modifiersopt privileged..."); }  //$NON-NLS-1$
			    consumeAspectHeaderName(true);  
				break;
	 
	    case 162 : if (DEBUG) { System.out.println("AspectHeaderRest ::= AspectHeaderRestStart PseudoTokens"); }  //$NON-NLS-1$
			    consumeAspectHeaderRest();  
				break;
	 
	    case 163 : if (DEBUG) { System.out.println("AspectHeaderRestStart ::= Identifier"); }  //$NON-NLS-1$
			    consumePseudoTokenIdentifier();  
				break;
	 
	    case 166 : if (DEBUG) { System.out.println("AspectBodyDeclarations ::= AspectBodyDeclarations..."); }  //$NON-NLS-1$
			    consumeClassBodyDeclarations();  
				break;
	 
	    case 167 : if (DEBUG) { System.out.println("AspectBodyDeclarationsopt ::="); }  //$NON-NLS-1$
			    consumeEmptyClassBodyDeclarationsopt();  
				break;
	 
	    case 168 : if (DEBUG) { System.out.println("AspectBodyDeclarationsopt ::= NestedType..."); }  //$NON-NLS-1$
			    consumeClassBodyDeclarationsopt();  
				break;
	 
	    case 169 : if (DEBUG) { System.out.println("AspectBodyDeclaration ::=..."); }  //$NON-NLS-1$
			    consumeClassBodyDeclarationInAspect();  
				break;
	 
	    case 173 : if (DEBUG) { System.out.println("ClassBodyDeclarationNoAroundMethod ::= Diet NestedMethod"); }  //$NON-NLS-1$
			    consumeClassBodyDeclaration();  
				break;
	 
	    case 182 : if (DEBUG) { System.out.println("ClassMemberDeclarationNoAroundMethod ::= SEMICOLON"); }  //$NON-NLS-1$
			    consumeEmptyClassMemberDeclaration();  
				break;

	    case 184 : if (DEBUG) { System.out.println("MethodDeclarationNoAround ::= MethodHeaderNoAround..."); }  //$NON-NLS-1$
			    // set to true to consume a method with a body
	  consumeMethodDeclaration(true);   
				break;
	 
	    case 185 : if (DEBUG) { System.out.println("AbstractMethodDeclarationNoAround ::=..."); }  //$NON-NLS-1$
			    // set to false to consume a method without body
	  consumeMethodDeclaration(false);  
				break;
	 
	    case 186 : if (DEBUG) { System.out.println("MethodHeaderNoAround ::= MethodHeaderNameNoAround..."); }  //$NON-NLS-1$
			    consumeMethodHeader();  
				break;
	 
	    case 187 : if (DEBUG) { System.out.println("MethodHeaderNameNoAround ::= Modifiersopt TypeParameters"); }  //$NON-NLS-1$
			    consumeMethodHeaderNameWithTypeParameters(false);  
				break;
	 
	    case 188 : if (DEBUG) { System.out.println("MethodHeaderNameNoAround ::= Modifiersopt Type..."); }  //$NON-NLS-1$
			    consumeMethodHeaderName(false);  
				break;
	 
	    case 189 : if (DEBUG) { System.out.println("PointcutDeclaration ::= PointcutHeader..."); }  //$NON-NLS-1$
			    consumeEmptyPointcutDeclaration();  
				break;
	 
	    case 190 : if (DEBUG) { System.out.println("PointcutDeclaration ::= PointcutHeader..."); }  //$NON-NLS-1$
			    consumePointcutDeclaration();  
				break;
	 
	    case 191 : if (DEBUG) { System.out.println("PointcutHeader ::= Modifiersopt pointcut JavaIdentifier"); }  //$NON-NLS-1$
			    consumePointcutHeader();  
				break;
	 
	    case 194 : if (DEBUG) { System.out.println("AroundDeclaration ::= AroundHeader MethodBody"); }  //$NON-NLS-1$
			    consumeAroundDeclaration();  
				break;
	 
	    case 195 : if (DEBUG) { System.out.println("AroundHeader ::= AroundHeaderName FormalParameterListopt"); }  //$NON-NLS-1$
			    consumeAroundHeader();  
				break;
	 
	    case 196 : if (DEBUG) { System.out.println("AroundHeaderName ::= Modifiersopt Type around LPAREN"); }  //$NON-NLS-1$
			    consumeAroundHeaderName();  
				break;
	 
	    case 197 : if (DEBUG) { System.out.println("BasicAdviceDeclaration ::= BasicAdviceHeader MethodBody"); }  //$NON-NLS-1$
			    consumeBasicAdviceDeclaration();  
				break;
	 
	    case 198 : if (DEBUG) { System.out.println("BasicAdviceHeader ::= BasicAdviceHeaderName..."); }  //$NON-NLS-1$
			    consumeBasicAdviceHeader();  
				break;
	 
	    case 199 : if (DEBUG) { System.out.println("BasicAdviceHeaderName ::= Modifiersopt before LPAREN"); }  //$NON-NLS-1$
			    consumeBasicAdviceHeaderName(false);  
				break;
	 
	    case 200 : if (DEBUG) { System.out.println("BasicAdviceHeaderName ::= Modifiersopt after LPAREN"); }  //$NON-NLS-1$
			    consumeBasicAdviceHeaderName(true);  
				break;
	 
	    case 201 : if (DEBUG) { System.out.println("ExtraParamopt ::= Identifier LPAREN FormalParameter..."); }  //$NON-NLS-1$
			    consumeExtraParameterWithFormal();  
				break;
	 
	    case 202 : if (DEBUG) { System.out.println("ExtraParamopt ::= Identifier LPAREN RPAREN"); }  //$NON-NLS-1$
			    consumeExtraParameterNoFormal();  
				break;
	 
	    case 203 : if (DEBUG) { System.out.println("ExtraParamopt ::= Identifier"); }  //$NON-NLS-1$
			    consumeExtraParameterNoFormal();  
				break;
	 
	    case 206 : if (DEBUG) { System.out.println("OnType ::= OnType DOT JavaIdentifier"); }  //$NON-NLS-1$
			    consumeQualifiedName();  
				break;
	 
	    case 211 : if (DEBUG) { System.out.println("InterTypeMethodDeclaration ::= InterTypeMethodHeader..."); }  //$NON-NLS-1$
			    // set to true to consume a method with a body
	  consumeInterTypeMethodDeclaration(true);   
				break;
	 
	    case 212 : if (DEBUG) { System.out.println("InterTypeMethodHeader ::= InterTypeMethodHeaderName..."); }  //$NON-NLS-1$
			    consumeInterTypeMethodHeader();  
				break;
	 
	    case 213 : if (DEBUG) { System.out.println("InterTypeMethodHeaderName ::= Modifiersopt Type OnType"); }  //$NON-NLS-1$
			    consumeInterTypeMethodHeaderName();  
				break;
	 
	    case 214 : if (DEBUG) { System.out.println("InterTypeMethodHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
			    consumeInterTypeMethodHeaderNameWithTypeParameters();  
				break;
	 
	    case 215 : if (DEBUG) { System.out.println("AbstractInterTypeMethodDeclaration ::=..."); }  //$NON-NLS-1$
			    // set to false to consume a method without body
	  consumeInterTypeMethodDeclaration(false);  
				break;
	 
	    case 216 : if (DEBUG) { System.out.println("InterTypeConstructorDeclaration ::=..."); }  //$NON-NLS-1$
			    // set to true to consume a method with a body
	  consumeInterTypeConstructorDeclaration();   
				break;
	 
	    case 217 : if (DEBUG) { System.out.println("InterTypeConstructorHeader ::=..."); }  //$NON-NLS-1$
			    consumeInterTypeConstructorHeader();  
				break;
	 
	    case 218 : if (DEBUG) { System.out.println("InterTypeConstructorHeaderName ::= Modifiersopt Name DOT"); }  //$NON-NLS-1$
			    consumeInterTypeConstructorHeaderName();  
				break;
	 
	    case 219 : if (DEBUG) { System.out.println("InterTypeConstructorHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
			    consumeInterTypeConstructorHeaderNameWithTypeParameters();  
				break;
	 
	    case 220 : if (DEBUG) { System.out.println("InterTypeFieldDeclaration ::= Modifiersopt Type OnType"); }  //$NON-NLS-1$
			    consumeInterTypeFieldDeclaration();  
				break;
	 
	    case 222 : if (DEBUG) { System.out.println("EnterITDVariable ::="); }  //$NON-NLS-1$
			    consumeEnterITDVariable();  
				break;
	 
	    case 223 : if (DEBUG) { System.out.println("InterTypeFieldBody ::="); }  //$NON-NLS-1$
			    consumeExitITDVariableWithoutInitializer();  
				break;
	 
	    case 224 : if (DEBUG) { System.out.println("InterTypeFieldBody ::= EQUAL ForceNoDiet..."); }  //$NON-NLS-1$
			    consumeExitITDVariableWithInitializer();  
				break;
	 
	    case 226 : if (DEBUG) { System.out.println("DeclareDeclaration ::= DeclareHeader PseudoTokens..."); }  //$NON-NLS-1$
			    consumeDeclareDeclaration();  
				break;
	 
	    case 227 : if (DEBUG) { System.out.println("DeclareHeader ::= declare Identifier COLON"); }  //$NON-NLS-1$
			    consumeDeclareHeader();  
				break;
	 
	    case 228 : if (DEBUG) { System.out.println("DeclareDeclaration ::= DeclareAnnotationHeader..."); }  //$NON-NLS-1$
			    consumeDeclareAnnotation();  
				break;
	 
	    case 229 : if (DEBUG) { System.out.println("DeclareAnnotationHeader ::= declare AT Identifier COLON"); }  //$NON-NLS-1$
			    consumeDeclareAnnotationHeader();  
				break;
	 
	    case 232 : if (DEBUG) { System.out.println("PseudoTokens ::= PseudoTokens ColonPseudoToken"); }  //$NON-NLS-1$
			    consumePseudoTokens();  
				break;
	 
	    case 233 : if (DEBUG) { System.out.println("PseudoTokens ::= PseudoTokens PseudoToken"); }  //$NON-NLS-1$
			    consumePseudoTokens();  
				break;
	 
	    case 235 : if (DEBUG) { System.out.println("PseudoTokensNoColon ::= PseudoTokensNoColon PseudoToken"); }  //$NON-NLS-1$
			    consumePseudoTokens();  
				break;
	 
	    case 236 : if (DEBUG) { System.out.println("ColonPseudoToken ::= COLON"); }  //$NON-NLS-1$
			    consumePseudoToken(":");  
				break;
	 
	    case 237 : if (DEBUG) { System.out.println("PseudoToken ::= JavaIdentifier"); }  //$NON-NLS-1$
			    consumePseudoTokenIdentifier();  
				break;
	 
	    case 238 : if (DEBUG) { System.out.println("PseudoToken ::= LPAREN"); }  //$NON-NLS-1$
			    consumePseudoToken("(");  
				break;
	 
	    case 239 : if (DEBUG) { System.out.println("PseudoToken ::= RPAREN"); }  //$NON-NLS-1$
			    consumePseudoToken(")");  
				break;
	 
	    case 240 : if (DEBUG) { System.out.println("PseudoToken ::= DOT"); }  //$NON-NLS-1$
			    consumePseudoToken(".");  
				break;
	 
	    case 241 : if (DEBUG) { System.out.println("PseudoToken ::= MULTIPLY"); }  //$NON-NLS-1$
			    consumePseudoToken("*");  
				break;
	 
	    case 242 : if (DEBUG) { System.out.println("PseudoToken ::= PLUS"); }  //$NON-NLS-1$
			    consumePseudoToken("+");  
				break;
	 
	    case 243 : if (DEBUG) { System.out.println("PseudoToken ::= AND_AND"); }  //$NON-NLS-1$
			    consumePseudoToken("&&");  
				break;
	 
	    case 244 : if (DEBUG) { System.out.println("PseudoToken ::= OR_OR"); }  //$NON-NLS-1$
			    consumePseudoToken("||");  
				break;
	 
	    case 245 : if (DEBUG) { System.out.println("PseudoToken ::= NOT"); }  //$NON-NLS-1$
			    consumePseudoToken("!");  
				break;
	 
	    case 246 : if (DEBUG) { System.out.println("PseudoToken ::= COMMA"); }  //$NON-NLS-1$
			    consumePseudoToken(",");  
				break;
	 
	    case 247 : if (DEBUG) { System.out.println("PseudoToken ::= LBRACKET"); }  //$NON-NLS-1$
			    consumePseudoToken("[");  
				break;
	 
	    case 248 : if (DEBUG) { System.out.println("PseudoToken ::= RBRACKET"); }  //$NON-NLS-1$
			    consumePseudoToken("]");  
				break;
	 
	    case 249 : if (DEBUG) { System.out.println("PseudoToken ::= AT"); }  //$NON-NLS-1$
			    consumePseudoToken("@");  
				break;
	 
	    case 250 : if (DEBUG) { System.out.println("PseudoToken ::= ELLIPSIS"); }  //$NON-NLS-1$
			    consumePseudoToken("...");  
				break;
	 
	    case 251 : if (DEBUG) { System.out.println("PseudoToken ::= QUESTION"); }  //$NON-NLS-1$
			    consumePseudoToken("?");  
				break;
	 
	    case 252 : if (DEBUG) { System.out.println("PseudoToken ::= LESS"); }  //$NON-NLS-1$
			    consumePseudoToken("<");  
				break;
	 
	    case 253 : if (DEBUG) { System.out.println("PseudoToken ::= GREATER"); }  //$NON-NLS-1$
			    consumePseudoToken(">");  
				break;
	 
	    case 254 : if (DEBUG) { System.out.println("PseudoToken ::= RIGHT_SHIFT"); }  //$NON-NLS-1$
			    consumePseudoToken(">"); consumePseudoToken(">");  
				break;
	 
	    case 255 : if (DEBUG) { System.out.println("PseudoToken ::= UNSIGNED_RIGHT_SHIFT"); }  //$NON-NLS-1$
			    consumePseudoToken(">"); consumePseudoToken(">"); consumePseudoToken(">");  
				break;
	 
	    case 256 : if (DEBUG) { System.out.println("PseudoToken ::= AND"); }  //$NON-NLS-1$
			    consumePseudoToken("&");  
				break;
	 
	    case 257 : if (DEBUG) { System.out.println("PseudoToken ::= PrimitiveType"); }  //$NON-NLS-1$
			    consumePseudoTokenPrimitiveType();  
				break;
	 
	    case 258 : if (DEBUG) { System.out.println("PseudoToken ::= SimpleModifier"); }  //$NON-NLS-1$
			    consumePseudoTokenModifier();  
				break;
	 
	    case 259 : if (DEBUG) { System.out.println("PseudoToken ::= Literal"); }  //$NON-NLS-1$
			    consumePseudoTokenLiteral();  
				break;
	 
	    case 260 : if (DEBUG) { System.out.println("PseudoToken ::= this"); }  //$NON-NLS-1$
			    consumePseudoToken("this", 1, true);  
				break;
	 
	    case 261 : if (DEBUG) { System.out.println("PseudoToken ::= super"); }  //$NON-NLS-1$
			    consumePseudoToken("super", 1, true);  
				break;
	 
	    case 262 : if (DEBUG) { System.out.println("PseudoToken ::= if LPAREN Expression RPAREN"); }  //$NON-NLS-1$
			    consumePseudoTokenIf();  
				break;
	 
	    case 263 : if (DEBUG) { System.out.println("PseudoToken ::= assert"); }  //$NON-NLS-1$
			    consumePseudoToken("assert", 1, true);  
				break;
	 
	    case 264 : if (DEBUG) { System.out.println("PseudoToken ::= import"); }  //$NON-NLS-1$
			    consumePseudoToken("import", 1, true);  
				break;
	 
	    case 265 : if (DEBUG) { System.out.println("PseudoToken ::= package"); }  //$NON-NLS-1$
			    consumePseudoToken("package", 1, true);  
				break;
	 
	    case 266 : if (DEBUG) { System.out.println("PseudoToken ::= throw"); }  //$NON-NLS-1$
			    consumePseudoToken("throw", 1, true);  
				break;
	 
	    case 267 : if (DEBUG) { System.out.println("PseudoToken ::= new"); }  //$NON-NLS-1$
			    consumePseudoToken("new", 1, true);  
				break;
	 
	    case 268 : if (DEBUG) { System.out.println("PseudoToken ::= do"); }  //$NON-NLS-1$
			    consumePseudoToken("do", 1, true);  
				break;
	 
	    case 269 : if (DEBUG) { System.out.println("PseudoToken ::= for"); }  //$NON-NLS-1$
			    consumePseudoToken("for", 1, true);  
				break;
	 
	    case 270 : if (DEBUG) { System.out.println("PseudoToken ::= switch"); }  //$NON-NLS-1$
			    consumePseudoToken("switch", 1, true);  
				break;
	 
	    case 271 : if (DEBUG) { System.out.println("PseudoToken ::= try"); }  //$NON-NLS-1$
			    consumePseudoToken("try", 1, true);  
				break;
	 
	    case 272 : if (DEBUG) { System.out.println("PseudoToken ::= while"); }  //$NON-NLS-1$
			    consumePseudoToken("while", 1, true);  
				break;
	 
	    case 273 : if (DEBUG) { System.out.println("PseudoToken ::= break"); }  //$NON-NLS-1$
			    consumePseudoToken("break", 1, true);  
				break;
	 
	    case 274 : if (DEBUG) { System.out.println("PseudoToken ::= continue"); }  //$NON-NLS-1$
			    consumePseudoToken("continue", 1, true);  
				break;
	 
	    case 275 : if (DEBUG) { System.out.println("PseudoToken ::= return"); }  //$NON-NLS-1$
			    consumePseudoToken("return", 1, true);  
				break;
	 
	    case 276 : if (DEBUG) { System.out.println("PseudoToken ::= case"); }  //$NON-NLS-1$
			    consumePseudoToken("case", 1, true);  
				break;
	 
	    case 277 : if (DEBUG) { System.out.println("PseudoToken ::= catch"); }  //$NON-NLS-1$
			    consumePseudoToken("catch", 0, true);  
				break;
	 
	    case 278 : if (DEBUG) { System.out.println("PseudoToken ::= instanceof"); }  //$NON-NLS-1$
			    consumePseudoToken("instanceof", 0, true);  
				break;
	 
	    case 279 : if (DEBUG) { System.out.println("PseudoToken ::= else"); }  //$NON-NLS-1$
			    consumePseudoToken("else", 0, true);  
				break;
	 
	    case 280 : if (DEBUG) { System.out.println("PseudoToken ::= extends"); }  //$NON-NLS-1$
			    consumePseudoToken("extends", 0, true);  
				break;
	 
	    case 281 : if (DEBUG) { System.out.println("PseudoToken ::= finally"); }  //$NON-NLS-1$
			    consumePseudoToken("finally", 0, true);  
				break;
	 
	    case 282 : if (DEBUG) { System.out.println("PseudoToken ::= implements"); }  //$NON-NLS-1$
			    consumePseudoToken("implements", 0, true);  
				break;
	 
	    case 283 : if (DEBUG) { System.out.println("PseudoToken ::= throws"); }  //$NON-NLS-1$
			    consumePseudoToken("throws", 0, true);  
				break;
	 
	    case 284 : if (DEBUG) { System.out.println("ClassDeclaration ::= ClassHeader ClassBody"); }  //$NON-NLS-1$
			    consumeClassDeclaration();  
				break;
	 
	    case 285 : if (DEBUG) { System.out.println("ClassHeader ::= ClassHeaderName ClassHeaderExtendsopt..."); }  //$NON-NLS-1$
			    consumeClassHeader();  
				break;
	 
	    case 286 : if (DEBUG) { System.out.println("ClassHeaderName ::= ClassHeaderName1 TypeParameters"); }  //$NON-NLS-1$
			    consumeTypeHeaderNameWithTypeParameters();  
				break;
	 
	    case 288 : if (DEBUG) { System.out.println("ClassHeaderName1 ::= Modifiersopt class JavaIdentifier"); }  //$NON-NLS-1$
			    consumeClassHeaderName1();  
				break;
	 
	    case 289 : if (DEBUG) { System.out.println("ClassHeaderExtends ::= extends ClassType"); }  //$NON-NLS-1$
			    consumeClassHeaderExtends();  
				break;
	 
	    case 290 : if (DEBUG) { System.out.println("ClassHeaderImplements ::= implements InterfaceTypeList"); }  //$NON-NLS-1$
			    consumeClassHeaderImplements();  
				break;
	 
	    case 292 : if (DEBUG) { System.out.println("InterfaceTypeList ::= InterfaceTypeList COMMA..."); }  //$NON-NLS-1$
			    consumeInterfaceTypeList();  
				break;
	 
	    case 293 : if (DEBUG) { System.out.println("InterfaceType ::= ClassOrInterfaceType"); }  //$NON-NLS-1$
			    consumeInterfaceType();  
				break;
	 
	    case 296 : if (DEBUG) { System.out.println("ClassBodyDeclarations ::= ClassBodyDeclarations..."); }  //$NON-NLS-1$
			    consumeClassBodyDeclarations();  
				break;
	 
	    case 300 : if (DEBUG) { System.out.println("ClassBodyDeclaration ::= Diet NestedMethod Block"); }  //$NON-NLS-1$
			    consumeClassBodyDeclaration();  
				break;
	 
	    case 301 : if (DEBUG) { System.out.println("Diet ::="); }  //$NON-NLS-1$
			    consumeDiet();  
				break;

	    case 302 : if (DEBUG) { System.out.println("Initializer ::= Diet NestedMethod Block"); }  //$NON-NLS-1$
			    consumeClassBodyDeclaration();  
				break;
	 
	    case 309 : if (DEBUG) { System.out.println("ClassMemberDeclaration ::= SEMICOLON"); }  //$NON-NLS-1$
			    consumeEmptyClassMemberDeclaration();  
				break;

	    case 312 : if (DEBUG) { System.out.println("FieldDeclaration ::= Modifiersopt Type..."); }  //$NON-NLS-1$
			    consumeFieldDeclaration();  
				break;
	 
	    case 314 : if (DEBUG) { System.out.println("VariableDeclarators ::= VariableDeclarators COMMA..."); }  //$NON-NLS-1$
			    consumeVariableDeclarators();  
				break;
	 
	    case 317 : if (DEBUG) { System.out.println("EnterVariable ::="); }  //$NON-NLS-1$
			    consumeEnterVariable();  
				break;
	 
	    case 318 : if (DEBUG) { System.out.println("ExitVariableWithInitialization ::="); }  //$NON-NLS-1$
			    consumeExitVariableWithInitialization();  
				break;
	 
	    case 319 : if (DEBUG) { System.out.println("ExitVariableWithoutInitialization ::="); }  //$NON-NLS-1$
			    consumeExitVariableWithoutInitialization();  
				break;
	 
	    case 320 : if (DEBUG) { System.out.println("ForceNoDiet ::="); }  //$NON-NLS-1$
			    consumeForceNoDiet();  
				break;
	 
	    case 321 : if (DEBUG) { System.out.println("RestoreDiet ::="); }  //$NON-NLS-1$
			    consumeRestoreDiet();  
				break;
	 
	    case 326 : if (DEBUG) { System.out.println("MethodDeclaration ::= MethodHeader MethodBody"); }  //$NON-NLS-1$
			    // set to true to consume a method with a body
	  consumeMethodDeclaration(true);   
				break;
	 
	    case 327 : if (DEBUG) { System.out.println("AbstractMethodDeclaration ::= MethodHeader SEMICOLON"); }  //$NON-NLS-1$
			    // set to false to consume a method without body
	  consumeMethodDeclaration(false);  
				break;
	 
	    case 328 : if (DEBUG) { System.out.println("MethodHeader ::= MethodHeaderName FormalParameterListopt"); }  //$NON-NLS-1$
			    consumeMethodHeader();  
				break;
	 
	    case 329 : if (DEBUG) { System.out.println("MethodHeaderName ::= Modifiersopt TypeParameters Type..."); }  //$NON-NLS-1$
			    consumeMethodHeaderNameWithTypeParameters(false);  
				break;
	 
	    case 330 : if (DEBUG) { System.out.println("MethodHeaderName ::= Modifiersopt Type JavaIdentifier..."); }  //$NON-NLS-1$
			    consumeMethodHeaderName(false);  
				break;
	 
	    case 331 : if (DEBUG) { System.out.println("MethodHeaderRightParen ::= RPAREN"); }  //$NON-NLS-1$
			    consumeMethodHeaderRightParen();  
				break;
	 
	    case 332 : if (DEBUG) { System.out.println("MethodHeaderExtendedDims ::= Dimsopt"); }  //$NON-NLS-1$
			    consumeMethodHeaderExtendedDims();  
				break;
	 
	    case 333 : if (DEBUG) { System.out.println("MethodHeaderThrowsClause ::= throws ClassTypeList"); }  //$NON-NLS-1$
			    consumeMethodHeaderThrowsClause();  
				break;
	 
	    case 334 : if (DEBUG) { System.out.println("ConstructorHeader ::= ConstructorHeaderName..."); }  //$NON-NLS-1$
			    consumeConstructorHeader();  
				break;
	 
	    case 335 : if (DEBUG) { System.out.println("ConstructorHeaderName ::= Modifiersopt TypeParameters..."); }  //$NON-NLS-1$
			    consumeConstructorHeaderNameWithTypeParameters();  
				break;
	 
	    case 336 : if (DEBUG) { System.out.println("ConstructorHeaderName ::= Modifiersopt Identifier LPAREN"); }  //$NON-NLS-1$
			    consumeConstructorHeaderName();  
				break;
	 
	    case 337 : if (DEBUG) { System.out.println("ConstructorHeaderName ::= Modifiersopt aspect LPAREN"); }  //$NON-NLS-1$
			    consumeConstructorHeaderName();  
				break;
	 
	    case 339 : if (DEBUG) { System.out.println("FormalParameterList ::= FormalParameterList COMMA..."); }  //$NON-NLS-1$
			    consumeFormalParameterList();  
				break;
	 
	    case 340 : if (DEBUG) { System.out.println("FormalParameter ::= Modifiersopt Type..."); }  //$NON-NLS-1$
			    consumeFormalParameter(false);  
				break;
	 
	    case 341 : if (DEBUG) { System.out.println("FormalParameter ::= Modifiersopt Type ELLIPSIS..."); }  //$NON-NLS-1$
			    consumeFormalParameter(true);  
				break;
	 
	    case 343 : if (DEBUG) { System.out.println("ClassTypeList ::= ClassTypeList COMMA ClassTypeElt"); }  //$NON-NLS-1$
			    consumeClassTypeList();  
				break;
	 
	    case 344 : if (DEBUG) { System.out.println("ClassTypeElt ::= ClassType"); }  //$NON-NLS-1$
			    consumeClassTypeElt();  
				break;
	 
	    case 345 : if (DEBUG) { System.out.println("MethodBody ::= NestedMethod LBRACE BlockStatementsopt..."); }  //$NON-NLS-1$
			    consumeMethodBody();  
				break;
	 
	    case 346 : if (DEBUG) { System.out.println("NestedMethod ::="); }  //$NON-NLS-1$
			    consumeNestedMethod();  
				break;
	 
	    case 347 : if (DEBUG) { System.out.println("StaticInitializer ::= StaticOnly Block"); }  //$NON-NLS-1$
			    consumeStaticInitializer();  
				break;

	    case 348 : if (DEBUG) { System.out.println("StaticOnly ::= static"); }  //$NON-NLS-1$
			    consumeStaticOnly();  
				break;
	 
	    case 349 : if (DEBUG) { System.out.println("ConstructorDeclaration ::= ConstructorHeader MethodBody"); }  //$NON-NLS-1$
			    consumeConstructorDeclaration() ;  
				break;
	 
	    case 350 : if (DEBUG) { System.out.println("ConstructorDeclaration ::= ConstructorHeader SEMICOLON"); }  //$NON-NLS-1$
			    consumeInvalidConstructorDeclaration() ;  
				break;
	 
	    case 351 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= this LPAREN..."); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocation(0, THIS_CALL);  
				break;
	 
	    case 352 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= OnlyTypeArguments this"); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocationWithTypeArguments(0,THIS_CALL);  
				break;
	 
	    case 353 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= super LPAREN..."); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocation(0,SUPER_CALL);  
				break;
	 
	    case 354 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= OnlyTypeArguments..."); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocationWithTypeArguments(0,SUPER_CALL);  
				break;
	 
	    case 355 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT super..."); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocation(1, SUPER_CALL);  
				break;
	 
	    case 356 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT..."); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocationWithTypeArguments(1, SUPER_CALL);  
				break;
	 
	    case 357 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT super LPAREN"); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocation(2, SUPER_CALL);  
				break;
	 
	    case 358 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT..."); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocationWithTypeArguments(2, SUPER_CALL);  
				break;
	 
	    case 359 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT this..."); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocation(1, THIS_CALL);  
				break;
	 
	    case 360 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Primary DOT..."); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocationWithTypeArguments(1, THIS_CALL);  
				break;
	 
	    case 361 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT this LPAREN"); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocation(2, THIS_CALL);  
				break;
	 
	    case 362 : if (DEBUG) { System.out.println("ExplicitConstructorInvocation ::= Name DOT..."); }  //$NON-NLS-1$
			    consumeExplicitConstructorInvocationWithTypeArguments(2, THIS_CALL);  
				break;
	 
	    case 363 : if (DEBUG) { System.out.println("InterfaceDeclaration ::= InterfaceHeader InterfaceBody"); }  //$NON-NLS-1$
			    consumeInterfaceDeclaration();  
				break;
	 
	    case 364 : if (DEBUG) { System.out.println("InterfaceHeader ::= InterfaceHeaderName..."); }  //$NON-NLS-1$
			    consumeInterfaceHeader();  
				break;
	 
	    case 365 : if (DEBUG) { System.out.println("InterfaceHeaderName ::= InterfaceHeaderName1..."); }  //$NON-NLS-1$
			    consumeTypeHeaderNameWithTypeParameters();  
				break;
	 
	    case 367 : if (DEBUG) { System.out.println("InterfaceHeaderName1 ::= Modifiersopt interface..."); }  //$NON-NLS-1$
			    consumeInterfaceHeaderName1();  
				break;
	 
	    case 368 : if (DEBUG) { System.out.println("InterfaceHeaderExtends ::= extends InterfaceTypeList"); }  //$NON-NLS-1$
			    consumeInterfaceHeaderExtends();  
				break;
	 
	    case 371 : if (DEBUG) { System.out.println("InterfaceMemberDeclarations ::=..."); }  //$NON-NLS-1$
			    consumeInterfaceMemberDeclarations();  
				break;
	 
	    case 372 : if (DEBUG) { System.out.println("InterfaceMemberDeclaration ::= SEMICOLON"); }  //$NON-NLS-1$
			    consumeEmptyInterfaceMemberDeclaration();  
				break;
	 
	    case 374 : if (DEBUG) { System.out.println("InterfaceMemberDeclaration ::= MethodHeader MethodBody"); }  //$NON-NLS-1$
			    consumeInvalidMethodDeclaration();  
				break;
	 
	    case 375 : if (DEBUG) { System.out.println("InvalidConstructorDeclaration ::= ConstructorHeader..."); }  //$NON-NLS-1$
			    consumeInvalidConstructorDeclaration(true);   
				break;
	 
	    case 376 : if (DEBUG) { System.out.println("InvalidConstructorDeclaration ::= ConstructorHeader..."); }  //$NON-NLS-1$
			    consumeInvalidConstructorDeclaration(false);   
				break;
	 
	    case 384 : if (DEBUG) { System.out.println("PushLeftBrace ::="); }  //$NON-NLS-1$
			    consumePushLeftBrace();  
				break;
	 
	    case 385 : if (DEBUG) { System.out.println("ArrayInitializer ::= LBRACE PushLeftBrace ,opt RBRACE"); }  //$NON-NLS-1$
			    consumeEmptyArrayInitializer();  
				break;
	 
	    case 386 : if (DEBUG) { System.out.println("ArrayInitializer ::= LBRACE PushLeftBrace..."); }  //$NON-NLS-1$
			    consumeArrayInitializer();  
				break;
	 
	    case 387 : if (DEBUG) { System.out.println("ArrayInitializer ::= LBRACE PushLeftBrace..."); }  //$NON-NLS-1$
			    consumeArrayInitializer();  
				break;
	 
	    case 389 : if (DEBUG) { System.out.println("VariableInitializers ::= VariableInitializers COMMA..."); }  //$NON-NLS-1$
			    consumeVariableInitializers();  
				break;
	 
	    case 390 : if (DEBUG) { System.out.println("Block ::= OpenBlock LBRACE BlockStatementsopt RBRACE"); }  //$NON-NLS-1$
			    consumeBlock();  
				break;
	 
	    case 391 : if (DEBUG) { System.out.println("OpenBlock ::="); }  //$NON-NLS-1$
			    consumeOpenBlock() ;  
				break;
	 
	    case 393 : if (DEBUG) { System.out.println("BlockStatements ::= BlockStatements BlockStatement"); }  //$NON-NLS-1$
			    consumeBlockStatements() ;  
				break;
	 
	    case 397 : if (DEBUG) { System.out.println("BlockStatement ::= InterfaceDeclaration"); }  //$NON-NLS-1$
			    consumeInvalidInterfaceDeclaration();  
				break;
	 
	    case 398 : if (DEBUG) { System.out.println("BlockStatement ::= AnnotationTypeDeclaration"); }  //$NON-NLS-1$
			    consumeInvalidAnnotationTypeDeclaration();  
				break;
	 
	    case 399 : if (DEBUG) { System.out.println("BlockStatement ::= EnumDeclaration"); }  //$NON-NLS-1$
			    consumeInvalidEnumDeclaration();  
				break;
	 
	    case 400 : if (DEBUG) { System.out.println("LocalVariableDeclarationStatement ::=..."); }  //$NON-NLS-1$
			    consumeLocalVariableDeclarationStatement();  
				break;
	 
	    case 401 : if (DEBUG) { System.out.println("LocalVariableDeclaration ::= Type PushModifiers..."); }  //$NON-NLS-1$
			    consumeLocalVariableDeclaration();  
				break;
	 
	    case 402 : if (DEBUG) { System.out.println("LocalVariableDeclaration ::= Modifiers Type..."); }  //$NON-NLS-1$
			    consumeLocalVariableDeclaration();  
				break;
	 
	    case 403 : if (DEBUG) { System.out.println("PushModifiers ::="); }  //$NON-NLS-1$
			    consumePushModifiers();  
				break;
	 
	    case 404 : if (DEBUG) { System.out.println("PushModifiersForHeader ::="); }  //$NON-NLS-1$
			    consumePushModifiersForHeader();  
				break;
	 
	    case 405 : if (DEBUG) { System.out.println("PushRealModifiers ::="); }  //$NON-NLS-1$
			    consumePushRealModifiers();  
				break;
	 
	    case 431 : if (DEBUG) { System.out.println("EmptyStatement ::= SEMICOLON"); }  //$NON-NLS-1$
			    consumeEmptyStatement();  
				break;
	 
	    case 432 : if (DEBUG) { System.out.println("LabeledStatement ::= JavaIdentifier COLON Statement"); }  //$NON-NLS-1$
			    consumeStatementLabel() ;  
				break;
	 
	    case 433 : if (DEBUG) { System.out.println("LabeledStatementNoShortIf ::= JavaIdentifier COLON..."); }  //$NON-NLS-1$
			    consumeStatementLabel() ;  
				break;
	 
	     case 434 : if (DEBUG) { System.out.println("ExpressionStatement ::= StatementExpression SEMICOLON"); }  //$NON-NLS-1$
			    consumeExpressionStatement();  
				break;
	 
	    case 443 : if (DEBUG) { System.out.println("IfThenStatement ::= if LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
			    consumeStatementIfNoElse();  
				break;
	 
	    case 444 : if (DEBUG) { System.out.println("IfThenElseStatement ::= if LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
			    consumeStatementIfWithElse();  
				break;
	 
	    case 445 : if (DEBUG) { System.out.println("IfThenElseStatementNoShortIf ::= if LPAREN Expression..."); }  //$NON-NLS-1$
			    consumeStatementIfWithElse();  
				break;
	 
	    case 446 : if (DEBUG) { System.out.println("SwitchStatement ::= switch LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
			    consumeStatementSwitch() ;  
				break;
	 
	    case 447 : if (DEBUG) { System.out.println("SwitchBlock ::= LBRACE RBRACE"); }  //$NON-NLS-1$
			    consumeEmptySwitchBlock() ;  
				break;
	 
	    case 450 : if (DEBUG) { System.out.println("SwitchBlock ::= LBRACE SwitchBlockStatements..."); }  //$NON-NLS-1$
			    consumeSwitchBlock() ;  
				break;
	 
	    case 452 : if (DEBUG) { System.out.println("SwitchBlockStatements ::= SwitchBlockStatements..."); }  //$NON-NLS-1$
			    consumeSwitchBlockStatements() ;  
				break;
	 
	    case 453 : if (DEBUG) { System.out.println("SwitchBlockStatement ::= SwitchLabels BlockStatements"); }  //$NON-NLS-1$
			    consumeSwitchBlockStatement() ;  
				break;
	 
	    case 455 : if (DEBUG) { System.out.println("SwitchLabels ::= SwitchLabels SwitchLabel"); }  //$NON-NLS-1$
			    consumeSwitchLabels() ;  
				break;
	 
	     case 456 : if (DEBUG) { System.out.println("SwitchLabel ::= case ConstantExpression COLON"); }  //$NON-NLS-1$
			    consumeCaseLabel();  
				break;
	 
	     case 457 : if (DEBUG) { System.out.println("SwitchLabel ::= default COLON"); }  //$NON-NLS-1$
			    consumeDefaultLabel();  
				break;
	 
	    case 458 : if (DEBUG) { System.out.println("WhileStatement ::= while LPAREN Expression RPAREN..."); }  //$NON-NLS-1$
			    consumeStatementWhile() ;  
				break;
	 
	    case 459 : if (DEBUG) { System.out.println("WhileStatementNoShortIf ::= while LPAREN Expression..."); }  //$NON-NLS-1$
			    consumeStatementWhile() ;  
				break;
	 
	    case 460 : if (DEBUG) { System.out.println("DoStatement ::= do Statement while LPAREN Expression..."); }  //$NON-NLS-1$
			    consumeStatementDo() ;  
				break;
	 
	    case 461 : if (DEBUG) { System.out.println("ForStatement ::= for LPAREN ForInitopt SEMICOLON..."); }  //$NON-NLS-1$
			    consumeStatementFor() ;  
				break;
	 
	    case 462 : if (DEBUG) { System.out.println("ForStatementNoShortIf ::= for LPAREN ForInitopt..."); }  //$NON-NLS-1$
			    consumeStatementFor() ;  
				break;
	 
	    case 463 : if (DEBUG) { System.out.println("ForInit ::= StatementExpressionList"); }  //$NON-NLS-1$
			    consumeForInit() ;  
				break;
	 
	    case 467 : if (DEBUG) { System.out.println("StatementExpressionList ::= StatementExpressionList..."); }  //$NON-NLS-1$
			    consumeStatementExpressionList() ;  
				break;
	 
	    case 468 : if (DEBUG) { System.out.println("AssertStatement ::= assert Expression SEMICOLON"); }  //$NON-NLS-1$
			    consumeSimpleAssertStatement() ;  
				break;
	 
	    case 469 : if (DEBUG) { System.out.println("AssertStatement ::= assert Expression COLON Expression"); }  //$NON-NLS-1$
			    consumeAssertStatement() ;  
				break;
	 
	    case 470 : if (DEBUG) { System.out.println("BreakStatement ::= break SEMICOLON"); }  //$NON-NLS-1$
			    consumeStatementBreak() ;  
				break;
	 
	    case 471 : if (DEBUG) { System.out.println("BreakStatement ::= break Identifier SEMICOLON"); }  //$NON-NLS-1$
			    consumeStatementBreakWithLabel() ;  
				break;
	 
	    case 472 : if (DEBUG) { System.out.println("ContinueStatement ::= continue SEMICOLON"); }  //$NON-NLS-1$
			    consumeStatementContinue() ;  
				break;
	 
	    case 473 : if (DEBUG) { System.out.println("ContinueStatement ::= continue Identifier SEMICOLON"); }  //$NON-NLS-1$
			    consumeStatementContinueWithLabel() ;  
				break;
	 
	    case 474 : if (DEBUG) { System.out.println("ReturnStatement ::= return Expressionopt SEMICOLON"); }  //$NON-NLS-1$
			    consumeStatementReturn() ;  
				break;
	 
	    case 475 : if (DEBUG) { System.out.println("ThrowStatement ::= throw Expression SEMICOLON"); }  //$NON-NLS-1$
			    consumeStatementThrow();  
				break;
	 
	    case 476 : if (DEBUG) { System.out.println("SynchronizedStatement ::= OnlySynchronized LPAREN..."); }  //$NON-NLS-1$
			    consumeStatementSynchronized();  
				break;
	 
	    case 477 : if (DEBUG) { System.out.println("OnlySynchronized ::= synchronized"); }  //$NON-NLS-1$
			    consumeOnlySynchronized();  
				break;
	 
	    case 478 : if (DEBUG) { System.out.println("TryStatement ::= try TryBlock Catches"); }  //$NON-NLS-1$
			    consumeStatementTry(false);  
				break;
	 
	    case 479 : if (DEBUG) { System.out.println("TryStatement ::= try TryBlock Catchesopt Finally"); }  //$NON-NLS-1$
			    consumeStatementTry(true);  
				break;
	 
	    case 481 : if (DEBUG) { System.out.println("ExitTryBlock ::="); }  //$NON-NLS-1$
			    consumeExitTryBlock();  
				break;
	 
	    case 483 : if (DEBUG) { System.out.println("Catches ::= Catches CatchClause"); }  //$NON-NLS-1$
			    consumeCatches();  
				break;
	 
	    case 484 : if (DEBUG) { System.out.println("CatchClause ::= catch LPAREN FormalParameter RPAREN..."); }  //$NON-NLS-1$
			    consumeStatementCatch() ;  
				break;
	 
	    case 486 : if (DEBUG) { System.out.println("PushLPAREN ::= LPAREN"); }  //$NON-NLS-1$
			    consumeLeftParen();  
				break;
	 
	    case 487 : if (DEBUG) { System.out.println("PushRPAREN ::= RPAREN"); }  //$NON-NLS-1$
			    consumeRightParen();  
				break;
	 
	    case 492 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= this"); }  //$NON-NLS-1$
			    consumePrimaryNoNewArrayThis();  
				break;
	 
	    case 493 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PushLPAREN Expression_NotName..."); }  //$NON-NLS-1$
			    consumePrimaryNoNewArray();  
				break;
	 
	    case 494 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PushLPAREN Name PushRPAREN"); }  //$NON-NLS-1$
			    consumePrimaryNoNewArrayWithName();  
				break;
	 
	    case 497 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= Name DOT this"); }  //$NON-NLS-1$
			    consumePrimaryNoNewArrayNameThis();  
				break;
	 
	    case 498 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= Name DOT super"); }  //$NON-NLS-1$
			    consumePrimaryNoNewArrayNameSuper();  
				break;
	 
	    case 499 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= Name DOT class"); }  //$NON-NLS-1$
			    consumePrimaryNoNewArrayName();  
				break;
	 
	    case 500 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= Name Dims DOT class"); }  //$NON-NLS-1$
			    consumePrimaryNoNewArrayArrayType();  
				break;
	 
	    case 501 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PrimitiveType Dims DOT class"); }  //$NON-NLS-1$
			    consumePrimaryNoNewArrayPrimitiveArrayType();  
				break;
	 
	    case 502 : if (DEBUG) { System.out.println("PrimaryNoNewArray ::= PrimitiveType DOT class"); }  //$NON-NLS-1$
			    consumePrimaryNoNewArrayPrimitiveType();  
				break;
	 
	    case 505 : if (DEBUG) { System.out.println("AllocationHeader ::= new ClassType LPAREN..."); }  //$NON-NLS-1$
			    consumeAllocationHeader();  
				break;
	 
	    case 506 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= new..."); }  //$NON-NLS-1$
			    consumeClassInstanceCreationExpressionWithTypeArguments();  
				break;
	 
	    case 507 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= new ClassType LPAREN"); }  //$NON-NLS-1$
			    consumeClassInstanceCreationExpression();  
				break;
	 
	    case 508 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= Primary DOT new..."); }  //$NON-NLS-1$
			    consumeClassInstanceCreationExpressionQualifiedWithTypeArguments() ;  
				break;
	 
	    case 509 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::= Primary DOT new..."); }  //$NON-NLS-1$
			    consumeClassInstanceCreationExpressionQualified() ;  
				break;
	 
	    case 510 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::=..."); }  //$NON-NLS-1$
			    consumeClassInstanceCreationExpressionQualified() ;  
				break;
	 
	    case 511 : if (DEBUG) { System.out.println("ClassInstanceCreationExpression ::=..."); }  //$NON-NLS-1$
			    consumeClassInstanceCreationExpressionQualifiedWithTypeArguments() ;  
				break;
	 
	    case 512 : if (DEBUG) { System.out.println("ClassInstanceCreationExpressionName ::= Name DOT"); }  //$NON-NLS-1$
			    consumeClassInstanceCreationExpressionName() ;  
				break;
	 
	    case 513 : if (DEBUG) { System.out.println("ClassBodyopt ::="); }  //$NON-NLS-1$
			    consumeClassBodyopt();  
				break;
	 
	    case 515 : if (DEBUG) { System.out.println("EnterAnonymousClassBody ::="); }  //$NON-NLS-1$
			    consumeEnterAnonymousClassBody();  
				break;
	 
	    case 517 : if (DEBUG) { System.out.println("ArgumentList ::= ArgumentList COMMA Expression"); }  //$NON-NLS-1$
			    consumeArgumentList();  
				break;
	 
	    case 518 : if (DEBUG) { System.out.println("ArrayCreationHeader ::= new PrimitiveType..."); }  //$NON-NLS-1$
			    consumeArrayCreationHeader();  
				break;
	 
	    case 519 : if (DEBUG) { System.out.println("ArrayCreationHeader ::= new ClassOrInterfaceType..."); }  //$NON-NLS-1$
			    consumeArrayCreationHeader();  
				break;
	 
	    case 520 : if (DEBUG) { System.out.println("ArrayCreationWithoutArrayInitializer ::= new..."); }  //$NON-NLS-1$
			    consumeArrayCreationExpressionWithoutInitializer();  
				break;
	 
	    case 521 : if (DEBUG) { System.out.println("ArrayCreationWithArrayInitializer ::= new PrimitiveType"); }  //$NON-NLS-1$
			    consumeArrayCreationExpressionWithInitializer();  
				break;
	 
	    case 522 : if (DEBUG) { System.out.println("ArrayCreationWithoutArrayInitializer ::= new..."); }  //$NON-NLS-1$
			    consumeArrayCreationExpressionWithoutInitializer();  
				break;
	 
	    case 523 : if (DEBUG) { System.out.println("ArrayCreationWithArrayInitializer ::= new..."); }  //$NON-NLS-1$
			    consumeArrayCreationExpressionWithInitializer();  
				break;
	 
	    case 525 : if (DEBUG) { System.out.println("DimWithOrWithOutExprs ::= DimWithOrWithOutExprs..."); }  //$NON-NLS-1$
			    consumeDimWithOrWithOutExprs();  
				break;
	 
	     case 527 : if (DEBUG) { System.out.println("DimWithOrWithOutExpr ::= LBRACKET RBRACKET"); }  //$NON-NLS-1$
			    consumeDimWithOrWithOutExpr();  
				break;
	 
	     case 528 : if (DEBUG) { System.out.println("Dims ::= DimsLoop"); }  //$NON-NLS-1$
			    consumeDims();  
				break;
	 
	     case 531 : if (DEBUG) { System.out.println("OneDimLoop ::= LBRACKET RBRACKET"); }  //$NON-NLS-1$
			    consumeOneDimLoop();  
				break;
	 
	    case 532 : if (DEBUG) { System.out.println("FieldAccess ::= Primary DOT JavaIdentifier"); }  //$NON-NLS-1$
			    consumeFieldAccess(false);  
				break;
	 
	    case 533 : if (DEBUG) { System.out.println("FieldAccess ::= super DOT JavaIdentifier"); }  //$NON-NLS-1$
			    consumeFieldAccess(true);  
				break;
	 
	    case 534 : if (DEBUG) { System.out.println("MethodInvocation ::= NameOrAj LPAREN ArgumentListopt..."); }  //$NON-NLS-1$
			    consumeMethodInvocationName();  
				break;
	 
	    case 535 : if (DEBUG) { System.out.println("MethodInvocation ::= Name DOT OnlyTypeArguments..."); }  //$NON-NLS-1$
			    consumeMethodInvocationNameWithTypeArguments();  
				break;
	 
	    case 536 : if (DEBUG) { System.out.println("MethodInvocation ::= Primary DOT OnlyTypeArguments..."); }  //$NON-NLS-1$
			    consumeMethodInvocationPrimaryWithTypeArguments();  
				break;
	 
	    case 537 : if (DEBUG) { System.out.println("MethodInvocation ::= Primary DOT JavaIdentifier LPAREN"); }  //$NON-NLS-1$
			    consumeMethodInvocationPrimary();  
				break;
	 
	    case 538 : if (DEBUG) { System.out.println("MethodInvocation ::= super DOT OnlyTypeArguments..."); }  //$NON-NLS-1$
			    consumeMethodInvocationSuperWithTypeArguments();  
				break;
	 
	    case 539 : if (DEBUG) { System.out.println("MethodInvocation ::= super DOT JavaIdentifier LPAREN..."); }  //$NON-NLS-1$
			    consumeMethodInvocationSuper();  
				break;
	 
	    case 540 : if (DEBUG) { System.out.println("ArrayAccess ::= Name LBRACKET Expression RBRACKET"); }  //$NON-NLS-1$
			    consumeArrayAccess(true);  
				break;
	 
	    case 541 : if (DEBUG) { System.out.println("ArrayAccess ::= PrimaryNoNewArray LBRACKET Expression..."); }  //$NON-NLS-1$
			    consumeArrayAccess(false);  
				break;
	 
	    case 542 : if (DEBUG) { System.out.println("ArrayAccess ::= ArrayCreationWithArrayInitializer..."); }  //$NON-NLS-1$
			    consumeArrayAccess(false);  
				break;
	 
	    case 544 : if (DEBUG) { System.out.println("PostfixExpression ::= NameOrAj"); }  //$NON-NLS-1$
			    consumePostfixExpression();  
				break;
	 
	    case 547 : if (DEBUG) { System.out.println("PostIncrementExpression ::= PostfixExpression PLUS_PLUS"); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.PLUS,true);  
				break;
	 
	    case 548 : if (DEBUG) { System.out.println("PostDecrementExpression ::= PostfixExpression..."); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.MINUS,true);  
				break;
	 
	    case 549 : if (DEBUG) { System.out.println("PushPosition ::="); }  //$NON-NLS-1$
			    consumePushPosition();  
				break;
	 
	    case 552 : if (DEBUG) { System.out.println("UnaryExpression ::= PLUS PushPosition UnaryExpression"); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.PLUS);  
				break;
	 
	    case 553 : if (DEBUG) { System.out.println("UnaryExpression ::= MINUS PushPosition UnaryExpression"); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.MINUS);  
				break;
	 
	    case 555 : if (DEBUG) { System.out.println("PreIncrementExpression ::= PLUS_PLUS PushPosition..."); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.PLUS,false);  
				break;
	 
	    case 556 : if (DEBUG) { System.out.println("PreDecrementExpression ::= MINUS_MINUS PushPosition..."); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.MINUS,false);  
				break;
	 
	    case 558 : if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus ::= TWIDDLE PushPosition..."); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.TWIDDLE);  
				break;
	 
	    case 559 : if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus ::= NOT PushPosition..."); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.NOT);  
				break;
	 
	    case 561 : if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN PrimitiveType Dimsopt..."); }  //$NON-NLS-1$
			    consumeCastExpressionWithPrimitiveType();  
				break;
	 
	    case 562 : if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name..."); }  //$NON-NLS-1$
			    consumeCastExpressionWithGenericsArray();  
				break;
	 
	    case 563 : if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name..."); }  //$NON-NLS-1$
			    consumeCastExpressionWithQualifiedGenericsArray();  
				break;
	 
	    case 564 : if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name PushRPAREN..."); }  //$NON-NLS-1$
			    consumeCastExpressionLL1();  
				break;
	 
	    case 565 : if (DEBUG) { System.out.println("CastExpression ::= PushLPAREN Name Dims PushRPAREN..."); }  //$NON-NLS-1$
			    consumeCastExpressionWithNameArray();  
				break;
	 
	    case 566 : if (DEBUG) { System.out.println("OnlyTypeArgumentsForCastExpression ::= OnlyTypeArguments"); }  //$NON-NLS-1$
			    consumeOnlyTypeArgumentsForCastExpression();  
				break;
	 
	    case 567 : if (DEBUG) { System.out.println("InsideCastExpression ::="); }  //$NON-NLS-1$
			    consumeInsideCastExpression();  
				break;
	 
	    case 568 : if (DEBUG) { System.out.println("InsideCastExpressionLL1 ::="); }  //$NON-NLS-1$
			    consumeInsideCastExpressionLL1();  
				break;
	 
	    case 569 : if (DEBUG) { System.out.println("InsideCastExpressionWithQualifiedGenerics ::="); }  //$NON-NLS-1$
			    consumeInsideCastExpressionWithQualifiedGenerics();  
				break;
	 
	    case 571 : if (DEBUG) { System.out.println("MultiplicativeExpression ::= MultiplicativeExpression..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.MULTIPLY);  
				break;
	 
	    case 572 : if (DEBUG) { System.out.println("MultiplicativeExpression ::= MultiplicativeExpression..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.DIVIDE);  
				break;
	 
	    case 573 : if (DEBUG) { System.out.println("MultiplicativeExpression ::= MultiplicativeExpression..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.REMAINDER);  
				break;
	 
	    case 575 : if (DEBUG) { System.out.println("AdditiveExpression ::= AdditiveExpression PLUS..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.PLUS);  
				break;
	 
	    case 576 : if (DEBUG) { System.out.println("AdditiveExpression ::= AdditiveExpression MINUS..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.MINUS);  
				break;
	 
	    case 578 : if (DEBUG) { System.out.println("ShiftExpression ::= ShiftExpression LEFT_SHIFT..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.LEFT_SHIFT);  
				break;
	 
	    case 579 : if (DEBUG) { System.out.println("ShiftExpression ::= ShiftExpression RIGHT_SHIFT..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.RIGHT_SHIFT);  
				break;
	 
	    case 580 : if (DEBUG) { System.out.println("ShiftExpression ::= ShiftExpression UNSIGNED_RIGHT_SHIFT"); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.UNSIGNED_RIGHT_SHIFT);  
				break;
	 
	    case 582 : if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression LESS..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.LESS);  
				break;
	 
	    case 583 : if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression GREATER..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.GREATER);  
				break;
	 
	    case 584 : if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression LESS_EQUAL"); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.LESS_EQUAL);  
				break;
	 
	    case 585 : if (DEBUG) { System.out.println("RelationalExpression ::= RelationalExpression..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.GREATER_EQUAL);  
				break;
	 
	    case 587 : if (DEBUG) { System.out.println("InstanceofExpression ::= InstanceofExpression instanceof"); }  //$NON-NLS-1$
			    consumeInstanceOfExpression(OperatorIds.INSTANCEOF);  
				break;
	 
	    case 589 : if (DEBUG) { System.out.println("EqualityExpression ::= EqualityExpression EQUAL_EQUAL..."); }  //$NON-NLS-1$
			    consumeEqualityExpression(OperatorIds.EQUAL_EQUAL);  
				break;
	 
	    case 590 : if (DEBUG) { System.out.println("EqualityExpression ::= EqualityExpression NOT_EQUAL..."); }  //$NON-NLS-1$
			    consumeEqualityExpression(OperatorIds.NOT_EQUAL);  
				break;
	 
	    case 592 : if (DEBUG) { System.out.println("AndExpression ::= AndExpression AND EqualityExpression"); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.AND);  
				break;
	 
	    case 594 : if (DEBUG) { System.out.println("ExclusiveOrExpression ::= ExclusiveOrExpression XOR..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.XOR);  
				break;
	 
	    case 596 : if (DEBUG) { System.out.println("InclusiveOrExpression ::= InclusiveOrExpression OR..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.OR);  
				break;
	 
	    case 598 : if (DEBUG) { System.out.println("ConditionalAndExpression ::= ConditionalAndExpression..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.AND_AND);  
				break;
	 
	    case 600 : if (DEBUG) { System.out.println("ConditionalOrExpression ::= ConditionalOrExpression..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.OR_OR);  
				break;
	 
	    case 602 : if (DEBUG) { System.out.println("ConditionalExpression ::= ConditionalOrExpression..."); }  //$NON-NLS-1$
			    consumeConditionalExpression(OperatorIds.QUESTIONCOLON) ;  
				break;
	 
	    case 605 : if (DEBUG) { System.out.println("Assignment ::= PostfixExpression AssignmentOperator..."); }  //$NON-NLS-1$
			    consumeAssignment();  
				break;
	 
	    case 607 : if (DEBUG) { System.out.println("Assignment ::= InvalidArrayInitializerAssignement"); }  //$NON-NLS-1$
			    ignoreExpressionAssignment(); 
				break;
	 
	    case 608 : if (DEBUG) { System.out.println("AssignmentOperator ::= EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(EQUAL);  
				break;
	 
	    case 609 : if (DEBUG) { System.out.println("AssignmentOperator ::= MULTIPLY_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(MULTIPLY);  
				break;
	 
	    case 610 : if (DEBUG) { System.out.println("AssignmentOperator ::= DIVIDE_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(DIVIDE);  
				break;
	 
	    case 611 : if (DEBUG) { System.out.println("AssignmentOperator ::= REMAINDER_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(REMAINDER);  
				break;
	 
	    case 612 : if (DEBUG) { System.out.println("AssignmentOperator ::= PLUS_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(PLUS);  
				break;
	 
	    case 613 : if (DEBUG) { System.out.println("AssignmentOperator ::= MINUS_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(MINUS);  
				break;
	 
	    case 614 : if (DEBUG) { System.out.println("AssignmentOperator ::= LEFT_SHIFT_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(LEFT_SHIFT);  
				break;
	 
	    case 615 : if (DEBUG) { System.out.println("AssignmentOperator ::= RIGHT_SHIFT_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(RIGHT_SHIFT);  
				break;
	 
	    case 616 : if (DEBUG) { System.out.println("AssignmentOperator ::= UNSIGNED_RIGHT_SHIFT_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(UNSIGNED_RIGHT_SHIFT);  
				break;
	 
	    case 617 : if (DEBUG) { System.out.println("AssignmentOperator ::= AND_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(AND);  
				break;
	 
	    case 618 : if (DEBUG) { System.out.println("AssignmentOperator ::= XOR_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(XOR);  
				break;
	 
	    case 619 : if (DEBUG) { System.out.println("AssignmentOperator ::= OR_EQUAL"); }  //$NON-NLS-1$
			    consumeAssignmentOperator(OR);  
				break;
	 
	    case 623 : if (DEBUG) { System.out.println("Expressionopt ::="); }  //$NON-NLS-1$
			    consumeEmptyExpression();  
				break;
	 
	    case 628 : if (DEBUG) { System.out.println("ClassBodyDeclarationsopt ::="); }  //$NON-NLS-1$
			    consumeEmptyClassBodyDeclarationsopt();  
				break;
	 
	    case 629 : if (DEBUG) { System.out.println("ClassBodyDeclarationsopt ::= NestedType..."); }  //$NON-NLS-1$
			    consumeClassBodyDeclarationsopt();  
				break;
	 
	     case 630 : if (DEBUG) { System.out.println("Modifiersopt ::="); }  //$NON-NLS-1$
			    consumeDefaultModifiers();  
				break;
	 
	    case 631 : if (DEBUG) { System.out.println("Modifiersopt ::= Modifiers"); }  //$NON-NLS-1$
			    consumeModifiers();  
				break;
	 
	    case 632 : if (DEBUG) { System.out.println("BlockStatementsopt ::="); }  //$NON-NLS-1$
			    consumeEmptyBlockStatementsopt();  
				break;
	 
	     case 634 : if (DEBUG) { System.out.println("Dimsopt ::="); }  //$NON-NLS-1$
			    consumeEmptyDimsopt();  
				break;
	 
	     case 636 : if (DEBUG) { System.out.println("ArgumentListopt ::="); }  //$NON-NLS-1$
			    consumeEmptyArgumentListopt();  
				break;
	 
	    case 640 : if (DEBUG) { System.out.println("FormalParameterListopt ::="); }  //$NON-NLS-1$
			    consumeFormalParameterListopt();  
				break;
	 
	     case 644 : if (DEBUG) { System.out.println("InterfaceMemberDeclarationsopt ::="); }  //$NON-NLS-1$
			    consumeEmptyInterfaceMemberDeclarationsopt();  
				break;
	 
	     case 645 : if (DEBUG) { System.out.println("InterfaceMemberDeclarationsopt ::= NestedType..."); }  //$NON-NLS-1$
			    consumeInterfaceMemberDeclarationsopt();  
				break;
	 
	    case 646 : if (DEBUG) { System.out.println("NestedType ::="); }  //$NON-NLS-1$
			    consumeNestedType();  
				break;

	     case 647 : if (DEBUG) { System.out.println("ForInitopt ::="); }  //$NON-NLS-1$
			    consumeEmptyForInitopt();  
				break;
	 
	     case 649 : if (DEBUG) { System.out.println("ForUpdateopt ::="); }  //$NON-NLS-1$
			    consumeEmptyForUpdateopt();  
				break;
	 
	     case 653 : if (DEBUG) { System.out.println("Catchesopt ::="); }  //$NON-NLS-1$
			    consumeEmptyCatchesopt();  
				break;
	 
	     case 655 : if (DEBUG) { System.out.println("EnumDeclaration ::= EnumHeader EnumBody"); }  //$NON-NLS-1$
			    consumeEnumDeclaration();  
				break;
	 
	     case 656 : if (DEBUG) { System.out.println("EnumHeader ::= EnumHeaderName ClassHeaderImplementsopt"); }  //$NON-NLS-1$
			    consumeEnumHeader();  
				break;
	 
	     case 657 : if (DEBUG) { System.out.println("EnumHeaderName ::= Modifiersopt enum JavaIdentifier"); }  //$NON-NLS-1$
			    consumeEnumHeaderName();  
				break;
	 
	     case 658 : if (DEBUG) { System.out.println("EnumBody ::= LBRACE EnumBodyDeclarationsopt RBRACE"); }  //$NON-NLS-1$
			    consumeEnumBodyNoConstants();  
				break;
	 
	     case 659 : if (DEBUG) { System.out.println("EnumBody ::= LBRACE COMMA EnumBodyDeclarationsopt..."); }  //$NON-NLS-1$
			    consumeEnumBodyNoConstants();  
				break;
	 
	     case 660 : if (DEBUG) { System.out.println("EnumBody ::= LBRACE EnumConstants COMMA..."); }  //$NON-NLS-1$
			    consumeEnumBodyWithConstants();  
				break;
	 
	     case 661 : if (DEBUG) { System.out.println("EnumBody ::= LBRACE EnumConstants..."); }  //$NON-NLS-1$
			    consumeEnumBodyWithConstants();  
				break;
	 
	    case 663 : if (DEBUG) { System.out.println("EnumConstants ::= EnumConstants COMMA EnumConstant"); }  //$NON-NLS-1$
			    consumeEnumConstants();  
				break;
	 
	    case 664 : if (DEBUG) { System.out.println("EnumConstantHeaderName ::= Modifiersopt Identifier"); }  //$NON-NLS-1$
			    consumeEnumConstantHeaderName();  
				break;
	 
	    case 665 : if (DEBUG) { System.out.println("EnumConstantHeader ::= EnumConstantHeaderName..."); }  //$NON-NLS-1$
			    consumeEnumConstantHeader();  
				break;
	 
	    case 666 : if (DEBUG) { System.out.println("EnumConstant ::= EnumConstantHeader ForceNoDiet..."); }  //$NON-NLS-1$
			    consumeEnumConstantWithClassBody();  
				break;
	 
	    case 667 : if (DEBUG) { System.out.println("EnumConstant ::= EnumConstantHeader"); }  //$NON-NLS-1$
			    consumeEnumConstantNoClassBody();  
				break;
	 
	    case 668 : if (DEBUG) { System.out.println("Arguments ::= LPAREN ArgumentListopt RPAREN"); }  //$NON-NLS-1$
			    consumeArguments();  
				break;
	 
	    case 669 : if (DEBUG) { System.out.println("Argumentsopt ::="); }  //$NON-NLS-1$
			    consumeEmptyArguments();  
				break;
	 
	    case 671 : if (DEBUG) { System.out.println("EnumDeclarations ::= SEMICOLON ClassBodyDeclarationsopt"); }  //$NON-NLS-1$
			    consumeEnumDeclarations();  
				break;
	 
	    case 672 : if (DEBUG) { System.out.println("EnumBodyDeclarationsopt ::="); }  //$NON-NLS-1$
			    consumeEmptyEnumDeclarations();  
				break;
	 
	    case 674 : if (DEBUG) { System.out.println("EnhancedForStatement ::= EnhancedForStatementHeader..."); }  //$NON-NLS-1$
			    consumeEnhancedForStatement();  
				break;
	 
	    case 675 : if (DEBUG) { System.out.println("EnhancedForStatementNoShortIf ::=..."); }  //$NON-NLS-1$
			    consumeEnhancedForStatement();  
				break;
	 
	    case 676 : if (DEBUG) { System.out.println("EnhancedForStatementHeader ::= for LPAREN Type..."); }  //$NON-NLS-1$
			    consumeEnhancedForStatementHeader(false);  
				break;
	 
	    case 677 : if (DEBUG) { System.out.println("EnhancedForStatementHeader ::= for LPAREN Modifiers Type"); }  //$NON-NLS-1$
			    consumeEnhancedForStatementHeader(true);  
				break;
	 
	    case 678 : if (DEBUG) { System.out.println("SingleStaticImportDeclaration ::=..."); }  //$NON-NLS-1$
			    consumeImportDeclaration();  
				break;
	 
	    case 679 : if (DEBUG) { System.out.println("SingleStaticImportDeclarationName ::= import static Name"); }  //$NON-NLS-1$
			    consumeSingleStaticImportDeclarationName();  
				break;
	 
	    case 680 : if (DEBUG) { System.out.println("StaticImportOnDemandDeclaration ::=..."); }  //$NON-NLS-1$
			    consumeImportDeclaration();  
				break;
	 
	    case 681 : if (DEBUG) { System.out.println("StaticImportOnDemandDeclarationName ::= import static..."); }  //$NON-NLS-1$
			    consumeStaticImportOnDemandDeclarationName();  
				break;
	 
	    case 682 : if (DEBUG) { System.out.println("TypeArguments ::= LESS TypeArgumentList1"); }  //$NON-NLS-1$
			    consumeTypeArguments();  
				break;
	 
	    case 683 : if (DEBUG) { System.out.println("OnlyTypeArguments ::= LESS TypeArgumentList1"); }  //$NON-NLS-1$
			    consumeOnlyTypeArguments();  
				break;
	 
	    case 685 : if (DEBUG) { System.out.println("TypeArgumentList1 ::= TypeArgumentList COMMA..."); }  //$NON-NLS-1$
			    consumeTypeArgumentList1();  
				break;
	 
	    case 687 : if (DEBUG) { System.out.println("TypeArgumentList ::= TypeArgumentList COMMA TypeArgument"); }  //$NON-NLS-1$
			    consumeTypeArgumentList();  
				break;
	 
	    case 688 : if (DEBUG) { System.out.println("TypeArgument ::= ReferenceType"); }  //$NON-NLS-1$
			    consumeTypeArgument();  
				break;
	 
	    case 692 : if (DEBUG) { System.out.println("ReferenceType1 ::= ReferenceType GREATER"); }  //$NON-NLS-1$
			    consumeReferenceType1();  
				break;
	 
	    case 693 : if (DEBUG) { System.out.println("ReferenceType1 ::= ClassOrInterface LESS..."); }  //$NON-NLS-1$
			    consumeTypeArgumentReferenceType1();  
				break;
	 
	    case 695 : if (DEBUG) { System.out.println("TypeArgumentList2 ::= TypeArgumentList COMMA..."); }  //$NON-NLS-1$
			    consumeTypeArgumentList2();  
				break;
	 
	    case 698 : if (DEBUG) { System.out.println("ReferenceType2 ::= ReferenceType RIGHT_SHIFT"); }  //$NON-NLS-1$
			    consumeReferenceType2();  
				break;
	 
	    case 699 : if (DEBUG) { System.out.println("ReferenceType2 ::= ClassOrInterface LESS..."); }  //$NON-NLS-1$
			    consumeTypeArgumentReferenceType2();  
				break;
	 
	    case 701 : if (DEBUG) { System.out.println("TypeArgumentList3 ::= TypeArgumentList COMMA..."); }  //$NON-NLS-1$
			    consumeTypeArgumentList3();  
				break;
	 
	    case 704 : if (DEBUG) { System.out.println("ReferenceType3 ::= ReferenceType UNSIGNED_RIGHT_SHIFT"); }  //$NON-NLS-1$
			    consumeReferenceType3();  
				break;
	 
	    case 705 : if (DEBUG) { System.out.println("Wildcard ::= QUESTION"); }  //$NON-NLS-1$
			    consumeWildcard();  
				break;
	 
	    case 706 : if (DEBUG) { System.out.println("Wildcard ::= QUESTION WildcardBounds"); }  //$NON-NLS-1$
			    consumeWildcardWithBounds();  
				break;
	 
	    case 707 : if (DEBUG) { System.out.println("WildcardBounds ::= extends ReferenceType"); }  //$NON-NLS-1$
			    consumeWildcardBoundsExtends();  
				break;
	 
	    case 708 : if (DEBUG) { System.out.println("WildcardBounds ::= super ReferenceType"); }  //$NON-NLS-1$
			    consumeWildcardBoundsSuper();  
				break;
	 
	    case 709 : if (DEBUG) { System.out.println("Wildcard1 ::= QUESTION GREATER"); }  //$NON-NLS-1$
			    consumeWildcard1();  
				break;
	 
	    case 710 : if (DEBUG) { System.out.println("Wildcard1 ::= QUESTION WildcardBounds1"); }  //$NON-NLS-1$
			    consumeWildcard1WithBounds();  
				break;
	 
	    case 711 : if (DEBUG) { System.out.println("WildcardBounds1 ::= extends ReferenceType1"); }  //$NON-NLS-1$
			    consumeWildcardBounds1Extends();  
				break;
	 
	    case 712 : if (DEBUG) { System.out.println("WildcardBounds1 ::= super ReferenceType1"); }  //$NON-NLS-1$
			    consumeWildcardBounds1Super();  
				break;
	 
	    case 713 : if (DEBUG) { System.out.println("Wildcard2 ::= QUESTION RIGHT_SHIFT"); }  //$NON-NLS-1$
			    consumeWildcard2();  
				break;
	 
	    case 714 : if (DEBUG) { System.out.println("Wildcard2 ::= QUESTION WildcardBounds2"); }  //$NON-NLS-1$
			    consumeWildcard2WithBounds();  
				break;
	 
	    case 715 : if (DEBUG) { System.out.println("WildcardBounds2 ::= extends ReferenceType2"); }  //$NON-NLS-1$
			    consumeWildcardBounds2Extends();  
				break;
	 
	    case 716 : if (DEBUG) { System.out.println("WildcardBounds2 ::= super ReferenceType2"); }  //$NON-NLS-1$
			    consumeWildcardBounds2Super();  
				break;
	 
	    case 717 : if (DEBUG) { System.out.println("Wildcard3 ::= QUESTION UNSIGNED_RIGHT_SHIFT"); }  //$NON-NLS-1$
			    consumeWildcard3();  
				break;
	 
	    case 718 : if (DEBUG) { System.out.println("Wildcard3 ::= QUESTION WildcardBounds3"); }  //$NON-NLS-1$
			    consumeWildcard3WithBounds();  
				break;
	 
	    case 719 : if (DEBUG) { System.out.println("WildcardBounds3 ::= extends ReferenceType3"); }  //$NON-NLS-1$
			    consumeWildcardBounds3Extends();  
				break;
	 
	    case 720 : if (DEBUG) { System.out.println("WildcardBounds3 ::= super ReferenceType3"); }  //$NON-NLS-1$
			    consumeWildcardBounds3Super();  
				break;
	 
	    case 721 : if (DEBUG) { System.out.println("TypeParameterHeader ::= JavaIdentifier"); }  //$NON-NLS-1$
			    consumeTypeParameterHeader();  
				break;
	 
	    case 722 : if (DEBUG) { System.out.println("TypeParameters ::= LESS TypeParameterList1"); }  //$NON-NLS-1$
			    consumeTypeParameters();  
				break;
	 
	    case 724 : if (DEBUG) { System.out.println("TypeParameterList ::= TypeParameterList COMMA..."); }  //$NON-NLS-1$
			    consumeTypeParameterList();  
				break;
	 
	    case 726 : if (DEBUG) { System.out.println("TypeParameter ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
			    consumeTypeParameterWithExtends();  
				break;
	 
	    case 727 : if (DEBUG) { System.out.println("TypeParameter ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
			    consumeTypeParameterWithExtendsAndBounds();  
				break;
	 
	    case 729 : if (DEBUG) { System.out.println("AdditionalBoundList ::= AdditionalBoundList..."); }  //$NON-NLS-1$
			    consumeAdditionalBoundList();  
				break;
	 
	    case 730 : if (DEBUG) { System.out.println("AdditionalBound ::= AND ReferenceType"); }  //$NON-NLS-1$
			    consumeAdditionalBound();  
				break;
	 
	    case 732 : if (DEBUG) { System.out.println("TypeParameterList1 ::= TypeParameterList COMMA..."); }  //$NON-NLS-1$
			    consumeTypeParameterList1();  
				break;
	 
	    case 733 : if (DEBUG) { System.out.println("TypeParameter1 ::= TypeParameterHeader GREATER"); }  //$NON-NLS-1$
			    consumeTypeParameter1();  
				break;
	 
	    case 734 : if (DEBUG) { System.out.println("TypeParameter1 ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
			    consumeTypeParameter1WithExtends();  
				break;
	 
	    case 735 : if (DEBUG) { System.out.println("TypeParameter1 ::= TypeParameterHeader extends..."); }  //$NON-NLS-1$
			    consumeTypeParameter1WithExtendsAndBounds();  
				break;
	 
	    case 737 : if (DEBUG) { System.out.println("AdditionalBoundList1 ::= AdditionalBoundList..."); }  //$NON-NLS-1$
			    consumeAdditionalBoundList1();  
				break;
	 
	    case 738 : if (DEBUG) { System.out.println("AdditionalBound1 ::= AND ReferenceType1"); }  //$NON-NLS-1$
			    consumeAdditionalBound1();  
				break;
	 
	    case 744 : if (DEBUG) { System.out.println("UnaryExpression_NotName ::= PLUS PushPosition..."); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.PLUS);  
				break;
	 
	    case 745 : if (DEBUG) { System.out.println("UnaryExpression_NotName ::= MINUS PushPosition..."); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.MINUS);  
				break;
	 
	    case 748 : if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus_NotName ::= TWIDDLE..."); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.TWIDDLE);  
				break;
	 
	    case 749 : if (DEBUG) { System.out.println("UnaryExpressionNotPlusMinus_NotName ::= NOT PushPosition"); }  //$NON-NLS-1$
			    consumeUnaryExpression(OperatorIds.NOT);  
				break;
	 
	    case 752 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.MULTIPLY);  
				break;
	 
	    case 753 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::= Name MULTIPLY..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.MULTIPLY);  
				break;
	 
	    case 754 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.DIVIDE);  
				break;
	 
	    case 755 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::= Name DIVIDE..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.DIVIDE);  
				break;
	 
	    case 756 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.REMAINDER);  
				break;
	 
	    case 757 : if (DEBUG) { System.out.println("MultiplicativeExpression_NotName ::= Name REMAINDER..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.REMAINDER);  
				break;
	 
	    case 759 : if (DEBUG) { System.out.println("AdditiveExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.PLUS);  
				break;
	 
	    case 760 : if (DEBUG) { System.out.println("AdditiveExpression_NotName ::= Name PLUS..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.PLUS);  
				break;
	 
	    case 761 : if (DEBUG) { System.out.println("AdditiveExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.MINUS);  
				break;
	 
	    case 762 : if (DEBUG) { System.out.println("AdditiveExpression_NotName ::= Name MINUS..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.MINUS);  
				break;
	 
	    case 764 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= ShiftExpression_NotName..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.LEFT_SHIFT);  
				break;
	 
	    case 765 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= Name LEFT_SHIFT..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.LEFT_SHIFT);  
				break;
	 
	    case 766 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= ShiftExpression_NotName..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.RIGHT_SHIFT);  
				break;
	 
	    case 767 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= Name RIGHT_SHIFT..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.RIGHT_SHIFT);  
				break;
	 
	    case 768 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= ShiftExpression_NotName..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.UNSIGNED_RIGHT_SHIFT);  
				break;
	 
	    case 769 : if (DEBUG) { System.out.println("ShiftExpression_NotName ::= Name UNSIGNED_RIGHT_SHIFT..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.UNSIGNED_RIGHT_SHIFT);  
				break;
	 
	    case 771 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= ShiftExpression_NotName"); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.LESS);  
				break;
	 
	    case 772 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= Name LESS..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.LESS);  
				break;
	 
	    case 773 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= ShiftExpression_NotName"); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.GREATER);  
				break;
	 
	    case 774 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= Name GREATER..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.GREATER);  
				break;
	 
	    case 775 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.LESS_EQUAL);  
				break;
	 
	    case 776 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= Name LESS_EQUAL..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.LESS_EQUAL);  
				break;
	 
	    case 777 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.GREATER_EQUAL);  
				break;
	 
	    case 778 : if (DEBUG) { System.out.println("RelationalExpression_NotName ::= Name GREATER_EQUAL..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.GREATER_EQUAL);  
				break;
	 
	    case 780 : if (DEBUG) { System.out.println("InstanceofExpression_NotName ::= Name instanceof..."); }  //$NON-NLS-1$
			    consumeInstanceOfExpressionWithName(OperatorIds.INSTANCEOF);  
				break;
	 
	    case 781 : if (DEBUG) { System.out.println("InstanceofExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeInstanceOfExpression(OperatorIds.INSTANCEOF);  
				break;
	 
	    case 783 : if (DEBUG) { System.out.println("EqualityExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeEqualityExpression(OperatorIds.EQUAL_EQUAL);  
				break;
	 
	    case 784 : if (DEBUG) { System.out.println("EqualityExpression_NotName ::= Name EQUAL_EQUAL..."); }  //$NON-NLS-1$
			    consumeEqualityExpressionWithName(OperatorIds.EQUAL_EQUAL);  
				break;
	 
	    case 785 : if (DEBUG) { System.out.println("EqualityExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeEqualityExpression(OperatorIds.NOT_EQUAL);  
				break;
	 
	    case 786 : if (DEBUG) { System.out.println("EqualityExpression_NotName ::= Name NOT_EQUAL..."); }  //$NON-NLS-1$
			    consumeEqualityExpressionWithName(OperatorIds.NOT_EQUAL);  
				break;
	 
	    case 788 : if (DEBUG) { System.out.println("AndExpression_NotName ::= AndExpression_NotName AND..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.AND);  
				break;
	 
	    case 789 : if (DEBUG) { System.out.println("AndExpression_NotName ::= Name AND EqualityExpression"); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.AND);  
				break;
	 
	    case 791 : if (DEBUG) { System.out.println("ExclusiveOrExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.XOR);  
				break;
	 
	    case 792 : if (DEBUG) { System.out.println("ExclusiveOrExpression_NotName ::= Name XOR AndExpression"); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.XOR);  
				break;
	 
	    case 794 : if (DEBUG) { System.out.println("InclusiveOrExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.OR);  
				break;
	 
	    case 795 : if (DEBUG) { System.out.println("InclusiveOrExpression_NotName ::= Name OR..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.OR);  
				break;
	 
	    case 797 : if (DEBUG) { System.out.println("ConditionalAndExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.AND_AND);  
				break;
	 
	    case 798 : if (DEBUG) { System.out.println("ConditionalAndExpression_NotName ::= Name AND_AND..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.AND_AND);  
				break;
	 
	    case 800 : if (DEBUG) { System.out.println("ConditionalOrExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeBinaryExpression(OperatorIds.OR_OR);  
				break;
	 
	    case 801 : if (DEBUG) { System.out.println("ConditionalOrExpression_NotName ::= Name OR_OR..."); }  //$NON-NLS-1$
			    consumeBinaryExpressionWithName(OperatorIds.OR_OR);  
				break;
	 
	    case 803 : if (DEBUG) { System.out.println("ConditionalExpression_NotName ::=..."); }  //$NON-NLS-1$
			    consumeConditionalExpression(OperatorIds.QUESTIONCOLON) ;  
				break;
	 
	    case 804 : if (DEBUG) { System.out.println("ConditionalExpression_NotName ::= Name QUESTION..."); }  //$NON-NLS-1$
			    consumeConditionalExpressionWithName(OperatorIds.QUESTIONCOLON) ;  
				break;
	 
	    case 808 : if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeaderName ::= Modifiers AT..."); }  //$NON-NLS-1$
			    consumeAnnotationTypeDeclarationHeaderName() ;  
				break;
	 
	    case 809 : if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeaderName ::= AT..."); }  //$NON-NLS-1$
			    consumeAnnotationTypeDeclarationHeaderName() ;  
				break;
	 
	    case 810 : if (DEBUG) { System.out.println("AnnotationTypeDeclarationHeader ::=..."); }  //$NON-NLS-1$
			    consumeAnnotationTypeDeclarationHeader() ;  
				break;
	 
	    case 811 : if (DEBUG) { System.out.println("AnnotationTypeDeclaration ::=..."); }  //$NON-NLS-1$
			    consumeAnnotationTypeDeclaration() ;  
				break;
	 
	    case 813 : if (DEBUG) { System.out.println("AnnotationTypeMemberDeclarationsopt ::="); }  //$NON-NLS-1$
			    consumeEmptyAnnotationTypeMemberDeclarationsopt() ;  
				break;
	 
	    case 816 : if (DEBUG) { System.out.println("AnnotationTypeMemberDeclarations ::=..."); }  //$NON-NLS-1$
			    consumeAnnotationTypeMemberDeclarations() ;  
				break;
	 
	    case 817 : if (DEBUG) { System.out.println("AnnotationMethodHeaderName ::= Modifiersopt..."); }  //$NON-NLS-1$
			    consumeMethodHeaderNameWithTypeParameters(true);  
				break;
	 
	    case 818 : if (DEBUG) { System.out.println("AnnotationMethodHeaderName ::= Modifiersopt Type..."); }  //$NON-NLS-1$
			    consumeMethodHeaderName(true);  
				break;
	 
	    case 819 : if (DEBUG) { System.out.println("AnnotationMethodHeaderDefaultValueopt ::="); }  //$NON-NLS-1$
			    consumeEmptyMethodHeaderDefaultValue() ;  
				break;
	 
	    case 820 : if (DEBUG) { System.out.println("AnnotationMethodHeaderDefaultValueopt ::= DefaultValue"); }  //$NON-NLS-1$
			    consumeMethodHeaderDefaultValue();  
				break;
	 
	    case 821 : if (DEBUG) { System.out.println("AnnotationMethodHeader ::= AnnotationMethodHeaderName..."); }  //$NON-NLS-1$
			    consumeMethodHeader();  
				break;
	 
	    case 822 : if (DEBUG) { System.out.println("AnnotationTypeMemberDeclaration ::=..."); }  //$NON-NLS-1$
			    consumeAnnotationTypeMemberDeclaration() ;  
				break;
	 
	    case 830 : if (DEBUG) { System.out.println("AnnotationName ::= AT NameOrAj"); }  //$NON-NLS-1$
			    consumeAnnotationName() ;  
				break;
	 
	    case 831 : if (DEBUG) { System.out.println("NormalAnnotation ::= AnnotationName LPAREN..."); }  //$NON-NLS-1$
			    consumeNormalAnnotation() ;  
				break;
	 
	    case 832 : if (DEBUG) { System.out.println("MemberValuePairsopt ::="); }  //$NON-NLS-1$
			    consumeEmptyMemberValuePairsopt() ;  
				break;
	 
	    case 835 : if (DEBUG) { System.out.println("MemberValuePairs ::= MemberValuePairs COMMA..."); }  //$NON-NLS-1$
			    consumeMemberValuePairs() ;  
				break;
	 
	    case 836 : if (DEBUG) { System.out.println("MemberValuePair ::= SimpleNameOrAj EQUAL..."); }  //$NON-NLS-1$
			    consumeMemberValuePair() ;  
				break;
	 
	    case 837 : if (DEBUG) { System.out.println("EnterMemberValue ::="); }  //$NON-NLS-1$
			    consumeEnterMemberValue() ;  
				break;
	 
	    case 838 : if (DEBUG) { System.out.println("ExitMemberValue ::="); }  //$NON-NLS-1$
			    consumeExitMemberValue() ;  
				break;
	 
	    case 840 : if (DEBUG) { System.out.println("MemberValue ::= NameOrAj"); }  //$NON-NLS-1$
			    consumeMemberValueAsName() ;  
				break;
	 
	    case 843 : if (DEBUG) { System.out.println("MemberValueArrayInitializer ::= LBRACE PushLeftBrace..."); }  //$NON-NLS-1$
			    consumeMemberValueArrayInitializer() ;  
				break;
	 
	    case 844 : if (DEBUG) { System.out.println("MemberValueArrayInitializer ::= LBRACE PushLeftBrace..."); }  //$NON-NLS-1$
			    consumeMemberValueArrayInitializer() ;  
				break;
	 
	    case 845 : if (DEBUG) { System.out.println("MemberValueArrayInitializer ::= LBRACE PushLeftBrace..."); }  //$NON-NLS-1$
			    consumeEmptyMemberValueArrayInitializer() ;  
				break;
	 
	    case 846 : if (DEBUG) { System.out.println("MemberValueArrayInitializer ::= LBRACE PushLeftBrace..."); }  //$NON-NLS-1$
			    consumeEmptyMemberValueArrayInitializer() ;  
				break;
	 
	    case 848 : if (DEBUG) { System.out.println("MemberValues ::= MemberValues COMMA MemberValue"); }  //$NON-NLS-1$
			    consumeMemberValues() ;  
				break;
	 
	    case 849 : if (DEBUG) { System.out.println("MarkerAnnotation ::= AnnotationName"); }  //$NON-NLS-1$
			    consumeMarkerAnnotation() ;  
				break;
	 
	    case 850 : if (DEBUG) { System.out.println("SingleMemberAnnotation ::= AnnotationName LPAREN..."); }  //$NON-NLS-1$
			    consumeSingleMemberAnnotation() ;  
				break;
	 
	    case 851 : if (DEBUG) { System.out.println("RecoveryMethodHeaderName ::= Modifiersopt TypeParameters"); }  //$NON-NLS-1$
			    consumeRecoveryMethodHeaderNameWithTypeParameters();  
				break;
	 
	    case 852 : if (DEBUG) { System.out.println("RecoveryMethodHeaderName ::= Modifiersopt Type..."); }  //$NON-NLS-1$
			    consumeRecoveryMethodHeaderName();  
				break;
	 
	    case 853 : if (DEBUG) { System.out.println("RecoveryMethodHeader ::= RecoveryMethodHeaderName..."); }  //$NON-NLS-1$
			    consumeMethodHeader();  
				break;
	 
	    case 854 : if (DEBUG) { System.out.println("RecoveryMethodHeader ::= RecoveryMethodHeaderName..."); }  //$NON-NLS-1$
			    consumeMethodHeader();  
				break;
	 
		}
	}

	
protected void consumeSimpleAssertStatement() {
	super.consumeSimpleAssertStatement();
}


public Parser(
	ProblemReporter problemReporter,
	boolean optimizeStringLiterals) {
	super(problemReporter, optimizeStringLiterals);
}

// don't try to recover if we're parsing AspectJ constructs
protected boolean shouldTryToRecover() {
	int index = 0;
	ASTNode node;
	while (index < astStack.length && (node = astStack[index++]) != null) {
		if (!declarationFactory.shouldTryToRecover(node)) {
			return false;
		}
	}
	return true;
}

protected void pushOnAspectIntStack(int pos) {

	int stackLength = this.aspectIntStack.length;
	if (++this.aspectIntPtr >= stackLength) {
		System.arraycopy(
			this.aspectIntStack, 0,
			this.aspectIntStack = new int[stackLength + StackIncrement], 0,
			stackLength);
	}
	this.aspectIntStack[this.aspectIntPtr] = pos;
}
}
