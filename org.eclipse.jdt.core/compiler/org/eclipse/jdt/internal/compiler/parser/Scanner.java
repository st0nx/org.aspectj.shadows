/* *******************************************************************
 * Copyright (c) 2002 Palo Alto Research Center, Incorporated (PARC).
 *               2003,2004 contributors
 * All rights reserved. 
 * This program and the accompanying materials are made available 
 * under the terms of the Common Public License v1.0 
 * which accompanies this distribution and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html 
 *  
 * Contributors: 
 *     PARC     initial implementation 
 * ******************************************************************/
package org.eclipse.jdt.internal.compiler.parser;

import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.internal.compiler.parser.TerminalTokens;

// AspectJ Extension. The original scanner class is replaced by this one.
public class Scanner extends TheOriginalJDTScannerClass implements TerminalTokens {
	public Scanner(
		boolean tokenizeComments,
		boolean tokenizeWhiteSpace,
		boolean checkNonExternalizedStringLiterals,
		long sourceLevel,
		char[][] taskTags,
		char[][] taskPriorities) {
		super(
			tokenizeComments,
			tokenizeWhiteSpace,
			checkNonExternalizedStringLiterals,
			sourceLevel,
			taskTags,
			taskPriorities);
	}

	public Scanner() {
		super();
	}


	
	
	private static final char[] aspectV = "aspect".toCharArray();
	private static final char[] pointcutV = "pointcut".toCharArray();
	private static final char[] privilegedV = "privileged".toCharArray();
	private static final char[] beforeV = "before".toCharArray();
	private static final char[] afterV = "after".toCharArray();
	private static final char[] aroundV = "around".toCharArray();
	private static final char[] declareV = "declare".toCharArray();
	
	
	
	public int scanIdentifierOrKeyword() {
		int kind = super.scanIdentifierOrKeyword();
		if (kind != TerminalTokens.TokenNameIdentifier) return kind;
		
		char[] contents = getCurrentIdentifierSource();
		
		//XXX performance here is less than optimal, but code simplicity is pretty damn good
		if (CharOperation.equals(aspectV, contents)) return TerminalTokens.TokenNameaspect;
		else if (CharOperation.equals(pointcutV, contents)) return TerminalTokens.TokenNamepointcut;
		else if (CharOperation.equals(privilegedV, contents)) return TerminalTokens.TokenNameprivileged;
		else if (CharOperation.equals(beforeV, contents)) return TerminalTokens.TokenNamebefore;
		else if (CharOperation.equals(afterV, contents)) return TerminalTokens.TokenNameafter;
		else if (CharOperation.equals(aroundV, contents)) return TerminalTokens.TokenNamearound;
		else if (CharOperation.equals(declareV, contents)) return TerminalTokens.TokenNamedeclare;
	
		return kind;
	}
}
