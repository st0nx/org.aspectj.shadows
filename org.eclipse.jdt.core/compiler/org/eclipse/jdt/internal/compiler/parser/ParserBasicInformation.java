/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Palo Alto Research Center, Incorporated - AspectJ adaptation
 ******************************************************************************/
package org.eclipse.jdt.internal.compiler.parser;

/**An interface that contains static declarations for some basic information
 * about the parser such as the number of rules in the grammar, the starting state, etc...
 *
 *
 * <p><b>The contents of this file are generated automatically by jikespg from
 * org/eclipse/jdt/internal/compiler/parser/aj_grammar.txt.  This 
 * auto-generation means that all symbols have different values than
 * they do when generated from the Java grammar.  This design needs to be 
 * changed for AspectJ to be implemented as a clean extension to the jdt compiler.</b>
 * 
 * XXX unacceptable extension design
 */

public interface ParserBasicInformation {
	public final static int

	  ERROR_SYMBOL      = 112,
	  MAX_NAME_LENGTH   = 36,
	  NUM_STATES        = 685,

	  NT_OFFSET         = 112,
	  SCOPE_UBOUND      = 69,
	  SCOPE_SIZE        = 70,
	  LA_STATE_OFFSET   = 7502,
	  MAX_LA            = 1,
	  NUM_RULES         = 553,
	  NUM_TERMINALS     = 112,
	  NUM_NON_TERMINALS = 242,
	  NUM_SYMBOLS       = 354,
	  START_STATE       = 767,
	  EOFT_SYMBOL       = 75,
	  EOLT_SYMBOL       = 75,
	  ACCEPT_ACTION     = 7501,
	  ERROR_ACTION      = 7502;
}
