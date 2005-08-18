/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.jdt.internal.compiler.parser;

/*An interface that contains static declarations for some basic information
 about the parser such as the number of rules in the grammar, the starting state, etc...*/
public interface ParserBasicInformation {

	int 
	
	ERROR_SYMBOL      = 117,
    MAX_NAME_LENGTH   = 41,
    NUM_STATES        = 1095,

    NT_OFFSET         = 117,
    SCOPE_UBOUND      = 170,
    SCOPE_SIZE        = 171,
    LA_STATE_OFFSET   = 15082,
    MAX_LA            = 1,
    NUM_RULES         = 859,
    NUM_TERMINALS     = 117,
    NUM_NON_TERMINALS = 360,
    NUM_SYMBOLS       = 477,
    START_STATE       = 904,
    EOFT_SYMBOL       = 75,
    EOLT_SYMBOL       = 75,
    ACCEPT_ACTION     = 15081,
    ERROR_ACTION      = 15082;
}
