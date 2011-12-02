/*******************************************************************************
 * Copyright (c) 2000, 2011 IBM Corporation and others.
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

	 public final static int

     ERROR_SYMBOL      = 117,
     MAX_NAME_LENGTH   = 36,
     NUM_STATES        = 1179,

     NT_OFFSET         = 117,
     SCOPE_UBOUND      = -1,
     SCOPE_SIZE        = 0,
     LA_STATE_OFFSET   = 16003,
     MAX_LA            = 1,
     NUM_RULES         = 915,
     NUM_TERMINALS     = 117,
     NUM_NON_TERMINALS = 386,
     NUM_SYMBOLS       = 503,
     START_STATE       = 946,
     EOFT_SYMBOL       = 75,
     EOLT_SYMBOL       = 75,
     ACCEPT_ACTION     = 16002,
     ERROR_ACTION      = 16003;
}
