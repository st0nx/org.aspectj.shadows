/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.jdt.internal.compiler.batch;

/* AspectJ Extension */
// this file has a number of changes made by ASC to prevent us from
// leaking OS resources by keeping jars open longer than needed.

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.eclipse.jdt.internal.compiler.classfmt.ClassFileReader;
import org.eclipse.jdt.internal.compiler.env.NameEnvironmentAnswer;

public class ClasspathJar implements FileSystem.Classpath {
	
	/**
	 * ASC 23112004
	 * These fields and related logic throughout the class enable us to cope with very long
	 * classpaths.  Normally all the archives on the classpath were openede and left open
	 * for the duration of a compile - this doesn't scale since you will start running out
	 * of file handles when you have extremely long classpaths (e.g. 4000 jars).  These
	 * fields enable us to keep track of how many are currently open and if you attempt to
	 * open more than some defined limit, it will close some that you haven't used for a
	 * while before opening the new one.  The limit is tailorable through the
	 *   org.aspectj.weaver.openarchives
	 * system property, the default is 1000 which means most users will never exercise
	 * this logic.  The only change outside of this class related to this feature is that
	 * the FileSystem class now constructs a ClasspathJar object by passing in a File
	 * rather than a ZipFile - it is then the responsibility of this class to
	 * open and manage the ZipFile.
	 */
	private static int maxOpenArchives = 1000;
	private final static int MAXOPEN_DEFAULT = 1000;
    private static List openArchives = new ArrayList();
    private File f;


	
ZipFile zipFile;
Hashtable packageCache;
boolean closeZipFileAtEnd;


	
static {
	String openarchivesString = getSystemPropertyWithoutSecurityException("org.aspectj.weaver.openarchives",Integer.toString(MAXOPEN_DEFAULT));
	maxOpenArchives=Integer.parseInt(openarchivesString);
	if (maxOpenArchives<20) maxOpenArchives=1000;
}
	
	
public ClasspathJar(File file) throws IOException {
	this(file, true);
}
private ClasspathJar(File f, boolean closeZipFileAtEnd) throws IOException {
	if (!f.exists()) throw new FileNotFoundException("Jar does not exist :"+f.getName());
	this.f = f;
	this.packageCache = null;
	this.closeZipFileAtEnd = closeZipFileAtEnd;
}	
public NameEnvironmentAnswer findClass(char[] typeName, String qualifiedPackageName, String qualifiedBinaryFileName) {
	if (!isPackage(qualifiedPackageName)) 
		return null; // most common case

	try {
		ensureOpen();
		ClassFileReader reader = ClassFileReader.read(this.zipFile, qualifiedBinaryFileName);
		if (reader != null) return new NameEnvironmentAnswer(reader, null /*no access restriction*/);
	} catch (Exception e) {
		// treat as if class file is missing
	}
	return null;
}

private void ensureOpen() throws IOException {
	if (zipFile != null) return; // If its not null, the zip is already open
	if (openArchives.size()>=maxOpenArchives) {
		closeSomeArchives(openArchives.size()/10); // Close 10% of those open
	}
	zipFile = new ZipFile(f);
	openArchives.add(this);
}

private void closeSomeArchives(int n) {
	for (int i=n-1;i>=0;i--) {
		ClasspathJar zf = (ClasspathJar)openArchives.get(0);
		zf.close();
	}
}

public void close() {
	if (zipFile == null) return;
	try {
		openArchives.remove(this);
		zipFile.close();
	} catch (IOException ioe) {
		ioe.printStackTrace();
	} finally {
		zipFile = null;
	}
}

public boolean isPackage(String qualifiedPackageName) {
	if (this.packageCache != null)
		return this.packageCache.containsKey(qualifiedPackageName);

	this.packageCache = new Hashtable(41);
	this.packageCache.put("", ""); //$NON-NLS-1$ //$NON-NLS-2$
	try {
        ensureOpen();
	} catch (IOException ioe) {
		// Doesn't normally occur - probably means since starting the compile 
		// you have removed one of the jars.
		ioe.printStackTrace();
		return false;
	}
	nextEntry : for (Enumeration e = this.zipFile.entries(); e.hasMoreElements(); ) {
		String fileName = ((ZipEntry) e.nextElement()).getName();

		// add the package name & all of its parent packages
		int last = fileName.lastIndexOf('/');
		while (last > 0) {
			// extract the package name
			String packageName = fileName.substring(0, last);
			if (this.packageCache.containsKey(packageName))
				continue nextEntry;
			this.packageCache.put(packageName, packageName);
			last = packageName.lastIndexOf('/');
		}
	}
	return this.packageCache.containsKey(qualifiedPackageName);
}
public void reset() {
	if (this.zipFile != null && this.closeZipFileAtEnd) {
		close();
		}
	this.packageCache = null;
}
public String toString() {
	return "Classpath for jar file " + this.zipFile.getName(); //$NON-NLS-1$
}

// Copes with the security manager
private static String getSystemPropertyWithoutSecurityException (String aPropertyName, String aDefaultValue) {
	try {
		return System.getProperty(aPropertyName, aDefaultValue);
	} catch (SecurityException ex) {
		return aDefaultValue;
}
}
}
