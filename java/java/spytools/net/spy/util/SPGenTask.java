/*
 * Copyright (c) 2002 Scott Lamb <slamb@slamb.org>
 * This code is released under the MIT license; see the file LICENSE.
 *
 * $Id: SPGenTask.java,v 1.1 2002/05/23 04:13:33 dustin Exp $
 */

package net.spy.util;

import java.io.*;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.taskdefs.MatchingTask;
import net.spy.util.SPGen;

/**
 * Generates Java code from SPT files.
 *
 * This is an Ant task that recurses a directory and invokes
 * net.spy.util.SPGen on .spt files to generate Java code. It does the usual
 * stuff expected of a Make-style tool: leaving up-to-date .java files intact,
 * creating output with a temporary file until it is finished so partial
 * builds don't create problems, etc.
 *
 * @author Scott Lamb
 * @version $Revision: 1.1 $ $Date: 2002/05/23 04:13:33 $
 **/
public class SPGenTask extends MatchingTask {

    private File srcDir;
    private File destDir;

    /**
     * Sets the source directory to search for .spt files.
     **/
    public void setSrcdir(File srcDir) {
	this.srcDir = srcDir;
    }

    /**
     * Sets the destination directory to place .java files.
     * If not specified, assumed to be the same as the source directory.
     **/
    public void setDestdir(File destDir) {
	this.destDir = destDir;
    }

    /**
     * Performs the operation.
     **/
    public void execute() throws BuildException {
	if (srcDir == null) {
	    throw new BuildException("srcdir attribute must be set!", location);
	}
	if (!srcDir.isDirectory()) {
	    throw new BuildException("source directory \"" + srcDir
		    + "\" is not valid.");
	}
	if (destDir != null && !destDir.isDirectory()) {
	    throw new BuildException("destination directory \"" + destDir +
		    "\" is not valid.", location);
	}

	DirectoryScanner ds = getDirectoryScanner(srcDir);
	String[] includes = {"**\\*.spt"};
	ds.setIncludes(includes);
	ds.setBasedir(srcDir);
	ds.scan();
	String[] files = ds.getIncludedFiles();

	if (files.length > 0) {
	    log("Compiling " + files.length + " spt file"
		    + (files.length == 1 ? "" : "s")
		    + (destDir != null ? " to " + destDir : ""));
	    for (int i = 0; i < files.length; i ++) {
		processFile(files[i]);
	    }
	}
    }

    /**
     * Processes an individual file.
     * @param filename The filename, relative to the source directory.
     **/
    protected void processFile(String filename) {
	File srcFile = new File(srcDir, filename);
	File tmpFile = null;
	File destFile = new File((destDir == null) ? srcDir
						   : destDir,
		 filename.substring(0, filename.length()-4)+".java");
	BufferedReader in;
	PrintWriter out;

	try {

	    // Open input file
	    try {
		FileReader reader = new FileReader(srcFile);
		in = new BufferedReader(reader);
	    } catch (IOException e) {
		throw new BuildException(e);
	    }

	    if (destFile.exists()
		    && destFile.lastModified() > srcFile.lastModified()) {
		return;
	    }

	    // Open output file
	    // Temporary so partial builds don't impact future ones
	    try {
		tmpFile = File.createTempFile("sptgen", "java",
					      destDir == null ? srcDir
							      : destDir);
		FileWriter writer = new FileWriter(tmpFile);
		out = new PrintWriter(writer);
	    } catch (IOException e) {
		throw new BuildException(e);
	    }

	    String name = srcFile.getName();
	    // assert name.endsWith(".spt");
	    SPGen spg = new SPGen(name.substring(0, name.length()-4), in, out);
	    try {
		spg.generate();
		in.close();
		out.close();
	    } catch (Exception e) {
		throw new BuildException(e);
	    }

	    if (destFile.exists()) {
		if (!destFile.delete()) {
		    throw new BuildException("Unable to delete "
			    + destFile);
		}
	    } else {
		checkParent(destFile);
	    }
	    if (!tmpFile.renameTo(destFile)) {
		throw new BuildException("Unable to rename "
			+ tmpFile + " to " + destFile);
	    }
	    tmpFile = null;
	} finally {
	    if (tmpFile != null) {
		tmpFile.delete();
	    }
	}
    }

    /**
     * Checks if a parent directory exists and, if not, creates it.
     * Stops at the destination directory level.
     **/
    protected void checkParent(File f) throws BuildException {
	File parent = f.getParentFile();
	if (parent.equals(destDir == null ? srcDir
					  : destDir) && !parent.exists()) {
	    throw new BuildException("Destination dir no longer exists");
	}
	if (!parent.exists()) {
	    checkParent(parent);
	    if (!parent.mkdir()) {
		throw new BuildException("Unable to create directory "
			+ parent);
	    }
	} else if (!parent.isDirectory()) {
	    throw new BuildException("Not a directory: " + parent);
	}
    }
}
