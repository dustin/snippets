// Copyright (c) 2001  Beyond.com <dustin@beyond.com>
//
// $Id: Digest.java,v 1.2 2002/07/10 04:26:30 dustin Exp $

package net.spy.util;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;

/**
 * Digest for getting checksums, hashing passwords, stuff like that.
 */
public class Digest extends Object {

	/**
	 * Get a new Digest object.
	 */
	public Digest() {
		super();
	}

	// SSHA = 20 bytes of SHA data + a random salt.  Perl for checking can
	// be found here:
	// http://developer.netscape.com/docs/technote/ldap/pass_sha.html

	/**
	 * Check a plaintext password against a hashed password.
	 */
	public boolean checkPassword(String pw, String hash) {
		boolean rv=false;

		try {
			String hashtype=hash.substring(0,
				hash.indexOf('}')+1).toUpperCase();
			String data=hash.substring(hash.indexOf('}')+1);

			Base64 base64d=new Base64();
			byte datab[]=base64d.decode(data);
			byte salt[]=new byte[datab.length-20];
			System.arraycopy(datab, 20, salt, 0, salt.length);
			String newhash=getHash(pw, salt);

			rv=hash.equals(newhash);
		} catch(Exception e) {
			// Didn't decode or something, don't verify it.
			rv=false;
		}

		return(rv);
	}

	private String bytesToHex(byte in[]) {
		StringBuffer sb=new StringBuffer();

		for(int i=0; i<in.length; i++) {
			sb.append(Integer.toHexString((int)in[i] & 0xff));
		}

		return(sb.toString());
	}

	/**
	 * Get a hash for a String with a known salt.  This should only be used
	 * for verification, don't be stupid and start handing out words with
	 * static salts.
	 */
	protected String getHash(String word, byte salt[]) {
		MessageDigest md=null;
		try {
			md=MessageDigest.getInstance("SHA");
		} catch(NoSuchAlgorithmException e) {
			throw new Error("There's no SHA?");
		}
		md.update(word.getBytes());
		md.update(salt);
		Base64 base64=new Base64();
		byte pwhash[]=md.digest();
		String hout = "{SSHA}" + base64.encode(cat(pwhash, salt));
		return(hout.trim());
	}

	/**
	 * Get a hash for a String with no salt.  This should only be used for
	 * checksumming, not passwords.
	 */
	public String getSaltFreeHash(String word) {
		MessageDigest md=null;
		try {
			md=MessageDigest.getInstance("SHA");
		} catch(NoSuchAlgorithmException e) {
			throw new Error("There's no SHA?");
		}
		md.update(word.getBytes());
		Base64 base64=new Base64();
		byte pwhash[]=md.digest();
		String hout = "{SHA}" + base64.encode(pwhash);
		return(hout.trim());
	}

	/**
	 * Get a hash for a given String.
	 */
	public String getHash(String word) {
		// 8 bytes of salt should be enough sodium for anyone
		byte salt[]=new byte[8];
		SecureRandom sr=new SecureRandom();
		sr.nextBytes(salt);
		// System.out.println("Salt is " + bytesToHex(salt));

		return(getHash(word, salt));

	}

	private byte[] cat(byte a[], byte b[]) {
		byte r[]= new byte [a.length + b.length];
		System.arraycopy (a, 0, r, 0, a.length);
		System.arraycopy (b, 0, r, a.length, b.length);
		return(r);
	}

	/**
	 * Commandline testing.
	 *
	 * <p>
	 *
	 * Making a password:
	 *
	 * <ul>
	 *  net.spy.util.Digest word
	 * </ul>
	 *
	 * <p>
	 *
	 * Checking a password:
	 *
	 * <ul>
	 *  net.spy.util.Digest word hash
	 * </ul>
	 *
	 */
	public static void main(String args[]) throws Exception {
		Digest d=new Digest();
		if(args.length==1) {
			System.out.println(args[0] + ": " + d.getHash(args[0]));
		} else if(args.length==2) {
			if(d.checkPassword(args[0], args[1])) {
				System.out.println("Correct.");
			} else {
				System.out.println("Incorrect.");
			}
		} else {
			System.out.println("RTFS");
		}
	}

}
