// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: TestNNTPClient.java,v 1.1 2001/08/09 09:15:09 dustin Exp $

import java.io.*;
import net.spy.util.*;
import com.bigfoot.bugar.nntp.*;

/**
 * Get and decode jpg's from a newsgroup.
 */
public class TestNNTPClient extends Object {

	public static final int MAX_ARTICLES=100;

	/**
	 * Get an instance of TestClient.
	 */
	public TestNNTPClient() {
		super();
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		NNTPConnection nntp=new NNTPConnection(
			new NNTPClientAdapter(), "juan");

		Newsgroup group=nntp.getNewsgroup("alt.binaries.pictures.erotica");
		System.out.println(group);
		int first=group.getFirst();
		int last=group.getLast();
		int count=group.getArticleCount();

		if(args.length>0) {
			first+=Integer.parseInt(args[0]);
			System.out.println("Offsetting by " + args[0]);
		}

		System.out.println("Articles:  " + count
			+ " from " + first + " to " + last);

		for(int i=first; ((i-first) < MAX_ARTICLES) && i<=last; i++) {
			Article a=nntp.getArticle(group.toString(), i);
			System.out.println(a);
			try {
				ByteArrayInputStream bais=
					new ByteArrayInputStream(a.getBody().getBytes());
				UUInputStream uu=new UUInputStream(bais);
				String name=uu.getFilename().toLowerCase();

				if(name.endsWith(".jpg")) {
					StringBuffer sb=new StringBuffer();
					char chars[]=name.toCharArray();
					for(int j=0; j<chars.length; j++) {
						if(chars[j]=='.'
							|| Character.isLetterOrDigit(chars[j])) {

							sb.append(chars[j]);
						}
					}

					String filename=sb.toString();
					if(filename.length() < 5) {
						filename=i + ".jpg";
					}
					FileOutputStream fos=new FileOutputStream(filename);
					System.out.println("Writing " + filename);
					byte buffer[]=new byte[8192];
					int bytesread=uu.read(buffer);
					while(bytesread>0) {
						fos.write(buffer, 0, bytesread);
						bytesread=uu.read(buffer);
					}
					fos.close();
				}

				uu.close();
			} catch(IOException e) {
				e.printStackTrace();
			}
		}
	}

}

