import java.net.*;
import java.io.*;
import java.util.*;

public class Fetch {
	URL url;

	public Fetch(String u) throws MalformedURLException {
		url=new URL(u);
	}

	public Vector getLines() throws Exception {
		Vector v = new Vector();

		try {
			URLConnection uc = url.openConnection();

			InputStream i = uc.getInputStream();
			BufferedReader br =
				new BufferedReader( new InputStreamReader(i));

			String line;
			while( (line=br.readLine()) != null) {
				v.addElement(line);
			}

		} catch(Exception e) {
			throw new Exception(e.getMessage());
		}
		return(v);
	}
}
