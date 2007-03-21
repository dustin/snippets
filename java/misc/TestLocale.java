import java.util.*;
import java.text.*;

public class TestLocale extends Object {
	public static void main(String args[]) {
		Locale l=null;
		
		if(args.length==3) {
			l=new Locale(args[0], args[1], args[2]);
		} else {
			l=new Locale(args[0], args[1]);
		}

		NumberFormat f1=NumberFormat.getInstance(l);
		NumberFormat f2=NumberFormat.getCurrencyInstance(l);
		NumberFormat f3=NumberFormat.getPercentInstance(l);

		System.out.println("Using locale:  " + l.getDisplayName()
			+ " -- (" + l + ")");

		for(int i=0; i<5; i++) {
			double d=Math.random()*1000000;

			System.out.println("Number:  " + d);

			System.out.println("Formatted:  " + f1.format(d));
			System.out.println("$Formatted:  " + f2.format(d));
			System.out.println("%Formatted:  " + f3.format(d));
		}
	}
}
