import java.util.*;
import java.text.*;

public class TestLocale2 extends Object {
	public static void main(String args[]) {

		Locale ls[]=NumberFormat.getAvailableLocales();

		for(int i=0; i<ls.length; i++) {
			Locale l=ls[i];
			NumberFormat f1=NumberFormat.getInstance(l);
			NumberFormat f2=NumberFormat.getCurrencyInstance(l);
			NumberFormat f3=NumberFormat.getPercentInstance(l);

			System.out.println("Using locale:  " + l.getDisplayName()
				+ " -- (" + l + ")");

			double d=Math.random()*1000000;

			System.out.println("Number:  " + d);

			System.out.println("Formatted:  " + f1.format(d));
			System.out.println("$Formatted:  " + f2.format(d));
			System.out.println("%Formatted:  " + f3.format(d));
		}
	}
}
