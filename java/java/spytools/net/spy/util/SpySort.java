// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: SpySort.java,v 1.4 2002/07/10 05:42:26 dustin Exp $

package net.spy.util;

/**
 * Sorting class.
 */

public class SpySort extends Object {

	private SpyComparable compare=null;

	/**
	 * Get an SpySort object with the given comparable.
	 */
	public SpySort(SpyComparable c) {
		super();
		this.compare=c;
	}

	private void quicksort(Object a[], int l, int r) {
		int i=0, j=0;
		Object v=null, t=null;

		if(r>l) {
			v=a[r];
			i=l-1;
			j=r;

			do {
				while( (compare.compare(a[++i], v) < 0) && i < r) {
					// Nothing
				}
				while( (compare.compare(a[--j], v) > 0) && j > 0) {
					// Nothing
				}

				t = a[i];
				a[i] = a[j];
				a[j] = t;
			} while(j>i);

			a[j] = a[i];
			a[i] = a[r];
			a[r] = t;

			quicksort(a, l, i-1);
			quicksort(a, i + 1, r);
		}
	}

	/**
	 * Sort the given list of objects with this object's comparable.
	 */
	public void sort(Object o[]) {
		quicksort(o, 0, o.length-1);
	}

	// Integer sort in main
	public static void main(String args[]) {
		SpyComparable c=new SpyIntegerComparable();
		Object o[]=new Object[args.length];

		for(int i=0; i<args.length; i++) {
			o[i]=new Integer(args[i]);
		}

		System.out.println("Sorting " + o.length + " items.");

		SpySort s=new SpySort(c);
		s.sort(o);

		for(int i=0; i<o.length; i++) {
			System.out.println("" + i + ":\t" + o[i]);
		}
	}
}

