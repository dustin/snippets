class CountWheat
{
   public static void main(String args[]) {
	double i, j, k;
	j=1; k=0;

	for(i=1; i<=64; i++) {
		k+=j;
		System.out.println("On number " + i + ":  " + k);
		j*=2;
	}
   }
}
