public class HelloWorld {
	public native void displayHelloWorld();

	static {
		System.loadLibrary("hello");
	}

	public static void main(String args[]) throws Exception {
		HelloWorld hw=new HelloWorld();

		hw.displayHelloWorld();
	}
}
