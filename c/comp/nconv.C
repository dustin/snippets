#include <stdio.h>
#include <iostream.h>

class numba
{
	char strbuf[80];
public:
	float number;
	char *dispdec();
	numba();
};

numba::numba()
{
	number=0;
	strbuf[0]=0;
}

char *numba::dispdec()
{
	sprintf(strbuf, "%f", number);
	return(strbuf);
}

void main(void)
{
numba n;
	cout << "Input a number\n";
	cin >> n.number;

	cout << n.number << " in decimal is " << n.dispdec() << endl;
}
