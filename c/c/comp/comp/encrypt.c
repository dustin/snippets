#include <iostream.h>
#include <string.h>
#include <ctype.h>
#include <fstream.h>

#define decrypt(a, b) ( (char) ((((b-'A')+(a-'A'))%26) + 'A'))

void process(char *in, char *key)
{
int i=0, j=0, k=0;

	for(i=0; i<strlen(in); i++)
	{
		if(isupper(in[i]))
			cout << decrypt(in[i], key[j++]);
		else
			if(isalpha(in[i]))
			{
				cout << in[i];
			}
			else
				k--;

		if(++k == 5) {	cout << ' '; k=0; }

		if(j==strlen(key)) j=0;
	}
	cout << "\n\n";
}

void main(void)
{
char key[11], in[81];
ifstream fin("encrypt.in", ios::in);

	while(1)
	{
		fin.getline(key, 10);
		fin.getline(in, 80);
		if(fin.eof())
			break;
		cout << in << endl;
		process(in, key);
	}
}
