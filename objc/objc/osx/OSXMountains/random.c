/*
 *	C version of Marsaglia's UNI random number generator
 *	More or less transliterated from the Fortran -- with 1 bug fix
 *	Hence horrible style
 *
 *	Features:
 *		ANSI C
 *		not callable from Fortran (yet)
 */



char uni_id[] = "$Id: random.c,v 1.1 2003/04/08 23:17:26 dustin Exp $" ;
/*
 *	Global variables for rstart & uni
 */

#include <time.h>
#include <stdlib.h>
#define PARANOID
/* #include <sys/types.h>
 * #include <sys/time.h>
 */
#include <stdio.h>
#include <math.h>
typedef struct
{
  float u[98];
  float c;
  float cd;
  float cm;
  int ui;
  int uj;
}
Uni_save;

Uni_save uni_data;

float uni()
{
	float luni;			/* local variable for uni */

	luni = uni_data.u[uni_data.ui] - uni_data.u[uni_data.uj];
	if (luni < 0.0)
		luni += 1.0;
	uni_data.u[uni_data.ui] = luni;
	if (--uni_data.ui == 0)
		uni_data.ui = 97;
	if (--uni_data.uj == 0)
		uni_data.uj = 97;
	if ((uni_data.c -= uni_data.cd) < 0.0)
		uni_data.c += uni_data.cm;
	if ((luni -= uni_data.c) < 0.0)
		luni += 1.0;
	return ((float) luni);
}

void rstart(i,j,k,l)
int i;
int j;
int k;
int l;
{
	int ii, jj, m;
	float s, t;

	for (ii = 1; ii <= 97; ii++) {
		s = 0.0;
		t = 0.5;
		for (jj = 1; jj <= 24; jj++) {
			m = ((i*j % 179) * k) % 179;
			i = j;
			j = k;
			k = m;
			l = (53*l+1) % 169;
			if (l*m % 64 >= 32)
				s += t;
			t *= 0.5;
		}
		uni_data.u[ii] = s;
	}
	uni_data.c  = 362436.0   / 16777216.0;
	uni_data.cd = 7654321.0  / 16777216.0;
	uni_data.cm = 16777213.0 / 16777216.0;
	uni_data.ui = 97; /*  There is a bug in the original Fortran version */
	uni_data.uj = 33; /*  of UNI -- i and j should be SAVEd in UNI()     */
}


/* ~seed_uni: this takes a single integer in the range
 *		0 <= ijkl <= 900 000 000
 *	and produces the four smaller integers needed for rstart. It is
 *	based on the ideas contained in the RMARIN subroutine in
 *		F. James, "A Review of Pseudorandom Number Generators",
 *			Comp. Phys. Commun. Oct 1990, p.340
 *	To reduce the modifications to the existing code, seed_uni now
 *	takes the role of a preprocessor for rstart.
 *
 */

void seed_uni(ijkl)
int ijkl;
{
	int i, j, k, l, ij, kl;

	if( ijkl == 0 )
	{
	  ijkl = time((time_t *) 0);
	  ijkl %= 900000000;
	}
	/* check ijkl is within range */
	if( (ijkl < 0) || (ijkl > 900000000) )
		{
		fprintf(stderr,"seed_uni: ijkl = %d -- out of range\n\n", ijkl);
		exit(3);
		}


	/* decompose the long integer into the the equivalent four
	 * integers for rstart. This should be a 1-1 mapping
	 *	ijkl <--> (i, j, k, l)
	 * though not quite all of the possible sets of (i, j, k, l)
	 * can be produced.
	 */

	ij = ijkl/30082;
	kl = ijkl - (30082 * ij);

	i = ((ij/177) % 177) + 2;
	j = (ij % 177) + 2;
	k = ((kl/169) % 178) + 1;
	l = kl % 169;

#ifdef PARANOID
	if( (i <= 0) || (i > 178) )
		{
		fprintf(stderr,"seed_uni: i = %d -- out of range\n\n", i);
		exit(3);
		}

	if( (j <= 0) || (j > 178) )
		{
		fprintf(stderr,"seed_uni: j = %d -- out of range\n\n", j);
		exit(3);
		}

	if( (k <= 0) || (k > 178) )
		{
		fprintf(stderr,"seed_uni: k = %d -- out of range\n\n", k);
		exit(3);
		}

	if( (l < 0) || (l > 168) )
		{
		fprintf(stderr,"seed_uni: l = %d -- out of range\n\n", l);
		exit(3);
		}

	if (i == 1 && j == 1 && k == 1)
		{
                fprintf(stderr,"seed_uni: 1 1 1 not allowed for 1st 3 seeds\n\n");
		exit(4);
                }
#endif


        rstart(i, j, k, l);

}

float gaussian()
{
        double pi = 3.1415926536, two = 2.0, zero = 0.0;
	double ran1, ran2;

	do {
		ran1 = (double) uni();
	} while (ran1 == zero);

	ran2 = (double) uni();
	return (float) ( sqrt(-two * log(ran1)) * cos(two * pi * ran2) );
}

