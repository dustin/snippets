/*
 * Dustin Sallings
 * 
 * This program is useless, but at least it works.  The user inputs a login
 * name and a password to test, and the program checks to see if the login
 * name and the password match up.  Pretty easy once you figure out how to
 * use crypt().
 */

#include <stdio.h>
#include <unistd.h>

#include <termio.h>
#include <sys/ioctl.h>

#include <pwd.h>

#define TOGGLE	2
#define	ON	1
#define	OFF	0

#define	TRUE	1
#define	FALSE	0

#ifdef	STUPID
#define	MSG_CORRECT	"I think you guessed correctly, it worked anyway."
#define	MSG_WRONG	"BZZZZZT, try again."
#else
#define	MSG_CORRECT	"The password is correct."
#define	MSG_WRONG	"The password is wrong."
#endif

#ifndef PWFILE
#define PWFILE "passwd"
#endif

struct termios tinfo;
int e;

void
techo(int i)
{
  if (i == TOGGLE)
    {
      if (e == ON)
	techo(OFF);
      else
	techo(ON);
    }

  if (i == ON)
    {
      e = ON;
      tinfo.c_lflag &= ECHO;
    }

  if (i == OFF)
    {
      e = OFF;
      tinfo.c_lflag &= ~ECHO;
    }

  if (tcsetattr(0, TCSANOW, &tinfo))
    {
      perror("tcsetattr:  ");
      exit(1);
    }
}

void
main(void)
{
  struct passwd *p;
  char pass[10];
  char login[20];
  int found = FALSE;
  FILE *infile;

  if (tcgetattr(0, &tinfo))
    {
      perror("tcgetattr: ");
      exit(1);
    }

  puts("Input the login name of the user you want to look at.");
  fgets(login, 19, stdin);
  login[strlen(login) - 1] = 0x00;

#ifdef	DEBUG
  printf("Searching for %s...\n", login);
#endif

  if ((infile = fopen(PWFILE, "r")) == NULL)
    {
      perror(PWFILE);
      exit(1);
    }

/*
 * There is a library command, getpwnam(), that should do this, but I
 * couldn't get it to work, so I'll do it myself by looking at the name of
 * every one of them and comparing it to the name that was input.
 */
  while ((p = fgetpwent(infile)))
    {
      if (strcmp(p->pw_name, login) == 0)
	{
	  found = TRUE;
	  break;
	}
    }

  if (!found)
    {
      fprintf(stderr, "Sorry, couldn't find user %s.\n", login);
      exit(1);
    }

#ifdef DEBUG
  puts("Completed search.");
  printf("User: %s\tPass: %s\n", p->pw_name, p->pw_passwd);
#endif

  puts("Input your guess at his password.");
  techo(OFF);			/*
				 * Turn off echo so no one can see what we guess 
				 */
  fgets(pass, 9, stdin);
  pass[strlen(pass) - 1] = 0x00;
  techo(ON);			/*
				 * Turn it back on. 
				 */

#ifdef	DEBUG
  printf("Testing \"%s\" encrypted with \"%s\"...\n", p->pw_name, pass);
  puts(crypt(pass, p->pw_passwd));
#endif

/*
 * Here we see if the person input the correct password.  I really don't
 * understand how this works because I have no documentation on what crypt()
 * actually does, all I know is that this works.
 */

  if (strcmp(p->pw_passwd, crypt(pass, p->pw_passwd)) == 0)
    puts(MSG_CORRECT);
  else
    puts(MSG_WRONG);
}
