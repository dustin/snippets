#include <sys/types.h>

#define PORT 5342

#define MAXCONS 100

#define MAXPASS 50

#define COM_STOP	0
#define COM_INIT	1
#define COM_GET		2
#define COM_STAT	3

struct dllist {
    int socket;
    time_t contime;
    int tries;
};

struct global {
    int numusers;
    int numtries;
    fd_set fdset;
    int listening;
    time_t starttime;
    char encrypted[15];
    char password[15];
    int passmap[8];
    struct dllist **acons;
};

struct retpack {
    int info;
    char pswds[MAXPASS][15];
};

struct init {
    char password[15];
};

struct command {
    int command;
    char passwd[15];
};

struct info {
    int cons;
    int tries;
    int starttime;
    int curtime;
    char curpass[9];
    char encrypted[15];
    struct dllist acons[MAXCONS];
};

void con_add(int new);
void con_del(int n);
void pipe_del(int n);
int initialize(void);
char *newpasswd();
void getsolution(struct command com);
void inituser(int s);
void sendpasswords(int s);
void sendstatus(int s);
