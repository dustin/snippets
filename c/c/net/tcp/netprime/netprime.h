#ifndef NETPRIME_H
#define NETPRIME_H

#define PRIME_PORT 1052
#define BUFLEN 1024
#define NGEN 60

struct listthing {
    char *name;
    void (*func)(int s, char *arg);
};

int bit_set(int map, int bit);
int f_exists(char *file);
int getservsocket(int port);
int gettext(int s, char *buf);
int gettextcr(int s, char *buf);
char *kw(char *in);
int set_bit(int map, int bit);
void quit(int s, char *arg);
void generate(int s, char *arg);

#endif
