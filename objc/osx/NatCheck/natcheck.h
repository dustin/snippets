#ifndef NATCHECK_H
#define NATCHECK_H 1

enum nat_types { UNKNOWN, BASIC, PNAT };

struct check_result {
    int nat_type;
    int consistent;
    int unsolicitedFilt;
};

struct check_result performNatCheck(int ntries);
void perrordie(const char *msg);

#endif NATCHECK_H
