// Class definitions and stuph for doors.

class door{
char *buf;
char *backing;
int x;
int y;
int width;
int height;
public:
int opendoor(int x, int y, int width, int height, int fill);
int slamdoor(void);
int movedoor(int x, int y);
};
