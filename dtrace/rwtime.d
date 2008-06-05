syscall::read:entry,
syscall::write:entry,
syscall::writev:entry
/pid == $1/
{
	ts[probefunc] = timestamp;
	printf(".");
}

syscall::read:return,
syscall::write:return,
syscall::writev:return
/pid == $1 && ts[probefunc] != 0/
{
	printf("%d nsecs", timestamp - ts[probefunc]);
}
