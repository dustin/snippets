syscall:::entry
/pid == $1/
{
	ts[probefunc] = timestamp;
}

syscall:::return
/pid == $1 && ts[probefunc] != 0/
{
	printf("%d nsecs", timestamp - ts[probefunc]);
	ustack();
}
