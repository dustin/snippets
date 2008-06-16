syscall::execve:entry
{
	printf("%d:%d %s - %s", ppid, pid, execname, copyinstr(arg0));
}
