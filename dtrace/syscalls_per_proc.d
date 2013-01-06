syscall:::entry
{
	@[execname] = count();
}

tick-10s
{
    printa(@);
    clear(@);
}
