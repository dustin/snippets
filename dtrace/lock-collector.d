plockstat*:::mutex-block
/pid == $1/
{
        @collector[ustack()] = count();
}
