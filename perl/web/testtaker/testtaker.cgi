#!/usr/local/bin/perl

require 'cgi-lib.pl';

sub loadQuestions
{
    my(@a, @tmp, $n);

    open(FILE, "questions");
    while(<FILE>)
    {
        next if(/^#/);
        next if(!/[A-z0-9]/);
        chop;

        push(@a, $_);
    }
    close(FILE);

    $n=0;
    while($#a>0)
    {
        push(@questions, [ shift(@a), # question
                           shift(@a), # A
                           shift(@a), # B
                           shift(@a), # C
                           shift(@a), # D
                           shift(@a)] # Answer
                           );
	push(@tmp, $n++);
    }

    @qList=shuffle(@tmp);
}

sub shuffle
{
    my(@a)=@_;
    my(@ret, $r);

    while($#a>=0)
    {
	push(@ret, splice(@a, rand($#a), 1));
    }

    return(@ret);
}

sub displayQuestion
{
    my($which)=@_;
    my(@q, @vs);

    @vs=(0, 1, 2, 3, 4);
    @q=@{$questions[$qList[$which]]};

    print "<tr><td>$q[0]</td><td>\n";

    for(1..4)
    {
	print "<input type=\"radio\" name=\"question-$which\" ";
	print "value=\"$vs[$_]\"> $q[$_]<br>\n";
    }

    print "</td></tr>\n";
}

sub loadTest
{
    my($testname, $nquestions)=@_;

    print "<table border=\"3\">\n";
    print "<form method=\"post\" action=\"/cgi-bin/dustin/grade.cgi\">\n";

    for(0..$nquestions)
    {
        displayQuestion($_) if($_ <= $#qList);
    }

    print "</form></table>\n";
}

sub header
{
    print <<EOF;
Content-type: text/html

<html><head><title>Dustin's Test Server</title></head>
<body bgcolor="fFfFfF">

<h2>Dustin's Test Server</h2>
EOF
}

sub testList
{
    print <<EOF;
<form method="post" action="/cgi-bin/dustin/testtaker.cgi">
<select name="test">
<option value="CADT">California Drivers Test
</select>
<br>
Number of questions: <input name="nquestions" value="20" maxlenth="3"
size="3">
<br>
<input type="submit" value="Go">
</form>
EOF
}

&ReadParse(*in);

&loadQuestions;
&header;

if(defined($in{'test'}))
{
    &loadTest($in{'test'}, $in{'nquestions'});
}
else
{
    &testList;
}
