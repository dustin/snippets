##############################################################
#
# Copyright (c) 1997  Dustin Sallings
#
# $Id: billib.pl,v 1.1 1997/10/23 08:06:46 dustin Exp $
#
# Billing library routines
#
##############################################################

# Simple querier

sub doQuery
{
    my($dbh, $query)=@_;
    my($s);

    if(!($s=$dbh->execute($query)))
    {
	die("ERROR:  $Postgres::error\n$query");
    }

    return($s);
}

# Return a hash containing useful info for billing based on invoice
# number

sub getInvInfo
{
    my($dbh, $invoice)=@_;
    my(%h, $s, @r, $query, $tmp);

    %h=();

    $query="select * from invoices where invoice=$invoice";
    $s=doQuery($dbh, $query);
    ($h{'invoice'}, $h{'spam_id'}, $h{'billstart'}, $h{'billend'},
	$h{'comments'}, $h{'inv_ts'})=$s->fetchrow();

    ($tmp)=split(/\s/, $h{'inv_ts'});
    @r=split(/-/, $tmp);
    $h{'billdate'}="$r[1]-$r[2]-$r[0]";
    
    $query="select * from bill_info where spam_id=$h{'spam_id'}";
    $s=doQuery($dbh, $query);
    ($tmp, $h{'name'}, $h{'addr1'}, $h{'addr2'}, $h{'city'}, $h{'state'},
	$h{'zip'}, $h{'country'}, $h{'phone'}, $h{'fax'}, $h{'email'})=
	$s->fetchrow();

    $query ="select sum(amount) from trans\n";
    $query.="    where spam_id=$h{'spam_id'}\n";
    $query.="          and ts<='$h{'billend'} 23:59:59'\n";
    $query.="          and ts>='$h{'billstart'}'\n";
    $query.="          and amount>0;";
    $s=doQuery($dbh, $query);
    ($h{'debit'})=$s->fetchrow();
    $h{'debit'}=~s/[\$]//;
    $h{'debit'}="0.00" if($h{'debit'} eq "");

    $query ="select sum(amount) from trans\n";
    $query.="    where spam_id=$h{'spam_id'}\n";
    $query.="          and ts<='$h{'billend'} 23:59:59'\n";
    $query.="          and ts>='$h{'billstart'}'\n";
    $query.="          and amount<0;";
    $s=doQuery($dbh, $query);
    ($h{'credit'})=$s->fetchrow();
    $h{'credit'}=~s/[\$]//;
    $h{'credit'}="0.00" if($h{'credit'} eq "");

    $query ="select sum(amount) from trans\n";
    $query.="    where spam_id=$h{'spam_id'}\n";
    $query.="          and ts<'$h{'billstart'}'\n";
    $s=doQuery($dbh, $query);
    ($h{'forward'})=$s->fetchrow();
    $h{'forward'}=~s/[\$]//;
    $h{'forward'}="0.00" if($h{'forward'} eq "");

    $h{'total'}=$h{'forward'}+$h{'credit'}+$h{'debit'};

    $query ="select ts, descr, amount from trans\n";
    $query.="    where spam_id=$h{'spam_id'}\n";
    $query.="        and ts<='$h{'billend'} 23:59:59'\n";
    $query.="        and ts>='$h{'billstart'}';\n";
    $s=doQuery($dbh, $query);

    while(@r=$s->fetchrow())
    {
	push(@{$h{'trans'}}, [@r]);
    }

    return(%h);
}

1;
