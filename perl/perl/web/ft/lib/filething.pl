# Filething;

%userMap=();
$fileRoot="/home/dustins/lib/incoming";

sub ensurePath
{
    local($fn)=@_;
    local(@a, $np);

    if(-d $fn)
    {
        return();
    }

    @a=split(/\//, $fn);

    pop(@a);
    $np=join('/', @a);
    &ensurePath($np);

    if(! -d $np )
    {
        mkdir($np, 0755);
    }
}

sub newId
{
    local($to)=@_;
    local($f, $id);

    $id=10000;

    $f="$fileRoot/$to/$id";

    &ensurePath($f);

    while(-d $f)
    {
	$id++;
	$f="$fileRoot/$to/$id";
    }

    return($id);
}

open(IN, "/home/dustins/lib/filethinglist");
while(<IN>)
{
    chop;
    @a=split(/:/);

    $userMap{$a[0]}=$a[1];
}
close(IN);

1;
