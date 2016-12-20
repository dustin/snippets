$fn=90;

csize = 100;
nholes = 50;
rseed = 187;

holespos = [for (i=[0:1:nholes]) rands(min_value=-csize/2, max_value=csize/2, value_count=3, seed=rseed+i)];
holesizes = rands(min_value=10, max_value=40, value_count=nholes, seed=rseed);

difference() {
    cube(csize, center=true);
    for(i = [0:nholes]) {
        translate(holespos[i]) sphere(d=holesizes[i], center=true);
    }
}
