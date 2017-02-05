$fn=40;

iwidth=3.7;
ilen=5.1;

module interior() {
    hull() {
        translate([ilen, 0, 0]) cylinder(d=iwidth, h=6.5, center=true);
        cube([iwidth, iwidth, 6.5], center=true);
    }
}

module xt30() {
    difference() {
        resize([ilen+iwidth+1, 5, 6.5+.4]) interior();
        translate([.25, 0, -.25]) interior();
    }

    translate([0, 0, 3]) {
        hull() {
            cube([iwidth+1, iwidth+2, 1], center=true);
            translate([ilen+.2, 0, 0]) cylinder(d=iwidth+2, h=1, center=true);
        }
    }
}

rotate([0, 180, 0]) xt30();
