$fn=40;

iwidth=3.7;
ilen=5.5;
iheight=7;

module interior() {
    hull() {
        translate([ilen, 0, 0]) cylinder(d=iwidth, h=iheight, center=true);
        cube([iwidth, iwidth, iheight], center=true);
    }
}

module xt30() {
    difference() {
        resize([ilen+iwidth+1, 5, iheight+.4]) interior();
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
