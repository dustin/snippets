$fn=40;

module xtbase(l, w, h) {
    hull() {
        translate([l, 0, 0]) cylinder(d=w, h=h, center=true);
        cube([w, w, h], center=true);
    }
}

module xt(l, w, h) {
    difference() {
        resize([l+w+1, 5, h+.4]) xtbase(l, w, h);
        translate([.25, 0, -.25]) xtbase(l, w, h);
    }

    translate([0, 0, 3]) {
        hull() {
            cube([w+1, w+2, 1], center=true);
            translate([l+.2, 0, 0]) cylinder(d=w+2, h=1, center=true);
        }
    }
}

module xt30() {
    rotate([0, 180, 0]) xt(5.5, 3.7, 7);
}

xt30();
