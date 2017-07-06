$fa=2;
$fs=.05;

bottom = 44;
top = 35;
depth = 10;
hole = 6.4;
pad_thickness = 3.5;

module quick_mount() {
    difference() {
        hull() {
            cube([bottom, bottom, 1]);
            offset = (bottom - top) / 2;
            translate([offset, offset, depth]) cube([top, top, .1]);
        }
        bot2 = bottom - 4;
        top2 = top - 2;
        hull() {
            translate([2, 2, -.1]) cube([bot2, bot2, 1]);
            offset = (bot2 - top2) / 2;
            translate([offset+2, offset+2, depth-2]) cube([top2, top2, .1]);
        }
   }
}

difference() {
    union() {
        quick_mount();
        translate([bottom/2, bottom/2, depth-pad_thickness+.2])
            cylinder(d=17, d2=23, h=pad_thickness-2);
    }
    translate([bottom/2, bottom/2, -.1]) {
        cylinder(d=hole, h=depth+2);
    }
}