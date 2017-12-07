$fa=2;
$fs=.05;

hole = 8;
// 10mm
distance = 26;
width = 25;
height = 43;

module handlehook() {
    rotate([-10, 0, 0])
    cylinder(d=hole, d2=hole-1, h=9);
}

module baseplate() {
    cube([width, height, 5]);
    translate([width/2, 0, 0]) rotate([-90, 0, 0]) cylinder(d=11, h=height);
    translate([(width/2), 5, -8-5]) {
        handlehook();
        translate([0, 26]) handlehook();
    }
}

module plate() {
    difference() {
        baseplate();
        translate([-.1, height/2 - 13/2, -.1]) cube([width+.2, 13, 3]);
    }
}

camthickness=2;
camwidth=28;
camlength=43;

module hook() {
    difference() {
        cube([4.5, 6, 4.5]);
        translate([2.25, -.05, -1]) cube([4.5, 6.1, 4.5]);
    }
}

module dupmirror() {
    children();
    translate([camwidth, 0, 0]) mirror() children();
}

module hooks() {
    hook();
    translate([0, 30, 0]) hook();
}

module mount() {
    cube([28, 43, camthickness]);
    dupmirror() {
        translate([2.5, 2.8, camthickness]) hooks();
    }
}

union() {
    translate([-1.5, 0, 5]) mount();
    plate();
}
