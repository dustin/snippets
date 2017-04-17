top_width = 49;
top_thickness = 1.5;
thickness = 1.5;
width = 4;
edge = 2;

module clip() {
    cube([top_width + edge, width, thickness]);
    translate([0, 0, thickness]) {
        difference() {
            cube([4, width, top_thickness*2]);
            translate([edge, -.1, 0]) cube([5, width + .2, thickness]);
        }
    }
    translate([top_width + edge, 0, 0]) cube([edge, width, width]);
}

module hole() {
    rotate([90, 90, 0])
    difference() {
        rotate_extrude() translate([4, 0]) circle(d=width, $fa=2, $fs=0.02);
        rotate([0, 40, 0]) translate([-1, 0, -width]) cube([2, 6, width*2]);
    }
}

clip();
translate([top_width + edge + 6, width/2, 5.1]) hole();