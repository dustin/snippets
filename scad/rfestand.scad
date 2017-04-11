include <line2d.scad>;
include <polyline2d.scad>;

$fa=2 * 1;
$fs=.02 * 1;

angle=20;
height=100;
depth=60;
holder_len=65;
holder_depth=21;

module dupmirror() {
    children();
    mirror() children();
}

module rotate_at(angles, pos=[0, 0]) {
    translate(pos) rotate(angles) translate([-pos[0], -pos[1], -pos[2]])
        children();
}

module athing() {
    difference() {
        linear_extrude(10)
            union() {
            translate([2, 2])
                polyline2d(points = [
                               [depth, 0],
                               [0, 0], [0, height-4], [12, height-4]], width=5,
                    startingStyle="CAP_ROUND", endingStyle="CAP_ROUND");

            translate([15/2, 15/2]) circle(d=15);
            translate([depth-5+15/2, 15/2-.5]) circle(d=15);
            translate([1+15/2, height-7]) circle(d=15);
            rotate_at([0, 0, angle], [14, height-2])
                polyline2d(points=[[14, height-2], [14, height-holder_len], [14+holder_depth, height-holder_len]], width=5,
                    startingStyle="CAP_ROUND", endingStyle="CAP_ROUND");
        }
        translate([15/2, 15/2, 7]) cylinder(d=9, h=8, center=true);
        translate([depth-5 + 15/2, 15/2, 7]) cylinder(d=9, h=8, center=true);
        translate([1 + 15/2, height-14 + 15/2, 7]) cylinder(d=9, h=8, center=true);
    }
}

dupmirror() translate([5, 0])
athing();
