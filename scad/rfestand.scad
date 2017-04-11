include <line2d.scad>;
include <polyline2d.scad>;

$fa=2;
$fs=.02;

module dupmirror() {
    children();
    mirror() children();
}

module athing() {
    difference() {
        linear_extrude(10)
            union() {
            translate([2, 2])
                polyline2d(points = [
                               [65, 0],
                               [0, 0], [0, 96], [12, 96], [36, 30],
                               [65, 41]], width = 5);

            square([15, 15]);
            translate([55, 0]) square([15, 15]);
            polygon([[0, 85], [0, 98], [15, 98], [20, 85]]);
        }
        translate([15/2, 15/2, 7]) cylinder(d=9, h=8, center=true);
        translate([55 + 15/2, 15/2, 7]) cylinder(d=9, h=8, center=true);
        translate([1 + 15/2, 86 + 15/2, 7]) cylinder(d=9, h=8, center=true);
    }
}

dupmirror() translate([20, 0])
athing();
