use <diamond.scad>;

$fn=60;

difference() {
    union() {
        rotate([180, 0, 0]) dvase(25, 16, 17, 5);
        translate([0, 0, 3.5]) rotate([90, 0, 0])cylinder(d=20.5, h=4, center=true);
    }
    translate([0, 0, 3.5]) rotate([90, 0, 0]) cylinder(d=16.6, h=25, center=true);
}
