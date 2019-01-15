$fa=2;
$fs=.05;

difference() {
    union() {
        cylinder(d=7.5, h=1);
        cylinder(d=4.5, h=2);
    }
    
    translate([0, 0, -1]) hull() {
        cylinder(d=4, h=1.5);
        translate([6, 0, 0]) cylinder(d=4, h=1.5);
    }
    
    translate([0, 0, -.1]) cylinder(d=1.5, h=4);
}