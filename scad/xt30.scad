$fn=40;

module interior() {
    hull() {
        translate([5, 0, 0]) cylinder(d=3.7, h=6.5, center=true);
        cube([3.7, 3.7, 6.5], center=true);
    }
}

difference() {
    resize([3.7+6, 5, 6.5+.4]) interior();
    translate([.25, 0, -.25]) interior();
}

translate([0, 0, 3]) {
    hull() {
        cube([3.7+1, 3.7+2, 1], center=true);
        translate([5, 0, 0]) cylinder(d=3.7+2, h=1, center=true);
    }
}
