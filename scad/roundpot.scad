$fn = 60;

module halfcone(d1, d2, h) {
    difference() {
        union() {
            hull() {
                cylinder(d1=d1, d2=d2, h=h, center=true);
                translate([0, 0, -(h/2)+1]) sphere(d=d1, center=true);
            }
        }
        translate([-h, 0, 0]) cube([2*h, 2*h, 3*h], center=true);
    }
}

difference() {
    halfcone(10, 50, 50);
    translate([2, 0, 10]) halfcone(10, 50, 50);
}
