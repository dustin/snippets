$fn=60;

module wheel(d) {
    minkowski() {
        hull() {
            cylinder(d=d, h=.1, center=true);
            cylinder(d=d*.7, h=5, center=true);
            //sphere(d=d/3, center=true);
        }
        sphere(d=d/11);
    }
}

module halfmoon(d) {
    rotate([0, -90, 0]) {
        difference() {
            wheel(d);
            translate([d/3.75, d/3.75, 0]) cylinder(d=d-5, h=15, center=true);
        }
    }
}

module hollow(d) {
    translate([0, 0, (d/2)+(d/22)]) {
        difference() {
            halfmoon(d);
            scale(0.9) halfmoon(d);
        }
    }
}

translate([0, 0, 6]) hollow(30);
minkowski() {
    translate([0, 0, 4]) rotate([0, 0, 90]) cylinder(d1=11, d2=14, h=7, center=true, $fn=5);
    sphere(d=1);
}
