$fn=30;

module countersunk(h) {
    translate([-0.01, 0, 0])
    rotate([0, 90, 0])
        cylinder(d1=2, d2=4, h=h+.1);
}

module support(t) {
    difference() {
        cube([t, 2, t]);
        rotate([0, 45, 0])
            translate([0, -.1, 0])
                cube([t*2, 2.2, t*2]);
    }
}

module holder(d, h, t, hh=25) {
    translate([d/2+2*PI+h/2, 0, 0])
        difference() {
            hull() {
                cylinder(d=d+t, h=h, center=true);
                translate([-(d/2)-2*PI, 0, 0]) cube([h, d+t, h], center=true);
            }
            cylinder(d1=d, d2=d+.5, h=h+1, center=true);
        }
 
    translate([0, -(d+t)/2, -hh])
            difference() {
                cube([h, d+t, hh]);
                translate([0, (d+t)*(1/4), hh-7]) countersunk(h);
                translate([0, (d+3)*(3/4)+2, hh-7]) countersunk(h);

            }

    translate([2, -((d+t)/2), -9]) support(8);
    translate([2, ((d+t)/2)-2, -9]) support(8);
}

holder(19, 2, 6, 25);
