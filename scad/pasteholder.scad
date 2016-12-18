$fn=30;

module holder(d, h, t) {
    translate([d/2+2*PI+h/2, 0, 0])
        difference() {
            hull() {
                cylinder(d=d+t, h=h, center=true);
                translate([-(d/2)-2*PI, 0, 0]) cube([h, d+t, h], center=true);
            }
            cylinder(d=d, h=h+1, center=true);
        }
 
    translate([0, -(d+t)/2, -25]) cube([h, d+t, 25]);
}

holder(19, 2, 6);
