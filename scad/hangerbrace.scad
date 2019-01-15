$fa=2;
$fs=.05;


inch = 25.4;
thickness = 1.5 * inch;
topwidth = 75;
mainwidth = 50;

module band(d, o) {
    translate([o, 0, 25]) {
        rotate([0, 90, 0]) {
            rotate_extrude(convexity = 10)
            translate([d, 0, 0])
            circle(r = .125 * inch, $fn = 100);
        }
    }
}


difference() {
    union() {
        hull() {
            cylinder(d=thickness, h=mainwidth);
            translate([2 * inch, 0, 25]) {
                sphere(d=(1.5*inch));
            }
        }

        translate([0, 0, -mainwidth/4]) {
            difference() {
                cylinder(d=thickness, h=topwidth);
                translate([0, 0, -.1]) cylinder(d=inch, h=topwidth+.2);
            }
        }
    }

    translate([0, 0, -.1]) cylinder(d=inch, h=mainwidth + .2);
    translate([0, 0, 25]) rotate([0, 90, 0]) cylinder(d=inch, h=3 * inch);
    translate([inch, 0, 25]) rotate([0, 90, 0]) cylinder(d=1.25 * inch, h=.5 * inch);

    band(.9*inch, 1.5*inch);
    band(inch, inch);
    translate([-40, 0, -40]) cube([400, 400, 400]);
}
