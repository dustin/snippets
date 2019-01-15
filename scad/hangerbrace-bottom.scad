$fa=2;
$fs=.05;

inch = 25.4;
thickness = 1.5 * inch;
platewidth = 4.25*inch;

module support(l) {
    translate([0, 0, .5*inch])
    rotate([0, -90, 0])
    translate([0, 0, -2])
    linear_extrude(4)
    polygon(points=[[0, 0], [0, l], [2.5*inch, 0]]);
}

module base() {
    translate([0, platewidth/2, 0])
        cylinder(d=thickness, h=3*inch);
    linear_extrude(.5*inch)
        polygon(points=[[0, platewidth/4],
                        [3*inch, 0],
                        [3*inch, platewidth],
                        [0, platewidth*.75]]);
    beams = 3;
    mina = -60;
    maxa = -120;
    adiff = mina - maxa;
    translate([0, platewidth/2, 0]) {
        for (i = [0:beams-1]) {
            rotate([0, 0, mina - (i*(adiff/(beams-1)))])
                support(3*inch);
        }
    }
}

module band(d, o) {
    translate([o, 0, 25]) {
        rotate([0, 90, 0]) {
            rotate_extrude(convexity = 10)
            translate([d, 0, 0])
            square([2, 5]);
        }
    }
}

difference() {
    base();

    // Space for vertical post
    translate([0, platewidth/2, -.1])
        cylinder(d=inch, h=3*inch+.2);
    
 
    // Space for bottom posts
    translate([0, platewidth/2 - ((3.14*inch)/2), 0])
        rotate([0, 90, 0])
            cylinder(d=.64*inch, h=4*inch);
    translate([0, platewidth/2 + ((3.14*inch)/2), 0])
        rotate([0, 90, 0])
            cylinder(d=.64*inch, h=4*inch);

    // Cut off half the vertical post.
    mirror([1, 0, 0])
        translate([0, 0, -5])
            cube([400, 400, 400]);

    // Bottom zip tie bits
    translate([14, 26, -.1])
        cube([3, 5, thickness+.2]);
    translate([14, platewidth-26-5, -.1])
        cube([3, 5, thickness+.2]);

    translate([2.7*inch, 26, -.1])
        cube([5, 3, thickness+.2]);
    translate([2.7*inch, platewidth-26-3, -.1])
        cube([5, 3, thickness+.2]);

    // Top zip tie space
    translate([-1*inch, platewidth/2, 2.75*inch])
        rotate([0, 90, 0]) band(0.7*inch, 0);
    
    // Space for the bottom bar
    translate([0, platewidth-.1, 0])
        rotate([90, 0, 0])
            cylinder(d=inch, h=platewidth+.2);

    // space for the bolt
    hull() {
        translate([.4*inch, platewidth/2, 2.25 * inch])
            rotate([0, 90, 0])
                cylinder(d=.5*inch, h=3);
        translate([.4*inch, platewidth/2, inch])
            rotate([0, 90, 0])
                cylinder(d=.5*inch, h=3);

        }
}                
