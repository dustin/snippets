$fa=2;
$fs=.05;

height = 20;

module dupmirror() {
    children();
    mirror([0, 1, 0]) children();
}

module mainbody() {
    difference() {
        union() {
            // base pate
            translate([0, 0, 0.5]) cube([28, 36, 1], center=true);
            // walls
            intersection() {
                union() {
                    dupmirror() translate([3, 36/2-3/2, height/2]) cube([31, 3, height], center=true);
                    // fillet
                    dupmirror() {
                        difference() {
                            translate([3, 13.5, 2.5]) {
                                cube([34.5, 3, 3], center=true);
                            }
                            translate([-14.5, 12.3, 3.7]) rotate([0, 90, 0]) cylinder(d=5.5, h=35);
                        }
                    }
                }
                // Rounding mask
                union() {
                    translate([10, 0, height/2]) rotate([90, 0, 0]) cylinder(d=height, h=40, center=true);
                    //translate([0, -20, -5]) cube([40, 40, 15]);
                    translate([-30, -20, 0]) cube([40, 40, 40]);
                }
            }
            // outside standoff grippers
            dupmirror() translate([-12.5, 14.5, height/2]) cylinder(d=9, d2=8, h=height, center=true);
            // Rounding off the front of the above on the plate.
            hull() {
                dupmirror() translate([-12.5, 14.5, 1/2]) cylinder(d=9, h=1, center=true);
            }
        }
        // Angle to slice off the back
        translate([3, -20, -10]) rotate([0, 45, 0]) cube(40);
        // screw holes for camera
        translate([12, 20, 15.5]) rotate([90, 0, 0]) cylinder(d=2, h=40);
        // Standoff holes
        dupmirror() translate([-12.4, -14.5, -.1]) linear_extrude(30) circle(d=6.75, $fn=6);
        // Mounting slot along the bottom
        hull() {
            translate([9, 0, -.1]) cylinder(d=3, h=5, center=true);
            translate([-2, 0, -.1]) cylinder(d=3, h=5, center=true);
        }
    }
}

// Honeycomb based on http://www.thingiverse.com/thing:1763704
module honeycomb(x, y, cell, h, w)  {
    sqrt3=sqrt(3);
    little=0.01;

	difference()  {
		cube([(1.5*x+0.5)*cell+w, cell*sqrt3*y+1.5*w-w/2, h]);

		translate([w-cell*2, w, -little/2])
            linear_extrude(h+little) {
            for (a=[0:x/2+1], b=[0:y], c=[0:1/2:1/2])
                translate([(a+c)*3*cell-w/2, (b+c)*sqrt3*cell-w/2])
                    circle(cell-w, $fn=6);
        }
    }
}

module honeycombable(h) {
    points = [[0, 0], [4, 23], [14, 20], [14, 0]];
    linear_extrude(h) polygon(points);
}

module airtranslation() {
    translate([-5.5, 19, 19]) {
        rotate([90, 98, 0]) {
            children();
        }
    }
}

module block() {
    intersection() {
        airtranslation() {
            honeycombable(40);
        }
        mainbody();
    }
}

module punchout() {
    render()
        airtranslation() {
        intersection() {
            honeycombable(40);
            honeycomb(10, 8, 2, 40, .5);
        }
    }
}

union() {
    difference() {
        mainbody();
        airtranslation() {
            honeycombable(40);
        }
    }
    intersection() {
        block();
        punchout();
    }
}
