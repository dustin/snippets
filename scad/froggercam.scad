$fa=2;
$fs=.05;

height = 20;
length = 22;
width = 34;
hole_offset =  26.5/2; // 14.5;

module dupmirror() {
    children();
    mirror([0, 1, 0]) children();
}

module mainbody() {
    difference() {
        union() {
            // outside standoff grippers
            dupmirror() translate([0, hole_offset, height/2]) cylinder(d=9, d2=8, h=height, center=true);
            // Rounding off the front of the above on the plate.
            hull() {
                dupmirror() translate([0, hole_offset, 1/2]) cylinder(d=9, h=1, center=true);
            }
            // base pate
            translate([(length-4)/2, 0, 0.5]) cube([length-4, width, 1], center=true);
            // walls
            intersection() {
                union() {
                    dupmirror() translate([(length+4.5)/2, width/2-3/2, height/2]) cube([length+4.5, 3, height], center=true);
                    // fillet
                    dupmirror() {
                        difference() {
                            translate([length/2, 13.5, 2.5]) {
                                cube([length, 3, 3], center=true);
                            }
                            translate([0, 12.3, 3.7]) rotate([0, 90, 0]) cylinder(d=5.5, h=length);
                        }
                    }
                }
                // Rounding mask
                union() {
                    translate([length, 20, 15.5]) rotate([90, 0, 0]) cylinder(d=9, h=40);
                    translate([0, -20, 0]) cube([length, 40, height]);
                    translate([-length, -20, 17]) rotate([0, 30, 0]) cube([40, 40, 20]);
                }
            }
        }
        // Angle to slice off the back
        translate([-6.2, -20, -17.2]) rotate([0, 45, 0]) cube(40);
        // screw holes for camera
        translate([length, 20, 15.5]) rotate([90, 0, 0]) cylinder(d=2, h=40);
        // partial holes for the mount nips
        translate([length, 15, 15.5]) rotate([90, 0, 0]) cylinder(d=4.5, h=width-4);
        // Standoff holes
        dupmirror() translate([0, -hole_offset, -.1]) linear_extrude(30) circle(d=6.75, $fn=6);

        translate([15, -(width/2)-5, 3]) rotate([90, 0, 180]) scale([.1, .1, .5]) surface("hawk.png");
    }
}


mainbody();
