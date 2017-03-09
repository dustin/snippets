include <threads.scad>

$fa=2;
$fs=.05;

FCDIST = 30.5;
plate_width = 120;
plate_height = 42;
thickness = 1.5;

function fccenter(x, y, z) = [(x/2) - (FCDIST/2), (y/2) - (FCDIST/2), z];

module fcmount() {
    for(i = [[0, 0], [0, 1], [1, 1], [1, 0]]) {
        translate([i[0]*FCDIST, i[1]*FCDIST, 0]) {
            difference() {
                cylinder(h=7, d=5.5);
                translate([0, 0, 1]) cylinder(h=6.1, d=3);
            }
            translate([0, 0, 1]) thread_in(3, 6);
        }
    }
}

module mirror_over_plate(pw=plate_width) {
    children();
    translate([pw/2, 0, 0]) mirror([1, 0, 0]) translate([-pw/2, 0, 0]) children();
}

module pdbmount() {
    difference() {
        cube([50.5, 20.5, .5]);
        translate([0, 5, -.1]) cube([50.5, 11, .7]);
        mirror_over_plate(50.5) {
            translate([2, 2, -.1]) cylinder(d=1.5, h=thickness+.2);
            translate([50.5-2, 20.5-2, -.1]) cylinder(d=1.5, h=thickness+.2);
        }
    }
}


// This is the section I want honeycombed.
module honeycombable(h) {
    points = [[2, 2], [32, 11.5], [32, 32], [2, plate_height-2]];
    mirror_over_plate() linear_extrude(h) polygon(points);
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

module roundish_cube(x, y, z, d) {
    hull() {
        for(i = [[0, 0], [0, 1], [1, 1], [1, 0]]) {
            translate([i[0] * x, i[1] * y, 0])
                cylinder(d=d, h=z);
        }
    }
}

module roundtop_cube(w, l, d) {
    r=d;
    points=[[r, r], [w-r, r], [w-r, l-r], [r, l-r]];
    hull() difference() {
        for(i = points) { translate(i) sphere(r=r); }
        translate([0, 0, -2*r]) cube([w, l, r*2]);
    }
}

module s800mount() {
    difference() {
        // Main board
        roundtop_cube(plate_width, plate_height, thickness);
        // Punch out a hole for the PDB mount
        translate([34.75, 10.75, -.1]) cube([50.5, 20.5, thickness+.2]);
        // Add the space for the honeycomb.
        translate([0, 0, -.1]) honeycombable(thickness+.2);
        // ESC Wires
        translate([plate_width/2-10, plate_height-3, -.1]) {
            roundish_cube(20, 3.5, thickness+.2, 1.5);
        }
        // Servo Wires
        mirror_over_plate() {
            translate([7, plate_height-2.2, -.1])
                roundish_cube(6, 2.4, thickness+.2, 1);
        }
    }

    // add the PDB mount.
    translate([34.75, 10.75, 0]) pdbmount();

    // Add a place to put the flight controller.
    translate(fccenter(plate_width, plate_height, thickness)) fcmount();
    
    // Shove the honeycomb in.  Render is required for preview.
    render() intersection() {
        honeycombable(thickness);
        mirror_over_plate() honeycomb(10, 8, 3, 10, .5);
    }
}

s800mount();
