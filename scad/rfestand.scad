// Which one would you like to see?
part = "right"; // [left:Left Side,right:Right Side,dowels:Dowels]


// Viewing Angle
angle=20; // [0:90]
// Overall height.
height=100; // [40:200]
// Overall depth.
depth=60; // [40:200]
// Length of the device holder part.
holder_len=65; // [20:180]
// Depth of the device holder part.
holder_depth=21; // [0:100]
// Width of the dowels.
width=50; // [20:200]

$fa=2 * 1;
$fs=.02 * 1;

// These aren't exposed in configurator, but make it a lot easier to understand what's going on below.
thickness = 5 * 1;
halfthick = thickness / 2;
doweldiam = 9 * 1;
dowelout = doweldiam * 1.666666666666666;
halfdowelout = dowelout/2;


module line(points=[], width=1) {
    for(i = [1:len(points)-1]) {
        hull() {
            translate([points[i-1][0], points[i-1][1]]) circle(d=width);
            translate([points[i][0], points[i][1]]) circle(d=width);
        }
    }
}

module athing() {
    difference() {
        linear_extrude(10) {
            line([[depth, 0], [0, 0], [0, height], [dowelout, height]], width=thickness);
            translate([dowelout, height]) rotate([0, 0, angle])
                line([[0, 0], [0, -holder_len], [holder_depth, -holder_len]], width=thickness);
            translate([halfdowelout, halfdowelout-halfthick]) circle(d=dowelout);
            translate([depth, halfdowelout-halfthick]) circle(d=dowelout);
            translate([1+halfdowelout, height - dowelout*.4]) circle(d=dowelout);
        }
        translate([halfdowelout, halfdowelout-halfthick, 7]) cylinder(d=doweldiam, h=8, center=true);
        translate([depth, halfdowelout-halfthick, 7]) cylinder(d=doweldiam, h=8, center=true);
        translate([1 + halfdowelout, height-dowelout*.4, 7]) cylinder(d=doweldiam, h=8, center=true);
    }
}

if (part == "left") {
    mirror() athing();
} else if (part == "right") {
    athing();
} else if (part == "dowels") {
    for(i = [0, 15, 30]) {
        translate([i, 0]) cylinder(d=doweldiam, h=width);
    }
}
