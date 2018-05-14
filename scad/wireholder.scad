// https://gist.github.com/dustin/0b48e7e7b7d318752acccaaaf4f590a5
$fa = 2;
$fs = 0.3;
smooth = 1;

module line(points=[], width=1) {
    for(i = [1:len(points)-1]) {
        hull() {
            translate([points[i-1][0], points[i-1][1]]) circle(d=width);
            translate([points[i][0], points[i][1]]) circle(d=width);
        }
    }
}

module cableroute(length, width, thickness) {
    line([for(i = [0 : 10 : 360]) [i / (360/length), width * sin(i)]], width=thickness);
}

module wireholder(width, gap) {
    difference() {
        translate([width/2, 0, 0]) sphere(d=width);
        linear_extrude(width/2 + .1) cableroute(width, 3.5, gap);
        translate([0, -width/2, -width/2-2]) cube([width, width, width/2]);
        translate([0, -width/4, 0]) cube([width, width/2, gap+.5]);
    }
}


if (smooth > 0) {
    minkowski() {
        wireholder(25, 5);
        sphere(d=smooth);
    }
} else {
    wireholder(25, 5);
}
