$fn = 120;

// Thanks, http://kitwallace.tumblr.com/post/87637876144/list-comprehension-in-openscad
function reverse(v) =
  [ for (i = [0:len(v)-1])  v[len(v) -1 - i] ];

module sinething(length, width, thickness) {
    table = [for(i = [0 : 10 : 360]) [i / (360/length), width * sin(i)]];
    right = [for(i = table) [i[0], i[1] + (thickness/2)]];
    left = [for(i = table) [i[0], i[1] - (thickness/2)]];
    polygon(concat(left, reverse(right)));
}

minkowski() {
    difference() {
        translate([25/2, 0, 0]) sphere(d=25);
        linear_extrude(12.6) sinething(25, 5, 5);
        translate([0, -12.5, -14.5]) cube([25, 25, 12.5]);
        translate([0, -13/2, 0]) cube([25, 13, 6]);
    }
    sphere(d=0.75);
}
