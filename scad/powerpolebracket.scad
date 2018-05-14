$fa=2 * 1;
$fs=.02 * 1;

thickness = 3;

appw = 16.5;
appiw = 8;
apph = 8.5;
appoff = 2;

cubew = 30;
cubeh = apph / 2 + thickness;
cubed = 7;

pind = 2.5 - .2;
pinh = apph / 2;

// Main block/cutout
difference() {
    cube([cubew, cubed, cubeh]);
    translate([cubew/2 - appw/2, -.1, thickness])
        cube([appw, 10, cubeh+1]);
    for (o = [-1, 1]) {
        translate([cubew/2 + (cubew/2 - (cubew-3.5)) * o, cubeh+.5, cubeh/2])
            rotate([90, 0, 0]) cylinder(d=2, h=cubeh+1);
    }
}

// pins
for (o = [-1, 0, 1]) {
    translate([cubew/2 - (appiw * o), appoff, thickness])
        cylinder(d=pind, h=pinh);
}
