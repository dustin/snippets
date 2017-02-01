$fn=30;

$len=60;
$top=20;
$bottom=40;
$toolspace=8;
$armwidth=5;

module arm(l, w, h) {
    rotate([10, 0, 0]) {
        hull() {
            cube([l, w, h], center=true);
            translate([0, 0, h/2]) sphere(d=l, center=true);
        }
    }
}

module arms(l, w, h) {
    translate([$armwidth/2+$toolspace/2, $top, 10]) arm(l, w, h);
    mirror() translate([$armwidth/2+$toolspace/2, $top, 10]) arm(l, w, h);
}

difference() {
    minkowski() {
        hull() {
            translate([0, -$len/2, 0]) cylinder(d=$top, h=3, center=true);
            translate([0, $len/2, 0]) cylinder(d=$bottom, h=3, center=true);
        }
        difference() {
            sphere(1, center=true);
            translate([0, 0, -1]) cube(2, center=true);
        }
    }
    translate([0, -$len/2, 0]) cylinder(d=8, h=5.1, center=true);
}

arms(5, 5, 20);
