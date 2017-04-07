w = 6.25;
wt = 1.8;
l1 = 11.4;
lo = 14;
h = 8.5;
$fa = 2;
$fs = 0.02;

module xtfoot() {
    offset(delta=.1) // give it a bit of wiggle room.
        polygon([[0, 0], [w, 0], [w, l1],
            [w/2 + wt/2, lo], [w/2 - wt/2, lo], [0, l1]]);
}

module xterior(thickness) {
    linear_extrude(h) {
        difference() {
            offset(thickness) xtfoot();
            xtfoot();
        }
    }
}

module xt60() {
    translate([0, 0, 1]) xterior(.75);
    linear_extrude(1) offset(1.5) xtfoot();
}

xt60();
