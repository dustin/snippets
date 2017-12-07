b_d = 90;
b_w = 90;
b_h = 75;

module box() {
    module s() {
        difference() {
            cube([b_w, b_d, b_h]);
            translate([-.2, 0, 30]) rotate([60, 0, 0])
                cube([91, 90, 90]);
        }
    }
    difference() {
        s();
        translate([2, 2, 2]) resize([b_w - 4, b_d - 4, b_h - 4])
            s();
        translate([2, b_d/2, 2]) cube([b_w - 4, b_d, b_h - 4]);
    }

}

module dps() {
    cube([71.5, 39, 40]);
}

module banana() {
    rotate([90, 0, 0]) cylinder(d=6, h=10, $fa=2, $fs=0.05);
}

module case() {
    difference() {
        box();
        translate([(b_w - 71.5)/2, 12, 30])
            rotate([60, 0, 0]) dps();
        translate([20, 5, 15]) banana();
        translate([35, 5, 15]) banana();
    }
}

module vent() {
    $fa=2;
    $fs=0.05;
    rotate([0, 90, 0]) hull() {
        cylinder(d=3, h=b_w+2);
        translate([0, 50, 0]) rotate([0, 0, 0]) cylinder(d=3, h=b_w+2);
    }
}

difference() {
    case();
    for (i = [0:3]) {
        translate([-.1, 20 + (10*i), 15])
            rotate([60, 0, 0]) vent();
    }
    for (i = [0:7]) {
        translate([10 + 10*i, 33, 100]) rotate([0, 90, 0]) vent();
    }
    translate([-.1, b_d-7, 15]) cube([b_w + 2, 3, 3]);
    translate([-.1, b_d-7, b_h-15]) cube([b_w + 2, 3, 3]);
}