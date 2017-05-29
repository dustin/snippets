$fa=2;
$fs=.05;

thickness=2;
width=28;
length=43;

gopro_width = 13;
gopro_length = 15;

module gopro() {
    module finger() {
        difference() {
            hull() {
                translate([0, gopro_length/2, 8]) rotate([0, 90, 0]) cylinder(d=gopro_length, h=2.8);
                cube([2.8, gopro_length, 8]);
            }
            translate([-.1, gopro_length/2, 8]) rotate([0, 90, 0]) cylinder(d=5, h=3);
        }
    }
    union() {
        cube([gopro_width, gopro_length, thickness]);
        translate([2, 0, thickness]) {
           finger();
           translate([2.8+3.3, 0, 0]) finger();
        }
    }
}

module hook() {
    difference() {
        cube([4.5, 6, 4.5]);
        translate([2.25, -.05, -1]) cube([4.5, 6.1, 4.5]);
    }
}

module dupmirror() {
    children();
    translate([width, 0, 0]) mirror() children();
}

module hooks() {
    hook();
    translate([0, 30, 0]) hook();
}

module mount() {
    cube([28, 43, thickness]);
    dupmirror() {
        translate([2.5, 2.8, thickness]) hooks();
    }
}

union() {
    mount();
    translate([width/2 - gopro_width/2, length/2 + gopro_length/2, 0]) rotate([180, 0, 0]) gopro();
}
