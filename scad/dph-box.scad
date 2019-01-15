
module base() {
    holed = 3.5;
    holer = holed/2;
    holeoff = holer + 1.5;
    basew = 93;
    based = 71;
    thickness = 1;
    
    module fan() {
        cube([30, 20, 30]);
    }
    
    translate([30, 0, 4]) fan();
    
    module plug() {
        cube([20.5, 8, 19.3]);
        translate([0, 0, 19.3-8]) cube([20.5, 11, 8]);
    }
    
    translate([8+1.25, 12, thickness]) rotate(90) plug();
    translate([basew - (8+1.25), 12+20.5, thickness])
        rotate(270) plug();

    module comm() {
        cube([22.5, 5.75, 7.25]);
    }
    
    translate([11.7, based - 5.75 - 2.6, thickness]) comm();
    translate([basew - 11.7 - 22.5, based - 5.75 - 2.6, thickness]) comm();

    difference() {
        cube([basew, based, thickness]);

        translate([holeoff, holeoff, 0])
        for (i = [[0, 0], [0, 1], [1, 0], [1, 1]]) {
            translate([(i[0]*86), (i[1]*64), -.1])
                cylinder(h=2, d=3.8, $fa=2, $fs=0.05);
        }
    }
}

base();
