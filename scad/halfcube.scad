$fn = 50;

module halfcube(size, height, thickness) {
    l = sqrt(size*size + size*size);
    l2 = l-(thickness * 2);

    translate([-l2/2, -thickness, -height/2]) cube([l2, thickness, height]);
    difference() {
        rotate(45) {
            difference() {
                cube([size, size, height], center=true);
                translate([0, 0, 1]) {
                    cube([size-(thickness*2), size-(thickness*2), height],
                         center=true);
                }
            }
        }
        translate([-l/2, 0, -l]) {
            cube([l, l, height+l]);
        }
    }
}

halfcube(70, 70, 2);
