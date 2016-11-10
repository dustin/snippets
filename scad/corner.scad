$fn = 50;

module poly(x1, x2, x3, y1, y2, z1, z2) {
    polyhedron(
        points=[
            [x1, y1, z1],  // 0
            [x2, y1, z2],  // 1
            [x3, y1, z2],  // 2
            [x1, y2, z2]], // 3
        faces=[[0, 3, 1],
               [1, 3, 2],
               [0, 2, 3],
               [1, 2, 3],
               [0, 1, 2]]);
}

module nailmount(d) {
    ot = d - .3;
    it = d - ot;
    rotate([90, 270, 0]) {
        // Outside (uneven circles)
       hull() {
            translate([d, 0, 0]) cylinder(h=d, r=2.5);
            translate([3 + d, 0, 0]) cylinder(h=d, r=1.5);
        }
        // Inside bits (even circles)
        hull() {
            translate([d, 0, 0]) cylinder(h=ot, r=2.5);
            translate([3 + d, 0]) cylinder(h=ot, r=2.5);
        }
    }
}

module vase(w, h, d, t) {
    ih = (d - (2*t)) / (d/h);
    iw = (d - (2*t)) / (d / (w/2));
    difference() {
        poly(0, w/2, -w/2, 0, d, 0, h);
        poly(0, iw-t, -iw+t, t, d-(2*t), h-ih, h);
    }
}

difference() {
    w=40;
    h=50;
    d=25;
    t=1.5;

    vase(w, h, d, t);
    translate([0, t-.20001, h*.75]) nailmount(t-.2);
}
