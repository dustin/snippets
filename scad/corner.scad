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

module vase(w, h, d, t) {
    ih = (d - (2*t)) / tan(atan(d/h));
    iw = (d - (2*t)) / tan(atan(d / (w/2)));
    difference() {
        poly(0, w/2, -w/2, 0, d, 0, h);
        poly(0, iw-t, -iw+t, t, d-(2*t), h-ih, h);
    }
}

vase(40, 50, 25, 1.5);
