module pyr(b, h) {
    w=sqrt(pow(b,2) * 2);
    rotate(45) cylinder(d1=w, d2=.001, h=h, $fn=4);
}

module cutout(b, h, t) {
    module m() {
        resize([b-t, b-t, h-t]) pyr(b, h);
    }
    hull() {
        translate([0, 0, t/3]) m();
        translate([-t, 0, t/3]) m();
    }
}

module hpyr(b, h) {
    difference() {
        pyr(b, h);
        cutout(b, h, 5);
    }
}

hpyr(76, 50);