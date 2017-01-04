$fn=30;

module diamond(w, w2, h1, h2) {
    $fn=9;
    cylinder(d1=0, d2=w, h=h1);
    translate([0, 0, h1]) cylinder(d1=w, d2=w2, h=h2);
}

module dvase(w, w2, h1, h2) {
    difference() {
            diamond(w, w2, h1, h2);
            translate([0, 0, 1]) resize([w-2, w-2, h1+h2-1.5]) diamond(w, w2, h1, h2);
            translate([0, 0, h1]) cylinder(d=w2-4, h=h2+.1);
    }
}

dvase(50, 32, 35, 10);
