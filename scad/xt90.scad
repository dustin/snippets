w = 8;
wt = 4.5;
l1 = 16.5;
lo = 19.2;
h = 12;
chamfer_height = 0.5;
base_thick = 0.2;
wall_thick = 1;
$fa = 2;
$fs = 0.02;

module xtfoot() {
    offset(delta=.2) // give it a bit of wiggle room.
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

module grip() {
  linear_extrude(h) circle(0.5);
}

module grips() {
    for(yo = [l1 * (1/4), l1 * (3/4)]) {
        translate([wall_thick / -4, yo, base_thick + chamfer_height]) grip();
        translate([w - (wall_thick / -4), yo, base_thick + chamfer_height]) grip();        
    }
}

module xt90() {
    // Walls
    translate([0, 0, base_thick + chamfer_height]) xterior(wall_thick);
    
    // Additional little grips to friction fit better
    // grips();
    
    // Chamfer transition
    translate([0, 0, base_thick])
    hull() {
        linear_extrude(0.01) offset(1.5) xtfoot();
        translate([0, 0, chamfer_height]) 
            linear_extrude(0.01) offset(1) xtfoot();
    }
    
    // Bottom base
    linear_extrude(base_thick) offset(1.5) xtfoot();
}

xt90();