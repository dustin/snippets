$fa=2;
$fs=.05;

difference() {
    cylinder(h=4, d=85);
    translate([0, 0, 3]) cylinder(h=2, d=82, d2=85);

    translate([0, 0, -24])
    scale([.7, .7, .5]) surface("hazard.png", center=true);
}
