include <threads.scad>

$fa=2;
$fs=.02;

thickness = .75;
margin = .5;

ssx = 75;
ssy = 55;
ssz = 9;

module mirror_over_plate(pw=ssx) {
    children();
    translate([pw/2, 0, 0]) mirror([1, 0, 0]) translate([-pw/2, 0, 0]) children();
}

module standoff(h) {
    cylinder(h=2, d1=7.5, d2=5.5);
    difference() {
        cylinder(h=h, d=5.5);
        translate([0, 0, 1]) cylinder(h=h-.9, d=3);
    }
    translate([0, 0, 1]) thread_in(3, h-1);
}

module standoffs(h) {
    mirror_over_plate(ssx+2*margin) {
        offset = 1 + (5.5/2);
        translate([margin+offset, margin+offset, thickness]) standoff(6);
        translate([margin+offset, ssy-offset-margin, thickness]) standoff(6);
    }
}

module mountingplate() {
    cube([ssx+margin*2, ssy+margin*2, thickness]);
    translate([ssx/2+margin, ssy/2+margin, thickness]) linear_extrude(thickness)
        resize([ssx-10, 0])
            text("Peligro!", size=15, font="Rock Salt",
                valign="center", halign="center");
    standoffs();
}

linear_extrude(ssz) {
    difference() {
        square([(margin+thickness)*2+ssx, (margin+thickness)*2+ssy]);
        translate([thickness+margin, thickness+margin]) square([ssx, ssy]);
    }
}
translate([thickness, thickness, 0]) mountingplate();