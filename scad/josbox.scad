aa_diameter = 14.2;
aa_len = 50.5;

thickness = 1;

interior_width = (aa_diameter+.2)*3;
interior_len = aa_len + .5;
box_width = 2*thickness + interior_width;
box_len = 2*thickness + interior_len;
box_height = aa_diameter+2;

font = "Henny Penny";

// https://github.com/JustinSDK/dotSCAD/blob/master/src/box_extrude.scad
module box_extrude(height, shell_thickness) {
    linear_extrude(shell_thickness)
        children();

    linear_extrude(height)
        difference() {
            children();
            offset(delta = -shell_thickness)
                children();
        }
}

module name() {
    linear_extrude(.3) text("Joselyn", font=font, size=8, valign="center", halign="center");
}

module bottom() {
    box_extrude(box_height, thickness)
        square([box_width, box_len]);

    translate([box_width/2, 0, box_height/2]) rotate([90]) name();
    translate([box_width/2, box_len, box_height/2]) rotate([90, 0, 180]) name();
    translate([box_width, box_len/2, box_height/2]) rotate([90, 0, 90]) name();
    translate([0, box_len/2, box_height/2]) rotate([90, 0, 270]) name();
}

module lid() {
   iw = 30;
   translate([0, 0, thickness]) cube([box_width, box_len, thickness]);
   translate([(box_width-interior_width)/2, (box_len-interior_len)/2, 0]) cube([interior_width, interior_len, thickness]);
   translate([(box_width/2)-(iw/2), (box_len/2)-(iw/2), thickness/2]) resize([iw, iw, thickness*2])  surface(file="hibiscus-small.png");
}

// bottom();
lid();
