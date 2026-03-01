include <BOSL2/std.scad>

// The largest width of the connector.
largeWidth = 41;
// The smallest width of the connector.
smallWidth = 36;
// Width of the notch at the bottom of the connector.
notchWidth = 9;
// Distance between the top of the connector and the bottom (excluding the notch).
baseDepth = 17.5;
// Distance between the top of the connector and the bottom (including the notch).
overallDepth = 19.5;

/* [Advanced] */

// Radius of the larger curve near the top of the connector.
largeRad = 4;
// Radius of the smaller curve near the bottom of the connector.
smallRad = 3;
// Fillet radius by the notch at the bottom.
notchRad = 2;

// How much of the connector should seat into the cap.
height = 13;
// Overall thickness of the cap.
thickness = 2;

/* [Hidden] */
$fa = 2;
$fs = 0.02;
notchDepth = overallDepth - baseDepth;


module halfBase() {
  translate([(largeWidth / 2) - largeRad,
             overallDepth - largeRad, 0])
    circle(largeRad);
  
  translate([(smallWidth / 2) - smallRad,
             smallRad + (overallDepth - baseDepth), 0])
    circle(smallRad);
}

module halfNotch() {
  difference() {
    square([notchWidth/2 + notchRad, notchDepth], center = false);
    right(notchWidth / 2 + notchRad) circle(r=notchRad);
  }
}

module profile() {
  union() {
    hull() xflip_copy() halfBase();
    xflip_copy() halfNotch();
  };
}

difference() {
  linear_extrude(height+thickness)
    offset(thickness) profile();
  up(thickness) linear_extrude(height+1) profile();
}
