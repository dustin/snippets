include <BOSL2/std.scad>

// Diameter of the current guide pin.
pinDiam = 4;
// Vertical clearance available within the track.
verticalClear = 25;
// Horizontal clearance within the track.
horizontalClear = 6;
// Total length of the resulting guide.
totalLen = 40;
// Extra space to allow things to fit loosely.
allowance = 0.5;

/* [Hidden] */
$fa=2;
$fs=.05;

intersection() {
  difference() {
    linear_extrude(height = verticalClear - allowance)
      ellipse(r=[totalLen / 2, (horizontalClear - allowance) / 2]);
    cylinder(h=verticalClear - 2, r=(pinDiam + allowance) / 2);
    sphere(d=horizontalClear - 1);
  };
    
  sphere(verticalClear);
}
