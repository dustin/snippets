include <BOSL2/std.scad>

od=25;
id=8;
h=12;

$fa = 2;
$fs = 0.02;

difference() {
  cyl(h=h, r=od/2, chamfer=1);
  cyl(h=h, r=id/2, chamfer=-1);
}