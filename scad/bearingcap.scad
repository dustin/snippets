// https://gist.github.com/dustin/94265d695f329c84b0e50976a027aa6a
$fa=2;
$fs=.02;

r608 = 22;
r608id = 8;

ss = 100;
intersection() {
    union() {
        cylinder(h=3, d=r608id);
        translate([0, 0, 3]) cylinder(h=5, d=r608);
    }
    translate([0, 0, -ss+4]) sphere(ss, $fa=.5, $fs=.01);
}
