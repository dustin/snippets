// https://gist.github.com/dustin/94265d695f329c84b0e50976a027aa6a
$fa=2;
$fs=.02;

// https://www.youtube.com/watch?v=EgOuvOLiwAM
// circles:  22, 11, 8, 4
// 7mm thick -- 5 each end

r608 = 22;
r608id = 8;

translate([24, 0, 0]) difference() {
    union() {
        cylinder(d=r608, h=1);
        cylinder(d=11, h=2);
        cylinder(d=8, h=7);
    }
    translate([0, 0, 1]) cylinder(d=4, h=8);
    translate([0, 0, 6]) cylinder(d=4, d2=5);
}

difference() {
    union() {
        cylinder(d=r608, h=1);
        cylinder(d=11, h=2);
    }
    translate([0, 0, 1]) cylinder(d=8, h=2);
}
cylinder(d=3.9, h=7);

