// https://gist.github.com/dustin/e23e47ce2e6e13e273450eab75bc5ee5
$fa=2;
$fs=.02;
thickness = 7;
armlen = 50;
r608 = 22;

module z() {
    for(i = [[[0, 0], [armlen, 0]],
            [[armlen, 0], [0, -armlen]],
            [[0,  -armlen], [armlen, -armlen]]]) {
        hull() for(s = i) {
            translate([s[0], s[1], 0]) cylinder(d=r608+4, h=thickness);
        }
    }
}

difference() {
    z();
    for(i = [[0, 0], [0, 1], [1, 1], [1, 0], [.5, .5]]) {
        translate([armlen*i[0], -armlen*i[1], -.1]) cylinder(d=r608, h=thickness+.2);
    }
}
