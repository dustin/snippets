#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"
#include "skies.inc"

camera {
	location <0, 3, -10>
	look_at <0, 1, 10>
}

light_source { <4, 5, -10> color White}

sphere { <0,0,0>, 10000
	texture {
		T_Cloud1
		scale 1000
	}
}

plane { y, -2000
	texture {
		Water
	}
	pigment { color red .6 green .6 blue 1 }
	finish { ambient .2 diffuse .4 reflection .9}
}
