// $Id: ball.pov,v 1.2 2003/04/20 08:29:43 dustin Exp $

#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"
#include "jewelry.inc"

camera {
	location <0, 3, -10>
	look_at <0, 1, 2>
}

light_source { <4, 5, -3> color White}
light_source { <-4, 3, -2> color White}

plane {
	<0, 2, 0>, -4
	pigment {
		hexagon
		color White
		color Black
		color Blue
		scale 2
	}
	finish { ambient .2 reflection .8 }
}

object {
	CBR
	rotate <50, 20, -20>
	texture{ Chrome_Texture }
	pigment { White }
}

object {
	Barbell
	rotate <20, 10, -20>
	translate <0, 1, 0>
	texture{ Chrome_Texture }
	pigment { White }
}
