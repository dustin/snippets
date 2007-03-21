// $Id: ipa.pov,v 1.2 2003/04/20 08:29:45 dustin Exp $

#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"
#include "chars.inc"

camera {
	location <0, -1.8, -10>
	look_at <0, 0, 0>
}

light_source {
	<3.3, 1, -4>
	color White
	looks_like { sphere { <0, 0, 0>, .3 pigment { White } }}
}

light_source {
	<-3.3, 1, -4>
	color White
	looks_like { sphere { <0, 0, 0>, .3 pigment { White } }}
}

cylinder {
	<3.3, .9, -4>, <3.3, -2, -4>, .1
	pigment { White }
}

cylinder {
	<-3.3, .9, -4>, <-3.3, -2, -4>, .1
	pigment { White }
}

plane {
	<0, 2, 0>, -2
	texture { Water }
	pigment { color red .6 green .6 blue 1}
	finish  { ambient .2 diffuse .4 reflection .9}
}

object {
	char_I 
	translate <-5, -2, 0>
	pigment {
		image_map {
			gif "smallspy1.gif"
			map_type 0
			interpolate 2
		}
	}
	// texture{ Chrome_Texture }
	finish { Shiny }
	normal {dents 0.8}
}

object {
	char_P
	translate <0, -2, 0>
		pigment {
			image_map {
			gif "smallspy1.gif"
			map_type 0
			interpolate 2
		}
	}
	// texture{ Chrome_Texture }
	finish { Shiny }
	normal {dents 0.8}
}

object {
	char_A 
	translate <5, -2, 0>
	pigment {
		image_map {
			gif "smallspy1.gif"
			map_type 0
			interpolate 2
		}
	}
	// texture{ Chrome_Texture }
	finish { Shiny }
	normal {dents 0.8}
}
