// $Id: spy.pov,v 1.1 2003/04/20 06:30:21 dustin Exp $

#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"
#include "chars.inc"
#include "skies.inc"

camera {
/*
  location <0, 4, -10>
  look_at <0, 0, 0>
*/
  location <0, -1.8, -10>
  look_at <0, 0, 0>
}

light_source {
   <3.3, 1, -4>
   color Gray75
   looks_like { sphere { <0, 0, 0>, .3 pigment { White } }}
}

light_source {
   <-3.3, 1, -4>
   color Gray75
   looks_like { sphere { <0, 0, 0>, .3 pigment { White } }}
}

sky_sphere { S_Cloud5 }

cylinder {
   <3.3, .9, -4>, <3.3, -2, -4>, .1
   pigment { White }
}

cylinder {
   <-3.3, .9, -4>, <-3.3, -2, -4>, .1
   pigment { White }
}

/*
sphere {
   <0, 0, 0>, 50
   // pigment { Apocalypse }
   pigment {
   image_map {
      gif "smallspy1.gif"
      map_type 0
      interpolate 2
   }
   }
}
*/

plane {
  <0, 2, 0>, -2
  texture { Water scale 10 }
  pigment { color red .6 green .6 blue 1}
  finish  { ambient .2 diffuse .4 reflection .9}
}

#declare lettering1 =
texture {
	pigment {
		image_map {
		gif "smallspy1.gif"
		map_type 0
		interpolate 2
		}
	}
	finish { Shiny }
	normal {dents 0.8}
}

#declare lettering = Brushed_Aluminum

object {
	char_S
	translate <-5, -2, 0>
	texture { lettering }
}

object {
	char_P
	translate <0, -2, 0>
	texture { lettering }
}

object {
	char_Y
	translate <5, -2, 0>
	texture { lettering }
}
