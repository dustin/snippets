// $Id: roundbar.pov,v 1.1 2003/04/20 06:30:18 dustin Exp $

#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"
#include "jewelry.inc"

camera {
  location <0, 3, -10>
  look_at <0, 1, 2>
}

light_source { <4, 5, -10> color White}
light_source { <-4, 3, -10> color White}

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
   RoundBar
   rotate <50, 20, -20>
   translate y
   texture{ Chrome_Texture }
   pigment { White }
}

object {
   Barbell
   rotate <-40, 15, 30>
   translate <0, 1.0, 1.5>
   texture{ Chrome_Texture }
   pigment { White }
}
