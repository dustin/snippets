#include "textures.inc"
#include "colors.inc"
#include "lights.inc"

background { Black }

light_source { <2, 5, -10> color White }

camera { location <0,0,-5> look_at <0,0,0> }

object {
	light_bulb1
	// Lightbulb
	pigment { White }
}
