// Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
//
// $Id: spy.pov,v 1.2 2003/04/20 08:19:04 dustin Exp $

#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"
#include "chars.inc"
#include "skies.inc"

#include "spy_words.inc"

camera {
  location <0, -1.8, -10>
  look_at <0, 0, 0>
}

sky_sphere { S_Cloud5 }

// Light on the right

light_source {
	<3.3, 1, -4>
	color Gray75
	looks_like {
		sphere { <0, 0, 0>, .3
			texture {
				Chrome_Texture
				pigment { White }
				normal { bumps 0.4 scale 0.2 }
			}
		}
	}
}

cylinder {
	<3.3, .9, -4>, <3.3, -2, -4>, .1
	texture {
		Chrome_Texture
		pigment { White }
		normal { bumps 0.2 scale 0.2 }
	}
}

// Light on the left

light_source {
	<-3.3, 1, -4>
	color Gray75
	looks_like {
		sphere { <0, 0, 0>, .3
			texture {
				Chrome_Texture
				pigment { White }
				normal { bumps 0.4 scale 0.2 }
			}
		}
	}
}

cylinder {
	<-3.3, .9, -4>, <-3.3, -2, -4>, .1
	texture {
		Chrome_Texture
		pigment { White }
		normal { bumps 0.2 scale 0.2 }
	}
}

// The water

plane {
  <0, 2, 0>, -2
  texture { Water scale 15 }
  pigment { color red 0.5 green 0.5 blue 0.9}
  finish  { ambient .2 diffuse .4 reflection .8}
}

// Letter textures

#declare lettering = Brushed_Aluminum

#declare small_lettering =
texture {
	lettering
	pigment { rgb <0.15, 0.15, 0.4> }
}

// SPY
object {
	word_spy
	translate <0, -2, 0>
	texture { lettering }
}

// INTERNETWORKING
object {
	word_internetworking
	translate <0, -15.5, -2>
	texture { small_lettering }
	scale 0.13
}
