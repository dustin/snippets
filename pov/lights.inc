// light bulbs

#declare light_bulb1 =
	union {
		blob {
			sphere { <0,0,0>, 1, 10 }
			cylinder { <0,0,0>, <0,-1,0>, 0.75, 5 }
			/*
			sphere { <0,-0.5,0>, 0.7, 5 }
			sphere { <0,-1,0>, 0.7, 5 }
			*/
		}
	}

#declare light_bulb =
	union {
		cone {
			<0,1.5,0>, 1
			<0,0,0>, 0.1
		}
		blob {
			sphere { <0,1,0>, 0.5, 10 }
			cylinder { <0,1,0>, <0,0,0>, 0.3, 2 }
		}
		cylinder { <0,1,0>, <0,-0.3,0>, 0.3 }
	}

// Found in the povray socks
#declare Lightbulb = union {
    merge {
      sphere { <0,0,0>,1 }
      cylinder {
        <0,0,1>, <0,0,0>, 1
        scale <0.35, 0.35, 1.0>
        translate  0.5*z
      }
      texture {
        pigment {color rgb <1, 1, 1>}
        finish {ambient .8 diffuse .6}
      }
    }
    cylinder {
      <0,0,1>, <0,0,0>, 1
      scale <0.4, 0.4, 0.5>
      texture { Brass_Texture }
      translate  1.5*z
    }
    rotate -90*x
    scale .5
  }

