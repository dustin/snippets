#declare CBR =
   union {
      torus {
         4.4, .4
      }
      sphere {
	 <4.4, 0, 0>, 1
      }
}

#declare Barbell =
   union {
      sphere { <0, 3.5, 0>, 1.5 }
      cylinder { <0, 3.5, 0>, <0, -3.5, 0>, .7 }
      sphere { <0, -3.5, 0>, 1.5 }
}

#declare RoundBar =
   union {
      difference {
         torus { 4.4, .4 }
	 box { <-3, 1, 3>, <4, -1, 5> }
      }
      sphere { <-3, 0, 3.22>, 1.3 }
      sphere { <3, 0, 3.22>, 1.3 }
}
