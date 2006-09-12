-- Some homework Jennalynn had that required identifying a sequence.

i 0 = 0
i x = x + i (x - 1)

f x = 20 + i x

s = [ f x | x <- [0..] ]
