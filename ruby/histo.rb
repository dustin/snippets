#!/usr/bin/env ruby

class Hash

	# Create a new +Hash+ that's a histogram of the elements in the given
	# enumerable.
	def Hash.histo(e)
		e.inject(Hash.new(0)) { |c, x| c[x] += 1 ; c}
	end

end
