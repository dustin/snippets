(* Time parsing *)
(* arch-tag: 51B91D7A-05DC-11D8-976E-000393DC8AE4 *)

module Time =

	struct

	class time =
		object (self)
			val mutable year = 0
			val mutable month = 0
			val mutable day = 0
			val mutable hour = 0
			val mutable minute = 0
			val mutable second = 0

			method getYear = year
			method getMonth = month
			method getDay = day
			method getHour = hour
			method getMinute = minute
			method getSecond = second

			method setYear y = year <- y
			method setMonth m = month <- m
			method setDay d = day <- d
			method setHour h = hour <- h
			method setMinute m = minute <- m
			method setSecond s = second <- s
	end;;

end;;
