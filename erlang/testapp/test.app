{application, test,
	[{description, "Test failover app."},
	 {vsn, "1.0"},
	 {modules, [ test, test_sup, test_proc ]},
	 {registered, [test_proc ]},
	 {applications, [kernel,stdlib]},
	 {mod, {test, []}}
	]}.
