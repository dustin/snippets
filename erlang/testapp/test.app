% arch-tag: B7C5B1A5-A07D-11D8-8FDC-000393CFE6B8

{application, test,
	[{description, "Test failover app."},
	 {vsn, "1.0"},
	 {modules, [ test, test_sup, test_proc ]},
	 {registered, [test_proc ]},
	 {applications, [kernel,stdlib]},
	 {mod, {test, []}}
	]}.
