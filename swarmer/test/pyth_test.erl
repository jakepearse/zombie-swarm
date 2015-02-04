-module(pyth_test).
-include_lib("eunit/include/eunit.hrl").

pyth_test_() ->
	{
		foreach,
		fun setup/0,
		fun teardown/1,
		[{"math_test", fun math_test/0}]
	}.

setup() ->
	[].

teardown(_Thing) ->
	error_logger:error_report(_Thing).

math_test() ->
	A = pythagoras:pyth(1,1,2,2),
	B = pythagoras:pyth(1,1,4,5),
	C = pythagoras:pyth(-1,-4,6,6),
	?_assertEqual(A, 1.4142135623730951),
	?_assertEqual(B, 5.0),
	?_assertEqual(C, 12.206555615733702).