-module(gleam_should_assertions).
-include_lib("eunit/include/eunit.hrl").

-export([should_equal/2, should_not_equal/2, should_be_ok/1,
         should_be_error/1]).

should_equal(Actual, Expected) -> 
    ?assertEqual(Expected, Actual),
    nil.
should_not_equal(Actual, Expected) -> 
    ?assertNotEqual(Expected, Actual),
    nil.
should_be_ok(A) -> 
    ?assertMatch({ok, _}, A),
    nil.
should_be_error(A) -> 
    ?assertMatch({error, _}, A),
    nil.
