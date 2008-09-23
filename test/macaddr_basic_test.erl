%%%-------------------------------------------------------------------
%%% File    : macaddr_basic_test.erl
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% Description : EUnit tests for Macaddr module
%%% Created : 22 Sep 2008 by Michael Mullis <michael@mullistechnologies.com>
%%%-------------------------------------------------------------------
-module(macaddr_basic_test).
-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    [?_assert(length(macaddr:address()) > 0),
     ?_assert(length(macaddr:address_list()) > 0),
     ?_assert(length(macaddr:address()) /= "00-00-00-00-00-00")
    ].

matching_test_() ->
  [?_assertEqual(["99:99:99:99:99:99"], macaddr:mac_matcher("   addr 99:99:99:99:99:99", [])),
   ?_assertEqual([], macaddr:mac_matcher("   notaddr  99:99:99:99:99:99:99:99:99", []))
  ].
