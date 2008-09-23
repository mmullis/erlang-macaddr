%%%-------------------------------------------------------------------
%%% File    : macaddr_basic_test.erl
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% Description : EUnit tests for Macaddr module
%%% Created : 22 Sep 2008 by Michael Mullis <michael@mullistechnologies.com>
%%%-------------------------------------------------------------------
-module(macaddr_tests).
-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    [{generator, macaddr_basic_test, basic_test_},
    {generator, macaddr_basic_test, matching_test_}].
