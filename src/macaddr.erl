%%%-------------------------------------------------------------------
%%% File    : macaddr.erl
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% @copyright 2008 Mullis Technologies Corporation
%%% @doc Cross platform MAC address determination.
%%%
%%% Inspired by the macaddr gem for ruby
%%% (<a href="http://rubyforge.org/projects/codeforpeople">http://rubyforge.org/projects/codeforpeople</a>)
%%%
%%% Tested on Linux (x86_64), Windows Vista, and MacOSX(10.5.4)
%%%
%%% Works for:  <br/>
%%% * /sbin/ifconfig  <br/>
%%% * /bin/ifconfig  <br/>
%%% * ifconfig  <br/>
%%% * ipconfig /all  <br/>
%%% <br/> <pre>
%%% To return the first MAC address on the system:
%%%   macaddr:address()
%%% <br/>
%%% To return an array of all MAC addresses:
%%%    macaddr:address_list
%%% </pre>
%%% @end

%%% Created : 22 Sep 2008 by Michael Mullis <michael@mullistechnologies.com>

%%% @end
%%%-------------------------------------------------------------------
-module(macaddr).
-author("michael@mullistechnologies.com").
-vsn("0.1.0").

-export([address/0, address_list/0]).
-export([mac_matcher/2]).

file_exists(FileName) ->
  case filelib:is_regular(FileName) of
    true ->
      true;
    %% Even if its not a regular file, it might still exist
    %% /dev/null exhibits this behavior
    false ->
      case filelib:last_modified(FileName) of
        0 ->
          false;
        _ ->
          true
      end
  end.

identify_null_file() ->
    case file_exists("/dev/null") of
        true -> "/dev/null";
        false -> "NUL"
    end.

%% Warning: do not export this because it allows arbitrary os command execution
get_interface_info(Cmd) ->
    %% @TODO os:type() may be more useful here than the original approach
    CmdString = Cmd ++ " 2>" ++ identify_null_file(),
    case os:cmd(CmdString) of
        [] -> "";
        {ok, Data} -> Data;
        Data -> Data
    end.

%%% @doc Exported for testability.   Internal use only.
mac_matcher(Line, Acc) ->
  MACRegex = " (?<FOO>([0-9A-F][0-9A-F][:\\-]){5}[0-9A-F][0-9A-F])([^:\\-0-9A-F]|$)",
  case re:run(Line, MACRegex, [caseless, {capture,['FOO']}]) of
    {match, [{Start, Length}]} ->
      MACAddress = string:strip(lists:sublist(Line, Start, Length+1)),
      {ok, StdMACAddress, _} =  regexp:gsub(MACAddress, "-", ":"),
      [StdMACAddress|Acc];
    _ ->
      Acc
  end.

%%% @doc Retrieve list of MAC addresses for machine
%%% @spec address_list() -> List
address_list() ->
  LinesLists = lists:map(fun get_interface_info/1, ["/sbin/ifconfig", "/bin/ifconfig", "ifconfig", "ipconfig /all"]),
  {ok, ALines} = regexp:split(string:join(LinesLists," "), "[\r\n]"),
  Lines = lists:filter(fun(Elem) ->  Elem /= []  end, ALines),

  Candidates0 = lists:foldl(fun mac_matcher/2, [], Lines),

  %% Length check to avoid some false hits from the regex because re module does not seem to support more complex regex to handle it
  Candidates = lists:reverse(lists:filter(fun(Elem) ->  (Elem /= "00:00:00:00:00:00" andalso
                                                         Elem /= "00-00-00-00-00-00" andalso
                                                         length(Elem) =< 17 ) end, Candidates0)),
  case length(Candidates) of
    0 -> throw({error, {no_mac_address_candidate, "No MAC Address"}});
    _ -> ok
  end,
  lists:usort(Candidates).  % remove duplicates

%%% @doc Retrieve the first MAC addresses for machine
%%% @spec address() -> string()
address() ->
    hd(address_list()).
