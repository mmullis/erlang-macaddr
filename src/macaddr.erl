%%%-------------------------------------------------------------------
%%% File    : macaddr.erl
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% @doc Cross platform MAC address determination.
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

%%% @copyright 2008 Michael Mullis
%%% need this to keep the next line from being added to doc
%%% @end
%%%-------------------------------------------------------------------
-module(macaddr).
-author("michael@mullistechnologies.com").
-vsn("0.1.0").

-export([address/0, address_list/0]).

%%% @doc Join elements of a list using separator.
join([H|T], Sep) ->
    lists:flatten([H | [[Sep, X] || X <- T]]).

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

%%% @spec address_list() -> List
address_list() ->
  LinesLists = lists:map(fun get_interface_info/1, ["/sbin/ifconfig", "/bin/ifconfig", "ifconfig", "ipconfig /all"]),
  {ok, ALines} = regexp:split(join(LinesLists," "), "[\r\n]"),
  Lines = lists:filter(fun(Elem) ->  Elem /= []  end, ALines),

  %%  MACRegex = "[: ]((?:[0-9a-fA-F][0-9a-fA-F][:\-]){5}[0-9a-fA-F][0-9a-fA-F])",
  %%  MACRegex = "\s((?:[0-9a-fA-F]{2}[:\-]){5}[0-9a-fA-F]{2}[^0-9a-fA-F:\-])",
  %%  MACRegex = "^|\s((?:[0-9a-fA-F]{2}[:\-]){5}[0-9a-fA-F]{2}[^0-9a-fA-F:\-])\s|$",
  MACRegex = "((?:[0-9a-fA-F]{2}[:\-]){5}[0-9a-fA-F]{2})",
  Candidates0 = lists:foldl(
                  fun(Line, Acc) ->
                      case re:run(join(Line, ""), MACRegex, [{capture,[1]}]) of
                        {match, [{Start, Length}]} ->
                          MACAddress = string:strip(lists:sublist(Line, Start, Length+1)),
                          {ok, StdMACAddress, _} =  regexp:gsub(MACAddress, "-", ":"),
                          [StdMACAddress|Acc];
                        _ ->
                          Acc
                      end
                  end, [], Lines),

  %% Length check to avoid some false hits from the regex because re module does not seem to support more complex regex to handle it
  Candidates = lists:reverse(lists:filter(fun(Elem) ->  (Elem /= "00:00:00:00:00:00" andalso
                                                         Elem /= "00-00-00-00-00-00" andalso
                                                         length(Elem) =< 17 ) end, Candidates0)),
  case length(Candidates) of
    0 -> throw({error, {no_mac_address_candidate, "No MAC Address"}});
    _ -> ok
  end,
  lists:usort(Candidates).  % remove duplicates

%%% @spec address() -> string()
address() ->
    hd(address_list()).
