%%%-------------------------------------------------------------------
%%% File    : macaddr.erl
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% Description : Cross platform MAC address determination.  
%%% Works for:
%%% * /sbin/ifconfig
%%% * /bin/ifconfig
%%% * ifconfig
%%% * ipconfig /all
%%% 
%%% To return the first MAC address on the system:
%%% 
%%%   macaddr:address()
%%% 
%%% To return an array of all MAC addresses:
%%% 
%%%    macaddr:address_list
%%%
%%% Created : 22 Sep 2008 by  <mmullis@MTC_OFFICE_1>
%%%-------------------------------------------------------------------
-module(macaddr).
-author("michael@mullistechnologies.com").
-export([]).
-compile([export_all]).

-vsn("1.0.0").
%%     ##
%%     # Accessor for the system's first MAC address, requires a call to #address
%%     # first

%%     ##
%%     # Discovers and returns the system's MAC addresses.  Returns the first
%%     # MAC address, and includes an accessor #list for the remaining addresses:
%%     #
%%     #   Mac.addr # => first address
%%     #   Mac.addr.list # => all addresses



%% Why do I have to find this http://www.trapexit.org/String_join_with
%% It should be in the standard library - Seriously frustrating

%% my_string_join(Items, Sep) ->
%%     lists:flatten(lists:reverse(my_string_join1(Items, Sep, []))).

%% my_string_join1([Head | []], _Sep, Acc) ->
%%     [Head | Acc];
%% my_string_join1([Head | Tail], Sep, Acc) ->
%%     my_string_join1(Tail, Sep, [Sep, Head | Acc]).

join([H|T], Sep) ->
    lists:flatten([H | [[Sep, X] || X <- T]]).
%% OR
%% ???
%% lists:merge(lists:flatten(lists:flatmap(fun(X)->[X," "] end, [a,b,c]))).

file_exists(FileName) ->
    filelib:is_regular(FileName).

identify_null_file() ->
    case file_exists("/dev/null") of
        true -> "/dev/null";
        false -> "NUL"
    end.

%% Warning: do not export this because it allows arbitrary os command execution
get_interface_info(Cmd) ->
    %% TODO: os:type() may be more useful here than the original approach
    CmdString = Cmd ++ " 2>" ++ identify_null_file(),
%%    io:format("CmdString: ~p~n", [CmdString]),
    case os:cmd(CmdString) of
        [] -> "";
        {ok, Data} -> Data;
        Data -> Data
    end.

address_list() ->
    LinesLists = lists:map(fun get_interface_info/1, ["/sbin/ifconfig", "/bin/ifconfig", "ifconfig", "ipconfig /all"]),
%%    io:format("LinesLists: ~p~n",[LinesLists]),
    {ok, ALines} = regexp:split(join(LinesLists," "), "[\r\n]"),
    %%raise "all of #{ cmds.join ' ' } failed" unless lines
    Lines = lists:filter(fun(Elem) -> Elem /= [] end, ALines),

%%    io:format("Lines: ~p~n",[Lines]),
    
    MacRegex = "[: ]((?:[0-9A-F][0-9A-F][:\-]){5}[0-9A-F][0-9A-F])",
           
    Candidates0 = lists:foldl(fun(Line, Acc) -> 
                                      case re:run(join(Line,""), MacRegex, [{capture,[1]}]) of
                                          {match, [{Start, Length}]} ->
                                              MacAddress = string:strip(lists:sublist(Line, Start, Length+1)),
                                              %% io:format("MACADDRESS: ~p~n", [MacAddress]),
                                              [MacAddress|Acc];
                                          _ ->
                                              Acc
                                      end
                              end, [], Lines),
    
    %%     case regexp:first_match(Elem, Regexp) of
    %%                                                    {match,Start,Length}
    %%                                                    [Elem|Acc];
    %%                                                    nomatch
    %%                                                    {error,errordesc()}
    
    %%                                                    {match, Matches} ->
    
    %%                                                    {error, ErrorDesc} ->
    %%                                                        Acc
    %%                                                end
    
    Candidates = lists:reverse(lists:filter(fun(Elem) -> Elem /= "00-00-00-00-00-00" end, Candidates0)),
    %%  io:format("Candidates: ~p~n", [Candidates]),
    case length(Candidates) of
        0 -> throw({error, {no_mac_address_candidate, "No Mac Address"}});
        _ -> ok
    end,
    Candidates.

address() ->
    hd(address_list()).

