-module(starter).
-author("vince").

-import(tools, [log/3]).
-import(werkzeug, [timeMilliSecond/0]).
-import(meinWerkzeug, [read_config/2]).
-include("messages.hrl").
-include("constants.hrl").

-export([start/2]).

start(NameserviceNode, StarterNumber) ->
  {NameserviceNodeName, Koordinator, Praktikumsgruppe, Team} = read_config([nameservice, koordinator, praktikumsgruppe, team], "koordinator.cfg"),
  net_adm:ping(NameserviceNode),
  Nameservice = global:whereis_name(NameserviceNodeName),
  LogName = io_lib:format("starter~b_ggt", [StarterNumber]),
  log(LogName, "looked up NS and coord~n", []),
  Nameservice ! {?GGTVALS, self()},
  receive
    {?GGTVALS_RES, Ttw, Ttt, Ggts} ->
      log(LogName, "Retrieved configuration TTW=~b TTT=~b GGTProcs=~b from coord:(~s)~n", [Ttw, Ttt, Ggts, timeMilliSecond()]),
      start_ggt_procs(LogName, Praktikumsgruppe, Team, StarterNumber, Ttw, Ttt, Ggts)
  end.

start_ggt_procs(_, _, _, _, _, _, 0) -> ok;
start_ggt_procs(LogName, Praktikumsgruppe, Team, StarterNumber, Ttw, Ttt, Count) ->
  Name = list_to_atom(io_lib:format("~b~b~b_~b", [Praktikumsgruppe, Team, Count, StarterNumber])),
  log(LogName, "Starting ggt proc '~s' with ttw ~b(s) ttt ~b(s):(~s)~n", [Name, Ttw, Ttt, timeMilliSecond()]),
  spawn(ggt_prozess, start, [Name, Ttw, Ttt, Count]).
