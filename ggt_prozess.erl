-module(ggt_prozess).
-author("vince").

-import(tools, [log/3]).
-import(werkzeug, [timeMilliSecond/0]).
-include("messages.hrl").
-include("constants.hrl").

-export([start/4]).

start(Name, Ttw, Ttw, Number) ->
  net_adm:ping(asdf),
  Nameservice = global:whereis_name(asdf),
  register_ggt_process(Name, Nameservice).

register_ggt_process(Name, Nameservice) ->
  register(Name, self()),
  log(Name, "ggt:locally registering ~p; name is '~p':(~s)~n", [self(), Name, timeMilliSecond()]),
  Nameservice ! {self(), {?REBIND, Name, node()}},
  receive
    {?REBIND_RES, ok} ->
      log(Name, "ggt: ~p bound as service ~p:(~s)~n", [self(), {Name, node()}, timeMilliSecond()]),
      coord ! {?CHECKIN, Name},
      log(Name, "ggt:~p (initial)::~p checkin in to coordinator {coord,coord@Mine} as service ~p:(~s)~n", [Name, self(), Name, timeMilliSecond()]),
      wait_for_neighbours(Name, Nameservice)
  end.

wait_for_neighbours(Name, Nameservice) ->
  log(Name, "ggt:~p (initial)::waiting for neighbours to be set:(~s)", [Name, timeMilliSecond()]),
  receive
    {?NEIGHBOURS, Left, Right} ->
      LeftNode = lookup(Nameservice, Left),
      RightNode = lookup(Nameservice, Right),
      log(Name, "ggt:~p (initial)::initializing neighbours::l=~p(~p), r=~p(~p):(~s)~n", [Name, Left, LeftNode, Right, RightNode, timeMilliSecond()]),
      log(Name, "ggt:~p (initial)::transition to pre process state: left neighbour=~p, right neigbour=~p:(~s)~n", [Name, Left, Right, timeMilliSecond()]),
      wait_for_first_mi(Name)
  end.

lookup(Nameservice, GgtName) ->
  Nameservice ! {self(), {?LOOKUP, GgtName}},
  receive
    {?REBIND_RES, ServiceAtNode} ->
      ServiceAtNode
  end.

wait_for_first_mi(Name) ->
  log(Name, "ggt:~p (pre_process)::waiting for mi to be set:(~s)~n", [Name, timeMilliSecond()]),
  receive
    {?SETPMI, Mi} ->
      log(Name, "ggt:~p (pre_process)::receiving set_pmi ~b:(~s)~n", [Name, Mi, timeMilliSecond()]),
      process(Name, Mi)
  end.

process(Name, Mi) ->
  receive
    {?SETPMI, MiNeu} ->
      log(Name, "ggt:~p (pre_process)::receiving set_pmi ~b:(~s)~n", [Name, MiNeu, timeMilliSecond()]),
      process(Name, MiNeu);
    {?SEND, Num} ->
      k
  end.
