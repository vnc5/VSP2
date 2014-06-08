-module(ggt_prozess).
-author("vince").

-import(tools, [log/3]).
-import(werkzeug, [timeMilliSecond/0]).
-include("messages.hrl").
-include("constants.hrl").

-export([start/5]).

start(Name, Ttw, Ttw, Number, KoordinatorName) ->
  net_adm:ping(asdf),
  Nameservice = global:whereis_name(asdf),
  Koordinator = global:whereis_name(KoordinatorName),
  register_ggt_process(Name, Nameservice, Koordinator).

register_ggt_process(Name, Nameservice, Koordinator) ->
  register(Name, self()),
  log(Name, "ggt:locally registering ~p; name is '~p':(~s)~n", [self(), Name, timeMilliSecond()]),
  Nameservice ! {self(), {?REBIND, Name, node()}},
  receive
    {?REBIND_RES, ok} ->
      log(Name, "ggt: ~p bound as service ~p:(~s)~n", [self(), {Name, node()}, timeMilliSecond()]),
      coord ! {?CHECKIN, Name},
      log(Name, "ggt:~p (initial)::~p checkin in to coordinator {coord,coord@Mine} as service ~p:(~s)~n", [Name, self(), Name, timeMilliSecond()]),
      wait_for_neighbours(Name, Nameservice, Koordinator)
  end.

wait_for_neighbours(Name, Nameservice, Koordinator) ->
  log(Name, "ggt:~p (initial)::waiting for neighbours to be set:(~s)", [Name, timeMilliSecond()]),
  receive
    {?NEIGHBOURS, Left, Right} ->
      LeftNode = lookup(Nameservice, Left),
      RightNode = lookup(Nameservice, Right),
      log(Name, "ggt:~p (initial)::initializing neighbours::l=~p(~p), r=~p(~p):(~s)~n", [Name, Left, LeftNode, Right, RightNode, timeMilliSecond()]),
      log(Name, "ggt:~p (initial)::transition to pre process state: left neighbour=~p, right neigbour=~p:(~s)~n", [Name, Left, Right, timeMilliSecond()]),
      wait_for_first_mi(Name, [LeftNode, Right], Koordinator)
  end.

lookup(Nameservice, GgtName) ->
  Nameservice ! {self(), {?LOOKUP, GgtName}},
  receive
    {?REBIND_RES, ServiceAtNode} ->
      ServiceAtNode
  end.

wait_for_first_mi(Name, NeightbourList, Koordinator) ->
  log(Name, "ggt:~p (pre_process)::waiting for mi to be set:(~s)~n", [Name, timeMilliSecond()]),
  receive
    {?SETPMI, Mi} ->
      log(Name, "ggt:~p (pre_process)::receiving set_pmi ~b:(~s)~n", [Name, Mi, timeMilliSecond()]),
      {ok,Timer} = timer:apply_after(timer:seconds(5), starteTerminierungsAbstimmung, [Name, NeightbourList]),
      process(Name, Mi, NeightbourList, Koordinator, now(), Timer)
  end.

process(Name, Mi, NeightbourList, Koordinator, StartingTime, Timer) ->
  receive
    {?SETPMI, MiNeu} ->
      timer:cancel(Timer),
      {ok,Timer} = timer:apply_after(timer:seconds(5), starteTerminierungsAbstimmung, [Name, NeightbourList]),
      log(Name, "ggt:~p (pre_process)::receiving set_pmi ~b:(~s)~n", [Name, MiNeu, timeMilliSecond()]),
      process(Name, MiNeu, NeightbourList, Koordinator, StartingTime, Timer);
    {?SEND, Num} ->
      timer:cancel(Timer),
      log(Name, "ggt:~p (pre_process)::receiving SEND with num ~b:(~s)~n", [Name, Num, timeMilliSecond()]),
      workHard(),
      if Num < Mi ->
        MiNeu = ((Mi-1) rem Num) + 1,
        log(Name, "ggt:~p Neues Berechnetes Mi ~b:(~s)~n", [Name, MiNeu, timeMilliSecond()]),
        contactNeightbours(Name,NeightbourList, MiNeu),
        Koordinator ! {?BRIEFME, Name, MiNeu, timeMilliSecond()}
      ;
      true ->
        MiNeu = Mi,
        log(Name, "ggt:~p Mi bleibt unverändert. ~b:(~s)~n", [Name, MiNeu, timeMilliSecond()])
      end,
      {ok,Timer} = timer:apply_after(timer:seconds(5), starteTerminierungsAbstimmung, [Name, NeightbourList]),
      process(Name, MiNeu, NeightbourList, Koordinator, StartingTime, Timer);
    {?KILL} ->
      terminate(Name, timeMilliSecond());
    {?VOTE, Initiator} ->
      log(Name, "ggt:~p VOTE empfangen. ~b:(~s)~n", [Name, Mi, timeMilliSecond()]),
      if Initiator /= Name ->
        terminierungsAbstimmung(Name, Initiator, NeightbourList);
      true ->
        Koordinator ! {?BRIEFTERM, Name, Mi, timeMilliSecond()}
      end,
      process(Name, Mi, NeightbourList, Koordinator, StartingTime, Timer)
  end
.

contactNeightbours(_,[],_) -> ok;
contactNeightbours(Name ,NeightbourList, Mi) ->
  [Proc, Tail] = NeightbourList,
    Proc ! {?SEND, Mi},
  log(Name, "state(ready) setting initial mi=~b for ~p:(~s)~n", [Mi, Proc, timeMilliSecond()]),
  contactNeightbours(Name,Tail, Mi)
.

terminate(Name, Time) ->
  log(Name, "ggt:~p::ggT is terminated:(~s)~n", [Name, Time])
.

starteTerminierungsAbstimmung(Name,NeightbourList) ->
  terminierungsAbstimmung(Name, Name, NeightbourList)
.

terminierungsAbstimmung(Name,Initiator,NeightbourList) ->
  log(Name, "ggt:~p::ggT starteTerminierungsAbstimmung:(~s)~n", [Name, timeMilliSecond()]),
  {Left, _} = NeightbourList,
  Left ! {?VOTE,Initiator}
.

workHard() ->
  {A1,A2,A3} = now(),
  random:seed(A1, A2, A3),
  timer:sleep(timer:seconds(random:uniform()))
.

