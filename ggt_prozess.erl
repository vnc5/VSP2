-module(ggt_prozess).
-author("vince").

-import(tools, [log/3]).
-import(werkzeug, [timeMilliSecond/0]).
-import(meinWerkzeug, [lookup/2]).
-include("messages.hrl").
-include("constants.hrl").

-export([start/5]).

start(Name, Nameservice, Coord, Ttw, Ttw) ->
  register_ggt_process(Name, Nameservice, Coord).

register_ggt_process(Name, Nameservice, Coord) ->
  register(Name, self()),
  log(Name, "ggt:locally registering ~p; name is '~p':(~s)~n", [self(), Name, timeMilliSecond()]),
  Nameservice ! {self(), {?REBIND, Name, node()}},
  receive
    {?REBIND_RES, ok} ->
      log(Name, "ggt: ~p bound as service ~p:(~s)~n", [self(), {Name, node()}, timeMilliSecond()]),
      log(Name, "ggt:~p (initial)::~p checkin in to coordinator ~p as service ~p:(~s)~n", [Name, self(), Coord, Name, timeMilliSecond()]),
      Coord ! {?CHECKIN, Name},
      wait_for_neighbours(Name, Nameservice, Coord)
  end.

wait_for_neighbours(Name, Nameservice, Koordinator) ->
  log(Name, "ggt:~p (initial)::waiting for neighbours to be set:(~s)", [Name, timeMilliSecond()]),
  receive
    {?NEIGHBOURS, Left, Right} ->
      LeftNode = lookup(Nameservice, Left),
      RightNode = lookup(Nameservice, Right),
      log(Name, "ggt:~p (initial)::initializing neighbours::l=~p(~p), r=~p(~p):(~s)~n", [Name, Left, LeftNode, Right, RightNode, timeMilliSecond()]),
      log(Name, "ggt:~p (initial)::transition to pre process state: left neighbour=~p, right neigbour=~p:(~s)~n", [Name, Left, Right, timeMilliSecond()]),
      wait_for_first_mi(Name, Nameservice, [LeftNode, Right], Koordinator)
  end.

wait_for_first_mi(Name, Nameservice, NeightbourList, Koordinator) ->
  log(Name, "ggt:~p (pre_process)::waiting for mi to be set:(~s)~n", [Name, timeMilliSecond()]),
  receive
    {?SETPMI, Mi} ->
      log(Name, "ggt:~p (pre_process)::receiving set_pmi ~b:(~s)~n", [Name, Mi, timeMilliSecond()]),
      {ok, Timer} = timer:apply_after(timer:seconds(5), ggt_prozess, starteTerminierungsAbstimmung, [Name, NeightbourList]),
      process(Name, Nameservice, Mi, NeightbourList, Koordinator, now(), Timer);
    {?WHATSON} ->
      whatson(Name, Koordinator, nok),
      wait_for_first_mi(Name, Nameservice, NeightbourList, Koordinator)
  end.

process(Name, Nameservice, Mi, NeightbourList, Coord, StartingTime, Timer) ->
  receive
    {?SETPMI, MiNeu} ->
      timer:cancel(Timer),
      {ok, Timer} = timer:apply_after(timer:seconds(5), ggt_prozess, starteTerminierungsAbstimmung, [Name, NeightbourList]),
      log(Name, "ggt:~p (pre_process)::receiving set_pmi ~b:(~s)~n", [Name, MiNeu, timeMilliSecond()]),
      process(Name, Nameservice, MiNeu, NeightbourList, Coord, StartingTime, Timer);
    {?SEND, Num} ->
      timer:cancel(Timer),
      log(Name, "ggt:~p (pre_process)::receiving send with num ~b:(~s)~n", [Name, Num, timeMilliSecond()]),
      workHard(),
      if Num < Mi ->
        MiNeu = ((Mi - 1) rem Num) + 1,
        log(Name, "ggt:~p Neues Berechnetes Mi ~b:(~s)~n", [Name, MiNeu, timeMilliSecond()]),
        contactNeightbours(Name, NeightbourList, MiNeu),
        Coord ! {?BRIEFME, Name, MiNeu, timeMilliSecond()};
        true ->
          MiNeu = Mi,
          log(Name, "ggt:~p Mi bleibt unverändert. ~b:(~s)~n", [Name, MiNeu, timeMilliSecond()])
      end,
      {ok, Timer} = timer:apply_after(timer:seconds(5), ggt_prozess, starteTerminierungsAbstimmung, [Name, NeightbourList]),
      process(Name, Nameservice, MiNeu, NeightbourList, Coord, StartingTime, Timer);
    ?KILL ->
      log(Name, "ggt:~p (voting)::received kill::starting cleanup:(~s)~n", [Name, timeMilliSecond()]),
      log(Name, "ggt:~p (voting)::globally unbinding ~p with ~p:(~s)~n", [Name, Name, self(), timeMilliSecond()]),
      unregister(Name),
      log(Name, "ggt:~p (voting)::unregistering ~p:(~s)~n", [Name, Name, timeMilliSecond()]),
      Nameservice ! {self(), {?UNBIND, Name}};
    {?WHATSON} ->
      whatson(Name, Coord, ok),
      process(Name, Nameservice, Mi, NeightbourList, Coord, StartingTime, Timer);
    {?TELLMI, From} ->
      From ! {?TELLMI_RES, Mi},
      process(Name, Nameservice, Mi, NeightbourList, Coord, StartingTime, Timer);
    {?VOTE, Initiator} ->
      log(Name, "ggt:~p VOTE empfangen. ~b:(~s)~n", [Name, Mi, timeMilliSecond()]),
      if Initiator /= Name ->
        terminierungsAbstimmung(Name, Initiator, NeightbourList);
        true ->
          Coord ! {?BRIEFTERM, Name, Mi, timeMilliSecond()}
      end,
      process(Name, Nameservice, Mi, NeightbourList, Coord, StartingTime, Timer)
  end.

contactNeightbours(_, [], _) -> ok;
contactNeightbours(Name, NeightbourList, Mi) ->
  [Proc, Tail] = NeightbourList,
  Proc ! {?SEND, Mi},
  log(Name, "state(ready) setting initial mi=~b for ~p:(~s)~n", [Mi, Proc, timeMilliSecond()]),
  contactNeightbours(Name, Tail, Mi).

starteTerminierungsAbstimmung(Name, NeightbourList) ->
  terminierungsAbstimmung(Name, Name, NeightbourList).

terminierungsAbstimmung(Name, Initiator, NeightbourList) ->
  log(Name, "ggt:~p::ggT starteTerminierungsAbstimmung:(~s)~n", [Name, timeMilliSecond()]),
  {Left, _} = NeightbourList,
  Left ! {?VOTE, Initiator}.

workHard() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  timer:sleep(timer:seconds(random:uniform())).

whatson(Name, Koordinator, Status) ->
  log(Name, "ggt:~p::ggT WhatsOnAbfrage erhalten:(~s)~n", [Name, timeMilliSecond()]),
  Koordinator ! {?WHATSON_RES, Status}.
