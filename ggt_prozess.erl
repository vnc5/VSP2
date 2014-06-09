-module(ggt_prozess).
-author("vince").

-import(timer, [send_after/2, sleep/1, cancel/1]).
-import(tools, [log/3]).
-import(werkzeug, [timeMilliSecond/0, reset_timer/3]).
-import(meinWerkzeug, [lookup/2, timestamp_micro/0]).
-include("messages.hrl").
-include("constants.hrl").

-export([start/5]).

start(Name, Nameservice, Coord, Ttw, Ttt) ->
  register_ggt_process(Name, Nameservice, Coord, Ttw, Ttt).

register_ggt_process(Name, Nameservice, Coord, Ttw, Ttt) ->
  log(Name, "ggt:locally registering ~p; name is '~p':(~s)~n", [self(), Name, timeMilliSecond()]),
  register(Name, self()),
  Nameservice ! {self(), {?REBIND, Name, node()}},
  receive
    {?REBIND_RES, ok} ->
      log(Name, "ggt: ~p bound as service ~p:(~s)~n", [self(), {Name, node()}, timeMilliSecond()]),
      log(Name, "ggt:~p (initial)::~p checkin in to coordinator ~p as service ~p:(~s)~n", [Name, self(), Coord, Name, timeMilliSecond()]),
      Coord ! {?CHECKIN, Name},
      wait_for_neighbours(Name, Nameservice, Coord, Ttw, Ttt)
  end.

wait_for_neighbours(Name, Nameservice, Koordinator, Ttw, Ttt) ->
  log(Name, "ggt:~p (initial)::waiting for neighbours to be set:(~s)", [Name, timeMilliSecond()]),
  receive
    {?NEIGHBOURS, Left, Right} ->
      LeftNode = lookup(Nameservice, Left),
      RightNode = lookup(Nameservice, Right),
      log(Name, "ggt:~p (initial)::initializing neighbours::l=~p(~p), r=~p(~p):(~s)~n", [Name, Left, LeftNode, Right, RightNode, timeMilliSecond()]),
      log(Name, "ggt:~p (initial)::transition to pre process state: left neighbour=~p, right neigbour=~p:(~s)~n", [Name, Left, Right, timeMilliSecond()]),
      wait_for_first_mi(Name, Nameservice, LeftNode, RightNode, Koordinator, Ttw, Ttt)
  end.

wait_for_first_mi(Name, Nameservice, Left, Right, Koordinator, Ttw, Ttt) ->
  log(Name, "ggt:~p (pre_process)::waiting for mi to be set:(~s)~n", [Name, timeMilliSecond()]),
  receive
    {?SETPMI, Mi} ->
      log(Name, "ggt:~p (pre_process)::receiving set_pmi ~b:(~s)~n", [Name, Mi, timeMilliSecond()]),
      Timer = reset_timer(Name, none, Ttt, terminate),
      process(Name, Nameservice, Mi, Left, Right, Koordinator, Timer, 2, Ttw, Ttt)
  end.

process(Name, Nameservice, Mi, Left, Right, Coord, Timer, LastMi, Ttw, Ttt) ->
  receive
    {?SETPMI, MiNeu} ->
      NewTimer = reset_timer(Name, Timer, Ttt, terminate),
      log(Name, "ggt:~p (pre_process)::receiving set_pmi ~b:(~s)~n", [Name, MiNeu, timeMilliSecond()]),
      process(Name, Nameservice, MiNeu, Left, Right, Coord, NewTimer, timestamp_micro(), Ttw, Ttt);
    {?SEND, Y} ->
      log(Name, "ggt:~p (voting)::receiving send y=~b:(~s)~n", [Name, Y, timeMilliSecond()]),
      cancel(Timer),
      NewMi = calc_ggt(Name, Mi, Y, Ttw),
      if
        Mi == NewMi ->
          NewTimer = reset_timer(Name, none, Ttt, terminate);
        true ->
          log(Name, "ggt:~p (processing)::calculated new mi::mi=~b::sending ~b to ~p:(~s)~n", [Name, NewMi, NewMi, Coord, timeMilliSecond()]),
          Coord ! {?BRIEFME, Name, NewMi, timeMilliSecond()},
          NewTimer = reset_timer(Name, none, Ttt, terminate),
          send_mi_to_neighbours(Name, NewMi, Left, Right)
      end,
      process(Name, Nameservice, NewMi, Left, Right, Coord, NewTimer, timestamp_micro(), Ttw, Ttt);
    ?KILL ->
      log(Name, "ggt:~p (voting)::received kill::starting cleanup:(~s)~n", [Name, timeMilliSecond()]),
      log(Name, "ggt:~p (voting)::globally unbinding ~p with ~p:(~s)~n", [Name, Name, self(), timeMilliSecond()]),
      unregister(Name),
      log(Name, "ggt:~p (voting)::unregistering ~p:(~s)~n", [Name, Name, timeMilliSecond()]),
      Nameservice ! {self(), {?UNBIND, Name}};
    ?WHATSON ->
      whatson(Name, Coord, ok),
      process(Name, Nameservice, Mi, Left, Right, Coord, Timer, LastMi, Ttw, Ttt);
    {?TELLMI, From} ->
      From ! {?TELLMI_RES, Mi},
      process(Name, Nameservice, Mi, Left, Right, Coord, Timer, LastMi, Ttw, Ttt);
    {?VOTE, Initiator} ->
      % "Erhält ein initiierender Prozess von seinem rechten Nachbarn die Anfrage nach der Terminierung (vote), meldet er die Terminierung dem Koordinator."
      % Kann mit dieser Schnittstelle nicht sichergestellt werden, ob die Nachricht vom rechten Nachbarn kam.
      if
        Initiator /= Name ->
          Diff = timestamp_micro() - LastMi,
          if
            Diff > (Ttt / 2) * 1000000 ->
              {LeftName, _} = Left,
              log(Name, "ggt:~p (voting)::accept vote from ~p::routing to ~p::time since last mi event ~f(s):(~s)~n", [Name, Initiator, LeftName, Diff, timeMilliSecond()]),
              Left ! {?VOTE, Initiator};
            true ->
              log(Name, "ggt:~p (processing)::reject vote from ~p::time since last mi event ~f(micro) :(~s)~n", [Name, Initiator, Diff, timeMilliSecond()])
          end;
        true ->
          Coord ! {?BRIEFTERM, {Name, Mi, timeMilliSecond()}, self()}
      end,
      process(Name, Nameservice, Mi, Left, Right, Coord, Timer, LastMi, Ttw, Ttt);
    terminate ->
      log(Name, "ggt:~p (processing)::starting vote:: mi is ~b:(~s)~n", [Name, Mi, timeMilliSecond()]),
      Left ! {?VOTE, Name},
      process(Name, Nameservice, Mi, Left, Right, Coord, Timer, LastMi, Ttw, Ttt)
  end.

send_mi_to_neighbours(Name, Mi, Left, Right) ->
  {LeftName, _} = Left,
  {RightName, _} = Right,
  log(Name, "ggt:~p (processing)::sending mi=~b to neighbours:: l=~p,r=~p:(~s)~n", [Name, Mi, LeftName, RightName, timeMilliSecond()]),
  Left ! {?SEND, Mi},
  Right ! {?SEND, Mi}.

calc_ggt(Name, Mi, Y, Ttw) ->
  if
    Y < Mi ->
      log(Name, "ggt:~p (processing)::calculation start:: calculating new ggt:: mi=~b, y=~b::duration ~b(s):(~s)~n", [Name, Mi, Y, Ttw]),
      sleep(Ttw * 1000),
      NewMi = ((Mi - 1) rem Y) + 1,
      log(Name, "ggt:~p (processing)::calculation done:: changes mi=~b to ~b:(~s)~n", [Name, Mi, NewMi, timeMilliSecond()]);
    true ->
      NewMi = Mi,
      log(Name, "ggt:~p (processing)::no ggt calculation reason:: ~b>=~b:(~s)~n", [Name, Y, Mi, timeMilliSecond()])
  end,
  NewMi.

reset_timer(Name, Timer, Ttt, Message) ->
  if
    Timer =:= none ->
      NewTimer = send_after(Ttt * 1000, Message);
    true ->
      NewTimer = Timer
  end,
  log(Name, "ggt:~p (processing)::mi event:: storing event time=~s:(~s)~n", [Name, timeMilliSecond(), timeMilliSecond()]),
  log(Name, "ggt:~p (processing)::mi event:: (re)setting timer:(~s)~n", [Name, timeMilliSecond(), timeMilliSecond()]),
  reset_timer(Timer, Ttt, Message).

whatson(Name, Koordinator, Status) ->
  log(Name, "ggt:~p::ggT WhatsOnAbfrage erhalten:(~s)~n", [Name, timeMilliSecond()]),
  Koordinator ! {?WHATSON_RES, Status}.
