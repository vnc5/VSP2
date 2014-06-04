-module(koordinator).
-author("vince").

-import(tools, [log/3]).
-import(werkzeug, [timeMilliSecond/0, bestimme_mis/2, list2String/1]).
-import(meinWerkzeug, [read_config/2, shuffle/1]).
-include("messages.hrl").
-include("constants.hrl").

-export([start/1]).

start(NameserviceNode) ->
  {Name, NameserviceNodeName, Rt, GgtCount, Ttw, Ttt} = read_config([name, nameservice, rt, ggtcount, ttw, ttt], "koordinator.cfg"),
  net_adm:ping(NameserviceNode),
  Nameservice = global:whereis_name(NameserviceNodeName),
  register_koordinator(Name, Nameservice, Rt, GgtCount, Ttw, Ttt).

register_koordinator(Name, Nameservice, Rt, GgtCount, Ttw, Ttt) ->
  Nameservice ! {self(), {?REBIND, meindienst, node()}},
  receive
    {?REBIND_RES, ok} ->
      log(Name, "state('pre init') seccessfully bound  with NS nameservice (remote refNS= ):(~s)~n", [timeMilliSecond()]),
      send_after(Rt * 1000, self(), ?STEP),
      receive_register_requests(Name, GgtCount, Ttw, Ttt, [])
  end.

receive_register_requests(Name, GgtCount, Ttw, Ttt, GgtProcs) ->
  receive
    {?GGTVALS, Pid} ->
      log(Name, "state(register) sending ttw/ttt/num_ggt_procs=(~b/~b/~b) to '~p':(~s)~n", [Ttw, Ttt, GgtCount, Pid, timeMilliSecond()]),
      Pid ! {?GGTVALS_RES, Ttw, Ttt, GgtCount},
      receive_register_requests(Name, GgtCount, Ttw, Ttt, GgtProcs);
    {?CHECKIN, GgtName} ->
      log(Name, "state(register) '~s' checked in:(~s)~n", [GgtName, timeMilliSecond()]),
      % Woher soll man wissen wieviele Starter verbinden?
      if
        length(GgtProcs) + 1 == GgtCount ->
          log(Name, "state(register) finished register phase: all ggt procs checked in:(~s)~n", [timeMilliSecond()]);
        true -> ok
      end,
      receive_register_requests(Name, GgtCount, Ttw, Ttt, [GgtName | GgtProcs]);
    ?STEP ->
      log(Name, "state(init) received 'step' preparing transition to 'ready' state :(~s)~n", [timeMilliSecond()]),
      create_ggt_ring(Name, GgtProcs),
      % ??? "Starten einer Berechnung über die Nachricht {calc target}"
      send_after(10000, self(), {?CALCSTART, 5}),
      ready_state_loop(Name, GgtProcs, GgtCount, Ttw, Ttt, GgtProcs)
  end.

create_ggt_ring(Name, GgtProcs) ->
  log(Name, "state(init) start creating ggt ring:(~s)~n", [timeMilliSecond()]),
  ShuffledGgtProcs = shuffle(GgtProcs),
  create_ggt_ring(Name, ShuffledGgtProcs, length(ShuffledGgtProcs)).
create_ggt_ring(_, _, 0) ->
  ok;
create_ggt_ring(Name, GgtProcs, Index) ->
  if
    Index - 1 ->
      PreviousIndex = length(GgtProcs);
    true ->
      PreviousIndex = Index
  end,
  Left = lists:nth(PreviousIndex, GgtProcs),
  Current = lists:nth(Index, GgtProcs),
  Right = lists:nth((Index + 1) rem length(GgtProcs), GgtProcs),
  log(Name, "state(init) sending neighbours (l,r)=(~s, ~s) to {~p}:(~s)~n", [Left, Right, Current, timeMilliSecond()]),
%%   Current ! {?NEIGHBOURS, Left, Right},
  create_ggt_ring(Name, GgtProcs, Index - 1).

ready_state_loop(Name, GgtProcs, GgtCount, Ttw, Ttt, GgtProcs) ->
  receive
    {?CALCSTART, Target} ->
      log(Name, "state(ready) starting new calculation with target ~b:(~s)~n", [Target, timeMilliSecond()]),
      Mis = bestimme_mis(target, length(GgtProcs)),
      log(Name, "state(ready) calculated mis:\"~s\":(~s)~n", [list2String(Mis), timeMilliSecond()]),
      set_start_values(Name, GgtProcs, Mis),
      trigger_calculation(Name, shuffle(GgtProcs), Target, max(round(length(GgtProcs) * 0.15), 2)),
      ready_state_loop(Name, GgtProcs, GgtCount, Ttw, Ttt, GgtProcs);
    {?BRIEFME, {GgtName, GgtMi, GgtTime}} ->
      log(Name, "state(ready) ggt '~s' sent new mi=~b (cTime=\"~s\"):(~s)~n", [GgtName, GgtMi, GgtTime, timeMilliSecond()]),
      ready_state_loop(Name, GgtProcs, GgtCount, Ttw, Ttt, GgtProcs);
    {?BRIEFTERM, {GgtName, GgtMi, GgtTime}, From} ->
      log(Name, "state(ready) ggt '~s' sent termination with ggt=~b (cTime=\"~s\"):(~s)~n", [GgtName, GgtMi, GgtTime, timeMilliSecond()]),
      if
        GgtMi > 5 ->
          log(Name, "state(ready) ggt '~s' sent wrong termination with ggt=~b (cTime=\"~s\"):(~s)~n", [GgtName, GgtMi, GgtTime, timeMilliSecond()]),
          if
            true ->
              From ! {?SEND, 5};
            true ->
              ok
          end;
        true -> ok
      end,
      ready_state_loop(Name, GgtProcs, GgtCount, Ttw, Ttt, GgtProcs);
    ?TOGGLE ->
      ok;
    ?WHATSON ->
      send_whats_on(Name, GgtProcs),
      ready_state_loop(Name, GgtProcs, GgtCount, Ttw, Ttt, GgtProcs);
    ?RESET ->
      log(Name, "state(ready) received reset:(~s)~n", [timeMilliSecond()]),
      log(Name, "state(ready) sending kill to ggt procs in ~s:(~s)~n", [list2String(GgtProcs), timeMilliSecond()]),
      send_kill(GgtProcs),
      log(Name, "state(ready) resetting wggt/corr_flag/min_reported_mi/ggt_proc_list to defaults:(~s)~n", [timeMilliSecond()]),
      log(Name, "state(ready) reset completed::transition to 'register' state:(~s)~n", [timeMilliSecond()]),
      receive_register_requests(Name, GgtCount, Ttw, Ttt, GgtProcs);
    ?KILL ->
      log(Name, "state(ready) received 'kill' sending kill to all ggt processes:(~s)~n", [timeMilliSecond()]),
      log(Name, "state(ready) sending kill to ggt procs in ~s:(~s)~n", [list2String(GgtProcs), timeMilliSecond()]),
      send_kill(GgtProcs),
      log(Name, "state(ready) globally unbinding coord ~p with ?:(~s)~n", [self(), timeMilliSecond()]),

      log(Name, "state(ready) unregistering ~p:(~s)~n", [self(), timeMilliSecond()]),

      log(Name, "state(ready) ~p going down:(~s)~n", [self(), timeMilliSecond()])
  end.

set_start_values(_, [], []) -> ok;
set_start_values(Name, GgtProcs, Mis) ->
  [Proc | GgtProcsTail] = GgtProcs,
  [Mi | MisTail] = Mis,
  log(Name, "state(ready) setting initial mi=~b for {~p}:(~s)~n", [Mi, Proc, timeMilliSecond()]),
%%   Proc ! {?SETPMI, Mi},
  set_start_values(Name, GgtProcsTail, MisTail).

trigger_calculation(_, _, _, 0) ->
  ok;
trigger_calculation(Name, GgtProcs, Target, ProcCount) ->
  [Proc | GgtProcsTail] = GgtProcs,
  % "eine Zahl (Vielfaches von target)" aha
  MultipleTarget = Target * random:uniform(100),
  log(Name, "state(ready) sending initial y=~b for {~p}:(~s)~n", [MultipleTarget, Proc, timeMilliSecond()]),
%%   Proc ! {?SEND, Target},
  trigger_calculation(Name, GgtProcsTail, Target, ProcCount - 1).

send_kill([]) -> ok;
send_kill(GgtProcs) ->
  [Proc | GgtProcsTail] = GgtProcs,
%%   Proc ! ?KILL,
  send_kill(GgtProcsTail).

send_whats_on(_, []) -> ok;
send_whats_on(Name, GgtProcs) ->
  [Proc | GgtProcsTail] = GgtProcs,
  log(Name, "state(ready) sending whats_on to '~s':(~s)~n", [Proc, timeMilliSecond()]),
  Proc ! ?WHATSON,
  send_whats_on(Name, GgtProcsTail).
