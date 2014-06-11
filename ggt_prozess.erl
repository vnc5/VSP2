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
  Parent = self(),
  Calculator = spawn_link(fun() -> calculator(Parent, Name, Ttw) end),
  register_ggt_process(Calculator, Name, Nameservice, Coord, Ttw, Ttt).

calculator(Parent, Name, Ttw) ->
  receive
    {calc, Mi, Y} ->
      calc_ggt(Parent, Name, Mi, Y, Ttw),
      calculator(Parent, Name, Ttw)
  end.

calc_ggt(Parent, Name, Mi, Y, Ttw) ->
  if
    Y < Mi ->
      log(Name, "ggt:~p (processing)::calculation start:: calculating new ggt:: mi=~b, y=~b::duration ~b(s):(~s)~n", [Name, Mi, Y, Ttw, timeMilliSecond()]),
      sleep(Ttw * 1000),
      NewMi = ((Mi - 1) rem Y) + 1,
      log(Name, "ggt:~p (processing)::calculation done:: changes mi=~b to ~b:(~s)~n", [Name, Mi, NewMi, timeMilliSecond()]);
    true ->
      NewMi = Mi,
      log(Name, "ggt:~p (processing)::no ggt calculation reason:: ~b>=~b:(~s)~n", [Name, Y, Mi, timeMilliSecond()])
  end,
  Parent ! {calc_done, NewMi}.

register_ggt_process(Calculator, Name, Nameservice, Coord, Ttw, Ttt) ->
  log(Name, "ggt:locally registering ~p; name is '~p':(~s)~n", [self(), Name, timeMilliSecond()]),
  register(Name, self()),
  Nameservice ! {self(), {?REBIND, Name, node()}},
  receive
    {?REBIND_RES, ok} ->
      log(Name, "ggt: ~p bound as service ~p:(~s)~n", [self(), {Name, node()}, timeMilliSecond()]),
      log(Name, "ggt:~p (initial)::~p checkin in to coordinator ~p as service ~p:(~s)~n", [Name, self(), Coord, Name, timeMilliSecond()]),
      Coord ! {?CHECKIN, Name},
      wait_for_neighbours(Calculator, Name, Nameservice, Coord, Ttw, Ttt)
  end.

wait_for_neighbours(Calculator, Name, Nameservice, Koordinator, Ttw, Ttt) ->
  log(Name, "ggt:~p (initial)::waiting for neighbours to be set:(~s)~n", [Name, timeMilliSecond()]),
  receive
    {?NEIGHBOURS, Left, Right} ->
      LeftNode = lookup(Nameservice, Left),
      RightNode = lookup(Nameservice, Right),
      log(Name, "ggt:~p (initial)::initializing neighbours::l=~p(~p), r=~p(~p):(~s)~n", [Name, Left, LeftNode, Right, RightNode, timeMilliSecond()]),
      log(Name, "ggt:~p (initial)::transition to pre process state: left neighbour=~p, right neigbour=~p:(~s)~n", [Name, Left, Right, timeMilliSecond()]),
      wait_for_first_mi(Calculator, Name, Nameservice, LeftNode, RightNode, Koordinator, Ttw, Ttt)
  end.

wait_for_first_mi(Calculator, Name, Nameservice, Left, Right, Koordinator, Ttw, Ttt) ->
  log(Name, "ggt:~p (pre_process)::waiting for mi to be set:(~s)~n", [Name, timeMilliSecond()]),
  receive
    {?SETPMI, Mi} ->
      log(Name, "ggt:~p (pre_process)::receiving set_pmi ~b:(~s)~n", [Name, Mi, timeMilliSecond()]),
      Timer = reset_timer(Name, none, Ttt, terminate),
      process(processing, Calculator, Name, Nameservice, Mi, Left, Right, Koordinator, false, 0, Timer, timestamp_micro(), Ttw, Ttt)
  end.

process(State, Calculator, Name, Nameservice, Mi, Left, Right, Coord, CanVote, SuccessfulVotes, Timer, LastMi, Ttw, Ttt) ->
  receive
    {?SETPMI, MiNeu} ->
      NewTimer = reset_timer(Name, Timer, Ttt, terminate),
      log(Name, "ggt:~p (~s)::receiving set_pmi ~b:(~s)~n", [Name, State, MiNeu, timeMilliSecond()]),
      process(processing, Calculator, Name, Nameservice, MiNeu, Left, Right, Coord, true, SuccessfulVotes, NewTimer, timestamp_micro(), Ttw, Ttt);
    {?SEND, Y} ->
      log(Name, "ggt:~p (~s)::receiving send y=~b:(~s)~n", [Name, State, Y, timeMilliSecond()]),
      cancel(Timer),
      Calculator ! {calc, Mi, Y},
      process(State, Calculator, Name, Nameservice, Mi, Left, Right, Coord, CanVote, SuccessfulVotes, none, LastMi, Ttw, Ttt);
    ?KILL ->
      log(Name, "ggt:~p (~s)::received kill::starting cleanup:(~s)~n", [Name, State, timeMilliSecond()]),
      log(Name, "ggt:~p (~s)::globally unbinding ~p with ~p:(~s)~n", [Name, State, Name, self(), timeMilliSecond()]),
      unregister(Name),
      log(Name, "ggt:~p (~s)::unregistering ~p:(~s)~n", [Name, State, Name, timeMilliSecond()]),
      Nameservice ! {self(), {?UNBIND, Name}};
    {?WHATSON, From} ->
      log(Name, "ggt:~p (~s)::receiving whats_on from \"~p\"::responding ~s:(~s)~n", [Name, State, From, State, timeMilliSecond()]),
      Coord ! {?WHATSON_RES, State},
      process(State, Calculator, Name, Nameservice, Mi, Left, Right, Coord, CanVote, SuccessfulVotes, Timer, LastMi, Ttw, Ttt);
    {?TELLMI, From} ->
      log(Name, "ggt:~p (~s)::receiving tell_mi from ~p::mi is ~b:(~s)~n", [Name, State, From, Mi, timeMilliSecond()]),
      From ! {?TELLMI_RES, Mi},
      process(State, Calculator, Name, Nameservice, Mi, Left, Right, Coord, CanVote, SuccessfulVotes, Timer, LastMi, Ttw, Ttt);
    {?VOTE, Initiator} ->
      % "Erhält ein initiierender Prozess von seinem rechten Nachbarn die Anfrage nach der Terminierung (vote), meldet er die Terminierung dem Koordinator."
      % Kann mit dieser Schnittstelle nicht sichergestellt werden, ob die Nachricht vom rechten Nachbarn kam.
      if
        Initiator /= Name ->
          NewSuccessfulVotes = SuccessfulVotes,
          NewState = State,
          Diff = timestamp_micro() - LastMi,
          if
            Diff > (Ttt / 2) * 1000000 ->
              {LeftName, _} = Left,
              log(Name, "ggt:~p (~s)::accept vote from ~p::routing to ~p::time since last mi event ~f(s):(~s)~n", [Name, State, Initiator, LeftName, Diff / 1000000, timeMilliSecond()]),
              Left ! {?VOTE, Initiator};
            true ->
              log(Name, "ggt:~p (~s)::reject vote from ~p::time since last mi event ~f(ms) :(~s)~n", [Name, State, Initiator, Diff / 1000, timeMilliSecond()])
          end;
        true ->
          NewSuccessfulVotes = SuccessfulVotes + 1,
          log(Name, "ggt:~p (~s)::terminating vote::number of successful votes::~b:(~s)~n", [Name, State, NewSuccessfulVotes, timeMilliSecond()]),
          log(Name, "ggt:~p (~s)::terminated vote::sending termination with ggt=~b to ~p:(~s)~n", [Name, State, Mi, Coord, timeMilliSecond()]),
          Coord ! {?BRIEFTERM, {Name, Mi, timeMilliSecond()}, self()},
          NewState = voted,
          log(Name, "ggt:~p (~s)::transition to state ~s:(~s)~n", [Name, State, NewState, timeMilliSecond()])
      end,
      process(NewState, Calculator, Name, Nameservice, Mi, Left, Right, Coord, CanVote, NewSuccessfulVotes, Timer, LastMi, Ttw, Ttt);
    terminate ->
      if
        CanVote =:= true ->
          NewCanVote = false,
          log(Name, "ggt:~p (~s)::starting vote:: mi is ~b:(~s)~n", [State, Name, Mi, timeMilliSecond()]),
          Left ! {?VOTE, Name};
        true ->
          NewCanVote = CanVote
      end,
      process(voting, Calculator, Name, Nameservice, Mi, Left, Right, Coord, NewCanVote, SuccessfulVotes, Timer, LastMi, Ttw, Ttt);
    {calc_done, NewMi} ->
      if
        Mi == NewMi ->
          NewTimer = reset_timer(Name, none, Ttt, terminate);
        true ->
          log(Name, "ggt:~p (processing)::calculated new mi::mi=~b::sending ~b to ~p:(~s)~n", [Name, NewMi, NewMi, Coord, timeMilliSecond()]),
          Coord ! {?BRIEFME, {Name, NewMi, timeMilliSecond()}},
          NewTimer = reset_timer(Name, none, Ttt, terminate),
          send_mi_to_neighbours(Name, NewMi, Left, Right)
      end,
      process(processing, Calculator, Name, Nameservice, NewMi, Left, Right, Coord, true, SuccessfulVotes, NewTimer, timestamp_micro(), Ttw, Ttt);
    Other ->
      log(Name, "Unknown: ~p~n", [Other]),
      process(State, Calculator, Name, Nameservice, Mi, Left, Right, Coord, CanVote, SuccessfulVotes, Timer, LastMi, Ttw, Ttt)
  end.

send_mi_to_neighbours(Name, Mi, Left, Right) ->
  {LeftName, _} = Left,
  {RightName, _} = Right,
  log(Name, "ggt:~p (processing)::sending mi=~b to neighbours:: l=~p,r=~p:(~s)~n", [Name, Mi, LeftName, RightName, timeMilliSecond()]),
  Left ! {?SEND, Mi},
  Right ! {?SEND, Mi}.

reset_timer(Name, Timer, Ttt, Message) ->
  if
    Timer =:= none ->
      {ok, NewTimer} = send_after(Ttt * 1000, Message);
    true ->
      NewTimer = Timer
  end,
  log(Name, "ggt:~p (processing)::mi event:: storing event time=~s:(~s)~n", [Name, timeMilliSecond(), timeMilliSecond()]),
  log(Name, "ggt:~p (processing)::mi event:: (re)setting timer:(~s)~n", [Name, timeMilliSecond()]),
  reset_timer(NewTimer, Ttt, Message).
