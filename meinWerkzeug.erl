-module(meinWerkzeug).
-author("vince").

-import(werkzeug, [get_config_value/2]).
-import(file, [consult/1]).
-include("messages.hrl").

-export([read_config/2, lookup/2]).

read_config(Keys, FileName) ->
  {ok, File} = consult(FileName),
  list_to_tuple(read_config_rec(Keys, File)).
read_config_rec([], _) ->
  [];
read_config_rec([Key|Keys], File) ->
  {ok, Value} = get_config_value(Key, File),
  [Value|read_config_rec(Keys, File)].

lookup(Nameservice, Name) ->
  Nameservice ! {self(), {?LOOKUP, Name}},
  receive
    {?REBIND_RES, ServiceAtNode} ->
      ServiceAtNode
  end.