-module(action_hover).
-ebt_extend(ebt_action).

-include("ebt.hrl").

-include("ebt_transform.hrl").

-compile(export_all).

do_enter(_Node) ->
	io:format("action hover 节点 do_enter ~n"),
	ok.

do_execute(_Node) ->
	io:format("action hover 节点 do_execute ~n"),
	?EBT_RESULT_FINISHED.

do_exit(_Node) ->
	io:format("action hover 节点 do_exit ~n"),
	ok.