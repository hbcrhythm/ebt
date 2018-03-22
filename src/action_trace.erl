-module(action_trace).
-ebt_extend(ebt_action).
-author('labihbc@gmail.com').
%% @doc 条件节点，检测是否在警戒范围

-include("ebt.hrl").

-include("ebt_transform.hrl").

-compile(export_all).

do_enter(_Node) ->
	io:format("action trace 节点 do_enter ~n"),
	ok.

do_execute(_Node) ->
	io:format("action trace 节点 do_execute ~n"),
	?EBT_RESULT_FINISHED.

do_exit(_Node) ->
	io:format("action trace 节点 do_exit ~n"),
	ok.