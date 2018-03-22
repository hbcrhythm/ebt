-module(condition_in_attack).
-ebt_extend(ebt_condition).

-include("ebt.hrl").

-include("ebt_transform.hrl").

-compile(export_all).

do_check(_Node) ->
	io:format("条件节点，判断是否在攻击范围 [是]~n"),
	true.
