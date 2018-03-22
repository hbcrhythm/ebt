-module(condition_in_warn).
-ebt_extend(ebt_condition).
-author('labihbc@gmail.com').
%% @doc 条件节点，检测是否在警戒范围

-include("ebt.hrl").

-include("ebt_transform.hrl").

-compile(export_all).

do_check(_Node = #ebt_node{param = Param = #{dynamic = Dynamic}}) ->
	case maps:get(id, Dynamic, undefined) of
		undefined -> false;
		Id ->
			% #data_monster_res{ai_warn_radius = WarnRadius} = data_monster_res:get(Id),
			ok
	end,
	io:format("条件节点，判断是否可以战斗, 结果 [是] ~n"),
	true.