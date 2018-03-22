-module(ai).
-author('labihbc@gmail.com').
-include("ebt_config.hrl").
-include("ebt.hrl").

-export([init/1, neaten/1]).

init(Data) ->
	{ok, Json, _} = rfc4627:decode(Data),
	RootId = get_field_string(Json, "root"),
	{ok, NodesJson} = rfc4627:get_field(Json, "nodes"),
	NodeList = init_nodes([RootId], NodesJson, []),
	#ebt_config_data{root_id = RootId, nodes = NodeList}.

%% 整理节点
neaten(#ebt_config_data{root_id = RootId, nodes = Nodes}) ->
	case lists:keytake(RootId, #ebt_config_node.id, Nodes) of
		false ->
			#ebt_node{};
		{value, EbtConfigNode = #ebt_config_node{}, Nodes2} ->
			do_neaten(EbtConfigNode, Nodes2)
	end.

do_neaten(#ebt_config_node{id = _Id, mod = Mod, children = Children, properties = Properties}, Nodes) ->
	F = fun(ChildId) ->
		case lists:keytake(ChildId, #ebt_config_node.id, Nodes) of
			false ->
				[];
			{value, EbtConfigNode = #ebt_config_node{}, Acc2} ->
				do_neaten(EbtConfigNode, Acc2)
		end
	end,
	ChildEbtNodes = lists:flatten([F(Child) || Child <- Children]),
	#ebt_node{mod = Mod, childs = ChildEbtNodes, param = #{static => Properties} }.


init_nodes([], _NodesJson, NodeList) ->
	NodeList;
init_nodes([Id | LeftIdList], NodesJson, NodeList) ->
	{ok, NodeJson} = rfc4627:get_field(NodesJson, Id),
	Name = get_field_string(NodeJson, "name"),
	Title = get_field_string(NodeJson, "title"),
	{ok, Properties} = rfc4627:get_field(NodeJson, "properties"),
	Children = lists:map(fun(E) -> binary_to_list(E) end, rfc4627:get_field(NodeJson, "children", [])),
	BteNode = 
		#ebt_config_node{
			id = Id,
			name = Name,
			mod = mod(Title),
			properties = get_all(Properties),
			children = Children
		},
	NewNodeList = [BteNode | NodeList],
	if 
		Children /= [] ->
			init_nodes(Children ++ LeftIdList, NodesJson, NewNodeList);
		true ->
			init_nodes(LeftIdList, NodesJson, NewNodeList)
	end.

get_field_string(Json, Key) ->
	{ok, Value} = rfc4627:get_field(Json, Key),
	binary_to_list(Value).

get_all(Properties) ->
	{obj, PrapList} = Properties,
	lists:map(fun({EKey, EValue}) ->
		if 
			is_binary(EValue) ->
				ENewValue = binary_to_list(EValue);
			true ->
				ENewValue = EValue
		end,
		{EKey, ENewValue}
	end,
	PrapList).

%% 模块转换成处理mod atom

mod("ActionTrace") ->
	action_trace;
mod("ActionHover") ->
	action_hover;
mod("ActionIdle") ->
	action_idle;
mod("ActionAttack") ->
	action_attack;

mod("ConditionInWarn") ->
	condition_in_warn;
mod("ConditionInAttack") ->
	condition_in_attack;

mod("Priority") ->
	ebt_priority_selector;
mod("RandomSelector") ->
	ebt_random_selector;
mod("Sequence") ->
	ebt_sequence.
