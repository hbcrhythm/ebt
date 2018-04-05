-module('ebt').
-author('labihbc@gmail.com').
-include("ebt_config.hrl").
-include("ebt.hrl").

-define(DEFAULT_ERL_DIR, "erl").
-define(DEFAULT_JSON_DIR, "json").

%% API exports

-export([create/1, neaten/1]).

%%====================================================================
%% API functions
%%====================================================================
create(JsonFile) ->
	ErlDir = application:get_env(ebt, erl_dir, ?DEFAULT_ERL_DIR),
	file:make_dir(ErlDir),
	JsonDir = application:get_env(ebt, json_dir, ?DEFAULT_JSON_DIR),
	case file:read_file(JsonDir ++ "/" ++ JsonFile) of
		{ok, EBinary} ->
			Json = jsx:decode(EBinary),
			RootId = get_field_string(Json, "root"),
			NodesJson = get_field(Json, "nodes"),
			NodeList = init_nodes([RootId], NodesJson, []),
			EbtConfigData = #ebt_config_data{root_id = RootId, nodes = NodeList},
			EbtNode = neaten(EbtConfigData),

			EbtNodeString = term_to_string(EbtNode),
			ModuleString = io_lib:format("%%------------------------------------------------------~n %% Automatic Generation ~n %%-----------------------------------------------------~n-module(~s).~n-author('labihbc@gmail.com').~n~n-export([get/0]).~n~nget() ->~n ",[JsonFile]),
			file:write_file(ErlDir ++ "/" ++  string:sub_string(JsonFile, 1, string:rstr(JsonFile, ".json") - 1) ++ ".erl", ModuleString ++ EbtNodeString ++ "."),
			io:format("File: [~s] Generation Success ! ~n ",[JsonFile]),
			% EbtNode;
			ok;
		_ ->
			undefined
	end.

%% 整理节点
neaten(#ebt_config_data{root_id = RootId, nodes = Nodes}) ->
	case lists:keytake(RootId, #ebt_config_node.id, Nodes) of
		false ->
			#ebt_node{};
		{value, EbtConfigNode = #ebt_config_node{}, Nodes2} ->
			do_neaten(EbtConfigNode, Nodes2)
	end.

%%====================================================================
%% Internal functions
%%====================================================================
init_nodes([], _NodesJson, NodeList) ->
	NodeList;
init_nodes([Id | LeftIdList], NodesJson, NodeList) ->
	NodeJson = get_field(NodesJson, Id),
	Name = get_field_string(NodeJson, "name"),
	Title = get_field_string(NodeJson, "title"),
	Properties = get_field(NodeJson, "properties"),
	Children = lists:map(fun(E) -> binary_to_list(E) end, get_field(NodeJson, "children", [])),
	BteNode = 
		#ebt_config_node{
			id = Id,
			name = Name,
			mod = to_atom(Title),
			properties = get_properties(Properties),
			children = Children
		},
	NewNodeList = [BteNode | NodeList],
	if 
		Children /= [] ->
			init_nodes(Children ++ LeftIdList, NodesJson, NewNodeList);
		true ->
			init_nodes(LeftIdList, NodesJson, NewNodeList)
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
	#ebt_node{mod = Mod, childs = ChildEbtNodes, param = #{static => Properties}}.



get_field(Json, Key) ->
	case lists:keyfind(to_binary(Key), 1, Json) of
		false ->
			not_found;
		{_, Value} ->
			Value
	end.
get_field(Json, Key, Default) ->
	case lists:keyfind(to_binary(Key), 1, Json) of
		false ->
			Default;
		{_, Value} ->
			Value
	end.

get_field_string(Json, Key) ->
	case lists:keyfind(to_binary(Key), 1, Json) of
		false ->
			not_found;
		{_, Value} ->
			to_list(Value)
	end.

get_properties([{}]) ->
	[];
get_properties(Properties) ->
	lists:map(fun({EKey, EValue}) ->
		ENewKey = if
			is_binary(EKey) ->
				binary_to_list(EKey);
			true ->
				EKey
		end,
		ENewValue = if 
			is_binary(EValue) ->
				binary_to_list(EValue);
			true ->
				EValue
		end,
		{ENewKey, ENewValue}
	end,
	Properties).


to_binary(Msg) when is_binary(Msg) ->
    Msg;
to_binary(Msg) when is_atom(Msg) ->
    list_to_binary(atom_to_list(Msg));
to_binary(Msg) when is_list(Msg) ->
    list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) ->
    list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_float(Msg) ->
    list_to_binary(f2s(Msg));
to_binary(Msg) when is_tuple(Msg) ->
    list_to_binary(tuple_to_list(Msg));
to_binary(_Msg) -> throw(other_value).
	
to_list(Msg) when is_list(Msg) ->
    Msg;
to_list(Msg) when is_atom(Msg) ->
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) ->
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) ->
    integer_to_list(Msg);
to_list(Msg) when is_float(Msg) ->
    f2s(Msg);
to_list(Msg) when is_tuple(Msg) ->
    tuple_to_list(Msg);
to_list(_) -> throw(other_value).

to_atom(Msg) when is_atom(Msg) ->
    Msg;
to_atom(Msg) when is_binary(Msg) ->
    list_to_atom2(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) ->
    list_to_atom2(Msg);
to_atom(_) -> throw(other_value).

list_to_atom2(List) when is_list(List) ->
    case catch(list_to_existing_atom(List)) of
        {'EXIT', _} -> erlang:list_to_atom(List);
        Atom when is_atom(Atom) -> Atom
    end.

f2s(N) when is_integer(N) ->
    integer_to_list(N) ++ ".00";
f2s(N) when is_list(N) ->
    [A] = io_lib:format("~.2f", [N]),
    A.

% term_to_bitstring(Term) ->
%     erlang:list_to_bitstring(io_lib:format("~p", [Term])).

term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).