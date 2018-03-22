%%%-------------------------------------------------------------------
%%% @author hbc labihbc@gmain.com
%%% @doc　几率选择节点,不可被同节点打断
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ebt_random_selector).
-ebt_extend(ebt_node).

-include("ebt.hrl").

-include("ebt_transform.hrl").

%% API
-export([init/1, do_evaluate/1, tick/1]).

do_evaluate(#ebt_node{id = Id, childs = Childs, param = #{static := SParam}}) ->
    io:format("ebt_random_selector ~n"),
    Data = ?EBT_NODE_DATA(Id),
    LastActiveNode = maps:get(active_child, Data, undefined),
    case LastActiveNode of
        undefined when SParam =:= [] ->
            Random = rand:uniform(length(Childs)),
            ActiveChild = lists:nth(Random, Childs),
            Data2 = Data#{active_child => ActiveChild},
            ?EBT_NODE_DATA(Id, Data2),
            true;
        undefined ->
            Random = rand:uniform(100),
            case select(SParam, Random) of
                undefined ->
                    false;
                ActiveId ->
                    ActiveChild = lists:nth(Random, Childs),
                    Data2 = Data#{active_child => ActiveChild},
                    ?EBT_NODE_DATA(Id, Data2),
                    true
            end;
        #ebt_node{} ->
            true
    end.


tick(#ebt_node{id = Id}) ->
    Data = ?EBT_NODE_DATA(Id),
    case maps:get(active_child, Data, undefined) of
        undefined ->
            ?EBT_RESULT_FINISHED;
        #ebt_node{mod = Mod} = ChildNode ->
            case Mod:tick(ChildNode) of
                ?EBT_RESULT_FINISHED ->
                    Mod:clear(ChildNode),
                    Data2 = maps:remove(active_child, Data),
                    ?EBT_NODE_DATA(Id, Data2),
                    ?EBT_RESULT_FINISHED;
                ?EBT_RESULT_RUNNING ->
                    ?EBT_RESULT_RUNNING
            end
    end.

% @private 选择可进入子节点
select(SParam, Random) ->
    select(SParam, Random, 0).
select([], _, _) ->
    undefined;
select([{Id, Prob} | T], Random, Acc) ->
    Acc2 = Prob + Acc,
    case Prob + Acc >= Random of
        true ->
            list_to_integer(Id);
        false ->
            select(T, Random, Acc2)
    end.