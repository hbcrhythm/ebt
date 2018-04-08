%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc 行为树
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_tree).

-include("ebt.hrl").

%% API
-export([init/1, init/2, run/1, destroy/1]).

%% (数据)初始化
-callback init(Root :: ebt_node())-> {ok, RootNew :: ebt_node()}.
-callback init(Root :: ebt_node(), DParam :: #{}) -> {ok, RootNew :: ebt_node()}.

%% 运行行为树
-callback run(Node :: ebt_node()) -> ok.

%% 行为树销毁
-callback destroy() -> ok.

init(#ebt_node{} = Root) ->
    {ok, init_node(Root, #{})}.
init(#ebt_node{} = Root, DParam) ->
	{ok, init_node(Root, DParam)}.

init_node(#ebt_node{mod = RootMod, childs = Childs, param = Param} = Root, DParam) ->
	Param2 = Param#{dynamic => DParam},
    {M, F, A} = application:get_env(ebt, m_mfa, {ebt_mod, mod, []}),
    RootMod2 = erlang:apply(M, F, [RootMod] ++ A),
    RootMod2:init(Root#ebt_node{id = make_ref(), mod = RootMod2, childs = [init_node(Child, DParam) || #ebt_node{} = Child <- Childs], param = Param2}).

run(#ebt_node{mod = Mod} = Node) ->
    case Mod:evaluate(Node) of		%% 评估是否有子节点可以执行
        true ->
            Mod:tick(Node);			%% 执行对应节点的逻辑
        false ->
            false
    end.

destroy(#ebt_node{mod = Mod} = Node) ->
    Mod:clear(Node).
