-module(test).
-compile(export_all).

-include("ebt.hrl").
-include("ebt_transform.hrl").

test() ->
	Node = #ebt_node{
	    mod = ebt_priority_selector,
	    childs = [
	        #ebt_node{
	            mod = action_fight,
	            precondition = fun action_fight:check_fight/1
	        },
	        #ebt_node{
	            mod = action_idle,
	            precondition = fun action_idle:check_idle/1
	        }
	    ]	
    },

	{ok, InitedNode} = ebt_tree:init(Node),
	ebt_tree:run(InitedNode).


ai_test() ->
	case file:read_file("json/" ++ "test.json") of
		{ok, EBinary} ->
			Result = ai:init(EBinary),
			io:format("Result ~p",[Result]),

			Result2 = ai:neaten(Result),
			io:format("Result2 ~p",[Result2]),
			ok;
		_ ->
			skip
	end.

% test:ai_test().