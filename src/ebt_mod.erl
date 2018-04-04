-module(ebt_mod).
-export([mod/1]).

%% @doc 模块转换成处理mod atom
%% @doc 本模块在项目中应该自定义。

% mod("ActionTrace") ->
% 	action_trace;
% mod("ActionHover") ->
% 	action_hover;
% mod("ActionIdle") ->
% 	action_idle;
% mod("ActionAttack") ->
% 	action_attack;
% mod("ActionBorn") ->
% 	action_born;
% mod("ActionBack") ->
% 	action_back;

% mod("ConditionInWarn") ->
% 	condition_in_warn;
% mod("ConditionInAttack") ->
% 	condition_in_attack;
% mod("ConditionIsDead") ->
% 	condition_is_dead;
% mod("ConditionIsPause") ->
% 	condition_is_pause;
% mod("ConditionIsBack") ->
% 	condition_is_back;

% mod("Priority") ->
% 	ebt_priority_selector;
% mod("RandomSelector") ->
% 	ebt_random_selector;
% mod("Sequence") ->
% 	ebt_sequence;

mod(_) ->
	undefined.
	