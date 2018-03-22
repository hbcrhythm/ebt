
-record(ebt_config_node, {
		id = "",
		name = "",			%% 节点所属类别名字
		mod = "",			%% 节点处理模块名字
		properties = [],	
		children = []
	}).

-record(ebt_config_data, {
		root_id = "",
		nodes = []			%% json里面的节点信息
	}).

%% 行为树节点参数
% -record(ebt_node_param, { 
%         static  =  #{}  :: #{}      %%静态参数, 一般静态参数放初始化配置
%         dynamic     %%动态参数, 动态参数运行时可能会修改
%     }).
% -type ebt_node_param() :: #ebt_node_param{}.