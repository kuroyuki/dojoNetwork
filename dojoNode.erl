%% @author yuki
%% @doc @todo Add description to dojoNode.

-define(THRESHOLD, 100).

-module(dojoNode).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, init/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
start(Pos)->
  spawn(?MODULE, init, [Pos]).

init(Pos)->	
	Sources = ets:new(sources, []),
	node_loop(Pos, 0, Sources, []). 

node_loop(Pos, Voltage, Sources, Targets)->
	receive 
		{add_source, Source} ->
			ets:insert(Sources, {Source, 1}),
			io:format("~p received : add_source ~p~n", [Pos, Source]);

		{add_target, Target} ->
			NewTargets = [Targets | Target],
			io:format("~p received : add_target ~p~n", [Pos, Target]),
			node_loop(Pos, Voltage, Sources, NewTargets);		
			
		Any ->
			io:format("~p received : ~p~n", [Pos, Any])			
	after 1000 ->
		if
			Voltage>?THRESHOLD	->
				node_loop(Pos, 0, Sources, Targets);
			Voltage=<?THRESHOLD ->
				ok
		end
	end,
	node_loop(Pos, Voltage, Sources, Targets).






