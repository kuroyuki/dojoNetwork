%% Copyright
-module(dojoNode).
-author("yuki").

-define(THRESHOLD, 100).
-define(DEFAULT_PRE, 100).
-define(DEFAULT_CLEFT, 0).
-define(DEFAULT_POST, 0.5).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).
-export([init/1]).

%% ====================================================================
%% Internal functions
%% ====================================================================
start_link(Pos)->
  %return process link to initiator
	spawn_link(?MODULE, init, [Pos]).

init(Pos)->
  %create table for storing source synapses data
	Sources = ets:new(sources, []),

	node_loop(1, Pos, 0, Sources, []). 

node_loop(Timeout, Pos, Voltage, Sources, Targets)->
	receive 
		%message from Presynaptic cell
		{ap_from_source, Source} ->
			%find source in table
			case ets:lookup(Sources, Source) of
				[{Source, {Pre, Cleft, Post}}] ->	
					%modify Pre
					modify_presynapse(),
					%fill cleft with mediator from presynapse cell
					ets:insert(Sources, {Source, {Pre, Cleft + Pre, Post}});

				_Any ->
					io:format("no source ~p~n", [Source])
			end,
      %continue
      node_loop(Timeout, Pos, Voltage, Sources, Targets);

		%create synapse with Presynapric cell
		{add_source, Source} ->			
			case ets:lookup(Sources, Source) of								
				[] ->
					ets:insert(Sources, {Source, {?DEFAULT_PRE, ?DEFAULT_CLEFT, ?DEFAULT_POST}});
				[_Any] ->					
					already_exist
			end,
      %continue
      node_loop(Timeout, Pos, Voltage, Sources, Targets);

		%create synapse with Postsynaptic cell
		{add_target, Target} ->			
			case find_target(Targets, Target) of
				[] ->
					NewTargets = [Targets | Target],
          %continue
          node_loop(Timeout, Pos, Voltage, Sources, NewTargets) ;

				%such target already registered
				_Target ->
          %continue
          node_loop(Timeout, Pos, Voltage, Sources, Targets)
			end;
			
		Any ->
			io:format("~p received : ~p~n", [Pos, Any])	,
      %continue
      node_loop(Timeout, Pos, Voltage, Sources, Targets)
	
	%regular checking - all time-depend features should be implemented here
	after 1 ->
		%axon growing algorithm - will be here
		%transfer mediator from presynaptic clefts to this cell
		NewVoltage = check_postsynapses(Sources) + Voltage,

		if
      %Node threshold is achieved
			NewVoltage>?THRESHOLD	->				
				%io:format("~p : ~p generates AP~n", [erlang:now(), Pos]),
				send_ap_to_all_targets(Targets),
        %next check in 10 ms
				node_loop(10, Pos, 0, Sources, Targets);
      % Voltage threshold  is not achieved
			NewVoltage=<?THRESHOLD ->
				node_loop(10, Pos, NewVoltage, Sources, Targets)
		end
	end,
  io:format("~p node with pos ~p stopped ~n", [self(), Pos]).

send_ap_to_all_targets([])->
	ok;

send_ap_to_all_targets(TargetList)->
	[RemainTargets | Target] = TargetList,	
	Target ! {ap_from_source, self()},
	send_ap_to_all_targets(RemainTargets).

find_target([], _Target)->
	[];

find_target(TargetsList, Target) ->
	[RemainTargets | CheckTarget] = TargetsList,
	if 
		Target == CheckTarget ->
		   CheckTarget;
		
		Target =/= CheckTarget ->
			find_target(RemainTargets, Target) 
	end.

check_postsynapses(Sources) ->
	
	case ets:first(Sources) of
		'$end_of_table' ->			
			0;
		
		Source ->
			[{Source, {Pre, Cleft, Post}}] = ets:lookup(Sources, Source),			
			ets:insert(Sources, {Source, {Pre, Cleft - Cleft*Post,  Post}}),	
			Voltage = check_postsynapses(Sources, Source, Cleft*Post),		
			Voltage	
			
	end.
check_postsynapses(Sources, Next, CurrentVoltage) ->
	case ets:next(Sources, Next) of
		'$end_of_table' ->			
			CurrentVoltage+0;
		
		Source ->
			[{Source, {Pre, Cleft, Post}}] = ets:lookup(Sources, Source),
			ets:insert(Sources, {Source, {Pre, Cleft - Cleft*Post,  Post}}),
			check_postsynapses(Sources, Source, CurrentVoltage+Cleft*Post)
	end.

modify_presynapse()->
	ok.

