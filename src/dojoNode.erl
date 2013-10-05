%% Copyright
-module(dojoNode).
-author("yuki").

-define(THRESHOLD, 1).
-define(TIME_CONST, 1.1).

%% API
-export([start_link/1]).
-export([init/0]).

start_link(_Pos)->
  %return process link to initiator
  spawn_link(?MODULE, init, []).

init()->
  %create table for storing source synapses data
  Sources = ets:new(sources, []),

  io:format("~p ~p node started~n", [erlang:now(),self()]),
  node_loop(10, 0, Sources, []).

node_loop(Timeout, Voltage, Sources, Targets)->
  receive
      %AP from Presynaptic cell
      {ap, Source, Value} ->
          case ets:lookup(Sources, Source) of
              [{Source, {Cleft, Distance, Post}}] ->
                  %fill cleft with mediator from presynapse
                  NewCleft = Cleft + Value,
                  %change in membrane voltage
                  AddedVoltage =  NewCleft*Post/Distance,
                  %mediator in cleft
                  RemainMediator =  NewCleft-NewCleft*Post,

                  ets:insert(Sources, {Source, {RemainMediator, Distance, Post}}),

                  %add voltage from synapse to node's membrane
                  node_loop(1, Voltage + AddedVoltage, Sources, Targets) ;

              _Any ->
                  not_exist,
                  node_loop(Timeout, Voltage, Sources, Targets)
          end;

      %create synapse with Presynapric cell
      {add_source, Source, Distance, Post} ->
          case ets:lookup(Sources, Source) of
              [] ->
                  %create Synapse with random koeff and empty cleft
                  ets:insert(Sources, {Source, {0, Distance, Post}})   ;
              [_Any] ->
                  already_exist
          end,
          %continue
          node_loop(Timeout, Voltage, Sources, Targets);

      %create synapse with Postsynaptic cell
      {add_target, Target} ->
          case lists:member(Target, Targets) of
              false ->
                  %add target to targets list
                  NewTargets = [Target | Targets],
                  %continue
                  node_loop(Timeout, Voltage, Sources, NewTargets) ;
              %such target already registered
              true ->
                  %continue
                  node_loop(Timeout, Voltage, Sources, Targets)
          end;

      {kill, Reason} ->
          io:format("~p ~p node stopped with reason ~p~n", [erlang:now(), self(), Reason]),
          exit(self(), Reason);

      Unknown ->
          io:format("Unknown message :~p~n", [Unknown]),
          %continue
          node_loop(Timeout, Voltage, Sources, Targets)

      after Timeout ->

          if  Voltage > ?THRESHOLD ->

                  generate_AP(Targets, 1),
                  modify_postsynapses(Sources),

                  io:format("~p ~p generates AP~n", [erlang:now(), self()]),
                  %continue in 10 msec
                  node_loop(10, 0, Sources, Targets);

              Voltage =< ?THRESHOLD ->
                  if  Timeout < 10 ->
                          %continue
                          node_loop(10, Voltage, Sources, Targets);
                      Timeout >= 10 ->
                          %continue
                          node_loop(round(Timeout*?TIME_CONST), Voltage, Sources, Targets)
                  end
          end

      end.


generate_AP(Targets, Value)->
    Fun =  fun(Target) ->
        Target ! {ap, self(), Value}
    end,
    lists:foreach(Fun, Targets).

modify_postsynapses(Sources)->
    ok.

