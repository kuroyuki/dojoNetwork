%% Copyright
-module(dojoNode).
-author("yuki").

-define(THRESHOLD, 1).
-define(TIME_CONST, 1.1).

%% API
-export([start_link/0]).
-export([init/0]).

start_link()->
  %return process link to initiator
  spawn_link(?MODULE, init, []).

init()->
  io:format("~p ~p node started~n", [erlang:now(),self()]),
  node_loop(10, 0).

node_loop(Timeout, Voltage)->
  receive
      %AP from Presynaptic cell
      {ap, Source} ->
        {Distance, {Pre, Cleft, Post}} = dojoDB:get_synapse(Source, self()),
        %fill cleft with mediator from presynapse
        %NewCleft = Cleft + Pre,
        %change in membrane voltage
        AddedVoltage =  Cleft*Post/Distance,
        %mediator in cleft
        %RemainMediator =  NewCleft-NewCleft*Post,

        %save new data
        dojoDB:insert_synapse(Source, self(), {Distance, {Pre, Cleft, Post}}),

        %add voltage from synapse to node's membrane
        node_loop(1, Voltage + AddedVoltage) ;

      {kill, Reason} ->
          io:format("~p ~p node stopped with reason ~p~n", [erlang:now(), self(), Reason]),
          exit(self(), Reason);

      Unknown ->
          io:format("Unknown message :~p~n", [Unknown]),
          %continue
          node_loop(Timeout, Voltage)

      after Timeout ->

          if  Voltage > ?THRESHOLD ->

                  generate_AP(),

                  io:format("~p ~p generates AP~n", [erlang:now(), self()]),
                  %continue in 10 msec
                  node_loop(10, 0);

          Voltage =< ?THRESHOLD ->
              if  Timeout < 10 ->
                      %continue
                      node_loop(10, Voltage);
                  Timeout >= 10 ->
                      %continue
                      node_loop(round(Timeout*?TIME_CONST), Voltage)
              end
          end
      end.


generate_AP()->
    Targets = dojoDB:get_targets(self()),
    Fun =  fun({synapse, {Source, _Target, {_Distance, {_Pre, _Cleft, _Post}}}}) ->
        Source ! {ap, self()}
    end,
    lists:foreach(Fun, Targets).

