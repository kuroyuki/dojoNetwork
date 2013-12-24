%% Copyright
-module(dojoNode).
-author("yuki").

-define(THRESHOLD, 35.0).
-define(TIME_CONST, 1.1).

-define(REST_POTENTIAL, -40.0).

-define(A, 0.02). %Um decay rate
-define(B, 0.2).  %Um sensitivity
-define(C, -65).  %Vm reset
-define(D, 8).    %Um reset
-define(H, 1).    %Time slot


-compile(export_all).

%% API
-export([start_link/0]).
-export([init/0]).
-export([code_update/4]).

start_link()->
  %return process link to initiator
  spawn_link(?MODULE, init, []).

init()->
    %dojoManager!{node_message, {self(), "node started", []}},
    node_loop(-60, 0, [], []).

node_loop(Vm, Um, Sources, Targets) ->
  receive
    %AP from Presynaptic cell
    {ap, Source} ->
      {Source, _Current, Weight} = lists:keyfind(Source, 1, Sources),
      NewSources = lists:keyreplace(Source, 1 , Sources, {Source, 1, Weight}),
      node_loop(Vm, Um, NewSources, Targets);

    {add_source, Source, _Target, Weight} ->
      SourcePid = dojoDB:get_node_coords(Source),
      NewSources = lists:keystore(SourcePid, 1, Sources, {SourcePid, 0, Weight}),
      node_loop(Vm, Um, NewSources, Targets);

    {add_target, _Source, Target} ->
      TargetPid = dojoDB:get_node_coords(Target),
      NewTargets = lists:keystore(TargetPid, 1, Targets, {TargetPid}),
      node_loop(Vm, Um, Sources, NewTargets);

    {code_update} ->
      ?MODULE:code_update(Vm, Um, Sources, Targets);

    {kill, Reason} ->
      exit(self(), Reason);

    Any->
      io:format("unknown message ~p~n", [Any]),
      node_loop(Vm, Um, Sources, Targets)

   after 1000  ->
    if  Vm > ?THRESHOLD ->
      NewVm = ?REST_POTENTIAL,
      NewUm = Um + ?D,

      io:format(" ~p ~p ap generated ~n", [erlang:now(), self()]),
      broadcast_ap(Targets),

      node_loop(NewVm, NewUm, Sources, Targets);

    Vm =< ?THRESHOLD ->
      Isyn =  sum_currents(Sources),
      NewVm = Vm + ?H*izhikVm(Vm, Um, Isyn),
      NewUm = Um + ?H*izhikUm(Vm, Um),
      NewSources = modify_currents(Sources),
      %io:format("Vm ~p, Um ~p, Isyn ~p~n", [NewVm, NewUm, Isyn]),
      node_loop(NewVm, NewUm, NewSources, Targets)

    end
  end.

izhikVm(Vm, Um, Isyn) ->
  NewVm = 0.04*Vm*Vm + 5*Vm + 140 - Um + Isyn,
  NewVm.

izhikUm(Vm, Um)->
  NewUm = ?A*(?B*Vm - Um),
  NewUm.


sum_currents(List)->
  Fun =  fun({_Source, Current, Weight} , TotalCurrent) ->
    TotalCurrent + Current*Weight
  end,
  lists:foldl(Fun, 0,  List).

modify_currents(List)->
  Fun =  fun(Current) ->
    Current*math:exp(?H/-4)
  end,
  lists:keymap(Fun, 2,  List).

broadcast_ap(Targets) ->
  Fun = fun({Target}) ->
    Target ! {ap, self()}
  end,
  lists:foreach(Fun, Targets).

code_update(Vm, Um, Sources, Targets)->
  io:format("code updated~n",[]),
  node_loop(Vm, Um, Sources, Targets).
