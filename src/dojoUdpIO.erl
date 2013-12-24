%%%-------------------------------------------------------------------
%%% @author Yuki
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2013 7:18 PM
%%%-------------------------------------------------------------------
-module(dojoUdpIO).
-author("Yuki").

-define (UDP_HOST, "127.0.0.1").
-define(UDP_PORT, 45500).

%% API
-export([start/1, init/1]).
-export([code_update/4]).

start(Port)->
  spawn(?MODULE, init, [Port]).

init(Port)->
  {ok, Socket} = gen_udp:open(Port, [binary]),
  loop(Socket, <<>>, [], []).

loop(Socket, Binary, Sources, Targets) ->
  receive
    {udp, Socket, _Host, _Port, Bin} ->
      io:format("received bin ~p~n", [Bin]),
      NewTargets = extract_node_data(Bin, Targets),
      loop(Socket, Binary, Sources, NewTargets);

    {ap, SourcePid} ->
      {SourcePid, _Source, {TargetX, TargetY, TargetZ}} =  lists:keyfind(SourcePid, 1, Sources),
      NewBinary = <<TargetX:16, TargetY:16, TargetZ:16>>,
      loop(Socket, list_to_binary([Binary, NewBinary]), Sources, Targets);

    {add_source, Source, Target, _Weight} ->
      SourcePid = dojoDB:get_node_pid(Source),
      NewSources = lists:keystore(SourcePid, 1, Sources, {SourcePid, Source, Target}),
      io:format("new sources ~p ~n", [NewSources]),
      loop(Socket, Binary,  NewSources, Targets);

    {add_target, Source, Target} ->
      TargetPid = dojoDB:get_node_coords(Target),
      {Source, TargetsList} = lists:keyfind(Source, 1, Targets),
      NewTargets = lists:keyreplace(Source, 1, Targets, {Source, [TargetsList | TargetPid]}),
      loop(Socket, Binary,  Sources, NewTargets);

    {code_update} ->
      ?MODULE:code_update(Socket, Binary, Sources, Targets);

    Any->
      io:format("unknown message ~p~n", [Any]),
      loop(Socket, Binary, Sources, Targets)

    after 10 ->
      case Binary of
      <<>>  ->
        loop(Socket, <<>>, Sources, Targets);
      _Any ->
        gen_udp:send(Socket, ?UDP_HOST, ?UDP_PORT, Binary),
        io:format("send UDP binary ~p~n", [Binary]),
        loop(Socket, <<>>, Sources, Targets)
    end
  end.

extract_node_data(<<>>, Targets)->
  Targets;
extract_node_data(Bin, Targets)->
  %get remote source Coords from binary
  {Data, RemainData} = split_binary(Bin, 6),
  <<X:16, Y:16, Z:16>> = Data,
  %find Target Pid
  case  lists:keyfind({X,Y,Z}, 1, Targets) of
    {_Source, TargetsList} ->
      %send AP to each Target
      Fun = fun(TargetPid) ->
        TargetPid ! {ap, self()}
      end,
      lists:foreach(Fun, TargetsList),
      io:format("sent AP to ~p ~n", [TargetsList]),
      %continue
      extract_node_data(RemainData, Targets);
    %no such remote source
    false ->
      %insert it in local table without Targets
      NewTargets = lists:keystore({X,Y,Z}, 1 , Targets, {{X,Y,Z}, []}),
      %store Node in dojoDB
      dojoDB:insert_node({X,Y,Z}, self()),
      %continue
      extract_node_data(RemainData, NewTargets);
    %Targets found

    Any ->
      io:format("Any ~p ~n", [Any]),
      %continue
      extract_node_data(RemainData, Targets)
  end.

code_update(Socket, Binary, Sources, Targets)->
  io:format("code updated~n",[]),
  loop(Socket, Binary, Sources, Targets).

