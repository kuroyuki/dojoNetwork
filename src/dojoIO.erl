%% Copyright
-module(dojoIO).
-author("yuki").

%% ====================================================================
%% Defines
%% ====================================================================
-define(PACKET_TYPE, 0).
-define(TARGETS_REQUEST, 0).
-define(SOURCES_REQUEST, 1).
-define(SOURCES_REPLY, 2).
-define(NODES_DATA, 3).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_io/0, decode_external_packet/1]).

%% ====================================================================
%% Internal functions
%% ====================================================================
init_io()->
  Sources = ets:new(io_sources, []),
  Targets = ets:new(io_targets, []),

  {Sources, Targets}.
%% ====================================================================
%% Decodes binary packets and performs actions according to packet type
%% ====================================================================
decode_external_packet(Bin) ->
  {<<Packet>>, RemainData} = split_binary(Bin, 1),
  io:format("packet type ~p, remain data ~p~n", [Packet, RemainData]),
  case Packet of
    ?NODES_DATA ->
      <<X:16, Y:16>> = RemainData,
      io:format("Coords ~p~n", [{X,Y}]),
      case dojoNetwork:get_node_pid({X,Y,0}) of
        not_exist ->
          io:format("node ~p is not exist~n", [{X,Y,0}]);
        Pid ->
          io:format("node ~p found with result ~p ~n", [{X,Y,0}, Pid]),
          Pid ! {ap_from_source, self()}
      end,
      {ok, ap_transfered};

    ?TARGETS_REQUEST ->
      <<X:16, Y:16>> = RemainData,
      dojoNetwork:bind_nodes(self(), {X,Y,0}, 100),
      {ok, target_binded};

    ?SOURCES_REQUEST ->
      <<X:16, Y:16>> = RemainData,
      dojoNetwork:bind_nodes({X,Y,0}, self(), 100),
      {ok, source_binded};

    _Any ->
      {error, unknown_packet}
  end.


