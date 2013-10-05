%% Copyright
-module(dojoNetwork).
-author("yuki").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create_node/1, bind_nodes/3, add_voltage/2, get_node_pid/1]).
%% ====================================================================
%% API functions
%% ====================================================================
start_link()->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_node(Position) ->
  gen_server:cast(?MODULE, {new_node, Position}).

bind_nodes(Source, Target, Data) ->
  gen_server:cast(?MODULE, {bind_nodes, Source, Target, Data}).

add_voltage(Voltage, Node) ->
  gen_server:cast(?MODULE, {add_voltage, Voltage, Node}).

get_node_pid(Coords) ->
  gen_server:call(?MODULE, {get_node_pid, Coords}).
%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
  Result :: {ok, State}
  | {ok, State, Timeout}
  | {ok, State, hibernate}
  | {stop, Reason :: term()}
  | ignore,
  State :: term(),
  Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
  Filename = "./../net.dojo",
  dojoManager!{net_message, {"config data from", Filename}},
  {ok, S} = file:open(Filename, read),

  Nodes = ets:new(nodes, []),

  create_net(Nodes, S),

  {ok, Nodes}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.Paramserlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, Nodes :: term()) -> Result when
  Result :: {reply, Reply, NewState}
  | {reply, Reply, NewState, Timeout}
  | {reply, Reply, NewState, hibernate}
  | {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {stop, Reason, Reply, NewState}
  | {stop, Reason, NewState},
  Reply :: term(),
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity,
  Reason :: term().
%% ====================================================================
handle_call({get_node_pid, Node}, _From, Nodes) ->
  Reply =  ets:lookup(Nodes, Node),
  case Reply of
    [{_SourcePos, SourcePid}]  ->
      {reply, SourcePid, Nodes};
    _Any ->
      {reply, not_exist, Nodes}
  end.



%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), Nodes :: term()) -> Result when
  Result :: {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {stop, Reason :: term(), NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({new_node, Position}, Nodes) ->
  dojoManager!{net_message, {"casting node", Position}},
  insert_node(Nodes, Position),
  {noreply, Nodes};

handle_cast({bind_nodes, Source, Target, _Data}, Nodes) when is_pid(Source) ->
  case ets:lookup(Nodes, Target) of
    [{_TargetPos, TargetPid}] ->
      TargetPid ! {add_source, Source};
    _Any ->
      dojoManager!{net_message, {"no such target", Target}}
  end;
handle_cast({bind_nodes, Source, Target, _Data}, Nodes) when is_pid(Target) ->
  case ets:lookup(Nodes, Source) of
    [{_SourcePos, SourcePid}] ->
      SourcePid ! {add_target, Target};
    _Any ->
      dojoManager!{net_message, {"no such source", Source}}
  end;
handle_cast({bind_nodes, Source, Target, Data}, Nodes) ->
  case ets:lookup(Nodes, Source) of
    [{_SourcePos, SourcePid}] ->
      case ets:lookup(Nodes, Target) of
        [{_TargetPos, TargetPid}] ->
            {X1,Y1,Z1} = Source,
            {X2,Y2,Z2} = Target,
            XDist = X2-X1,
            YDist = Y2-Y1,
            ZDist = Z2-Z1,
            DistanceXY = math:sqrt(XDist*XDist + YDist*YDist),

            Distance =  math:sqrt(DistanceXY*DistanceXY + ZDist*ZDist),
          SourcePid ! {add_target, TargetPid},
          TargetPid ! {add_source, SourcePid, Distance, Data};
        _Any ->
          dojoManager!{net_message, {"no such target", Target}}
      end;
    _Any ->
      dojoManager!{net_message, {"no such source", Source}}
  end,

  {noreply, Nodes};

handle_cast({add_voltage, Voltage, Node}, Nodes) ->
  case ets:lookup(Nodes, Node) of
    [{_SourcePos, SourcePid}] ->
      SourcePid!{add_voltage, Voltage};
    _Any ->
      dojoManager!{net_message, {"no such node", Node}}
  end,
  {noreply, Nodes}.



%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), Nodes :: term()) -> Result when
  Result :: {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {stop, Reason :: term(), NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Any, Nodes) ->
  io:format("Handle info ~p~n", [Any]),
  {noreply, Nodes}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
  Reason :: normal
  | shutdown
  | {shutdown, term()}
  | term().
%% ====================================================================
terminate(Reason, Nodes) ->
  io:format("Network terminated~n"),
  Msg = {kill, Reason},
  send_message_broadcast(Msg, Nodes),
  ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
  Result :: {ok, NewState :: term()} | {error, Reason :: term()},
  OldVsn :: Vsn | {down, Vsn},
  Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
create_net(Table, Stream)->

  case io:read(Stream, '') of
    eof ->
      ok;

    {ok, [Source, Target, Data]} ->

      SourcePid = insert_node(Table, Source),

      TargetPid = insert_node(Table, Target),

      {X1,Y1,Z1} = Source,
      {X2,Y2,Z2} = Target,
      XDist = X2-X1,
      YDist = Y2-Y1,
      ZDist = Z2-Z1,
      DistanceXY = math:sqrt(XDist*XDist + YDist*YDist),

      Distance =  math:sqrt(DistanceXY*DistanceXY + ZDist*ZDist),

      SourcePid ! {add_target, TargetPid},
      TargetPid ! {add_source, SourcePid, Distance, Data},

      create_net(Table, Stream);

    Any ->
      dojoManager!{net_message, {"net is not created", Any}}
  end.

insert_node(Table, Node)->
  case ets:lookup(Table, Node) of
    [] ->
      NodePid = dojoNode:start_link(Node),
      ets:insert(Table, {Node, NodePid}),
      NodePid;

    [{_Node, NodePid}] ->
      NodePid;
    Any ->
      dojoManager!{net_message, {"node is not inserted", Any}}
  end.

send_message_broadcast(Msg, Table) ->
    case ets:first(Table) of
        '$end_of_table' ->
            ok;
        Node ->
            [{_Node, NodePid}] = ets:lookup(Table, Node),
            NodePid!Msg,
            send_message_broadcast(Msg, Node, Table)
    end.
send_message_broadcast(Msg, Next, Table) ->
    case ets:next(Table, Next) of
        '$end_of_table' ->
            ok;
        Node ->
            [{_Node, NodePid}] = ets:lookup(Table, Node),
            NodePid!Msg,
            send_message_broadcast(Msg, Node, Table)
    end.