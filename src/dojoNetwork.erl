%% Copyright
-module(dojoNetwork).
-author("yuki").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create_node/1, bind_nodes/3, add_voltage/2, get_node_pid/1, delete_node/1]).
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

delete_node(Coords) ->
  gen_server:cast(?MODULE, {delete_node, Coords}).
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
  dojoDB:init(),
  {ok, S} = file:open(Filename, read),
  create_net(S),

  {ok, []}.


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
handle_call({get_node_pid, Node}, _From, []) ->
  Reply =  dojoDB:get_node_pid(Node),
  case Reply of
    {ok, NodePid}  ->
      {reply, NodePid};
    {error, not_found} ->
      {reply, not_found}
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
handle_cast({new_node, Node}, []) ->
  dojoManager!{net_message, {"casting node", Node}},
  insert_node(Node),
  {noreply, []};

handle_cast({bind_nodes, Source, Target, Data}, []) ->
  case dojoDB:get_node_pid(Source) of
    {ok, SourcePid}  ->
      case dojoDB:get_node_pid(Target) of
        {ok, TargetPid}  ->
            {X1,Y1,Z1} = Source,
            {X2,Y2,Z2} = Target,
            XDist = X2-X1,
            YDist = Y2-Y1,
            ZDist = Z2-Z1,
            DistanceXY = math:sqrt(XDist*XDist + YDist*YDist),

            Distance =  math:sqrt(DistanceXY*DistanceXY + ZDist*ZDist),

            TargetPid!{add_source, Source, Target, Data},
            SourcePid!{add_target, Source, Target},

            dojoManager!{net_message, {"nodes binded", [SourcePid, TargetPid]}} ;
        _Any ->
          dojoManager!{net_message, {"no such target", Target}}
      end;
    _Any ->
      dojoManager!{net_message, {"no such source", Source}}
  end,
  {noreply, []};

handle_cast({add_voltage, _Voltage, Node}, []) ->
  dojoManager!{net_message, {"add voltage depricated", Node}},
  {noreply, []};
handle_cast({delete_node, Node}, []) ->
  case dojoDB:get_node_pid(Node) of
    {ok, NodePid}  ->
      dojoManager!{net_message, {"send kill message to", NodePid}},
      NodePid!{kill, user_request};
    _Any ->
      dojoManager!{net_message, {"no such node", Node}}
  end,
  {noreply, []}.




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
handle_info(Any, []) ->
  dojoManager!{net_message, {"Handle info", Any}},
  {noreply, []}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
  Reason :: normal
  | shutdown
  | {shutdown, term()}
  | term().
%% ====================================================================
terminate(Reason, []) ->
  dojoManager!{net_message, {"Network terminated", Reason}},
  Msg = {kill, Reason},
  dojoDB:send_message_broadcast(Msg),
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
create_net(Stream)->
   case io:read(Stream, '') of
    eof ->
      ok;

    {ok, [Source, Target, Data]} ->
      insert_node(Source),
      {ok,SourcePid} = dojoDB:get_node_pid(Source),
      insert_node(Target),
      {ok,TargetPid} = dojoDB:get_node_pid(Target),

      {X1,Y1,Z1} = Source,
      {X2,Y2,Z2} = Target,
      XDist = X2-X1,
      YDist = Y2-Y1,
      ZDist = Z2-Z1,
      DistanceXY = math:sqrt(XDist*XDist + YDist*YDist),

      Distance =  math:sqrt(DistanceXY*DistanceXY + ZDist*ZDist),

      dojoDB:insert_synapse(SourcePid,TargetPid, {Distance, Data}),

      create_net(Stream);

    Any ->
      dojoManager!{net_message, {"net is not created", Any}}
  end.

insert_node(Node)->
  case dojoDB:get_node_pid(Node) of
    {error, not_found} ->
      NodePid = dojoNode:start_link(),
      dojoDB:insert_node(Node, NodePid);
    {ok, _NodePid} ->
      already_exist
  end.
