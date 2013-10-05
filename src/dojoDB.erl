%%%-------------------------------------------------------------------
%%% @author Yuki
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2013 8:06 PM
%%%-------------------------------------------------------------------
-module(dojoDB).
-author("Yuki").

-record(node, {nodePos, nodePid}).
-record(synapse, {source, target, value}).

%% API
-export([init/0, insert_node/2, delete_node/1, get_node_pid/1, insert_synapse/3, delete_synapses/1,
  get_targets/1, get_sources/1]).

init()->
  mnesia:start(),
  mnesia:create_table(node,
    [{attributes, record_info(fields, node)}]),
  mnesia:create_table(synapse,
    [{type, bag},{attributes, record_info(fields, synapse)}]).

insert_node(NodePos, NodePid)->
 case get_node_pid(NodePos) of
   {ok, _NodePid} -> already_exist;
   {error, _} ->
     mnesia:dirty_write(#node{nodePos = NodePos, nodePid = NodePid}),
     node_added
  end.

delete_node(NodePid)->
  %deleting all synapse where this node as source
  mnesia:dirty_delete(synapse, NodePid),
  %get synapses where this node as target
  List = get_sources(NodePid),
  delete_synapses(List).

get_node_pid(NodePos)->
  case mnesia:dirty_read(node, NodePos) of
    [{node, NodePos, NodePid}] -> {ok, NodePid};
    [] -> {error, not_found}
  end.

insert_synapse(SourcePid, TargetPid, Value) ->
  case mnesia:dirty_match_object({synapse,  SourcePid, TargetPid, '_'}) of
    [] ->
      synapse_created;
    [_Some]  ->
      synapse_updated
  end,
  mnesia:dirty_write(#synapse{source= SourcePid, target = TargetPid, value=Value}).

delete_synapses(List)  ->
  lists:foreach(fun(X) -> mnesia:dirty_delete_object(synapse, X) end, List).

get_targets(SourcePid)->
  List = mnesia:dirty_match_object({synapse,  SourcePid, '_', '_'}),
  List.

get_sources(TargetPid)->
  List = mnesia:dirty_match_object({synapse,  '_', TargetPid, '_'}),
  List.




