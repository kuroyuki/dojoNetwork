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
%-record(synapse, {source, target, weight}).

%% API
-export([init/0, insert_node/2, delete_node/1, get_node_pid/1, get_node_coords/1]).

init()->
  mnesia:start(),
  mnesia:create_table(node,
    [{attributes, record_info(fields, node)}]).

insert_node(NodePos, NodePid)->
 case get_node_pid(NodePos) of
   {error, _} ->
     mnesia:dirty_write(#node{nodePos = NodePos, nodePid = NodePid}),
     {ok, node_added};
   NodePid ->
     {error, already_exist}
  end.

delete_node(NodePid)->
  %deleting all synapse where this node as source
  mnesia:dirty_delete(node, NodePid).

get_node_pid(NodePos)->
  case mnesia:dirty_read(node, NodePos) of
    [{node, NodePos, NodePid}] ->  NodePid;
    _Any -> {error, not_found}
  end.

get_node_coords(Node) ->
  case mnesia:dirty_match_object(#node{nodePos = '_', nodePid=Node}) of
      []->{error,not_exist};
      [{node, NodePos, Node}]  -> {ok,NodePos}
  end.


