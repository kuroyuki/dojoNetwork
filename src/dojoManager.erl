%% Copyright
-module(dojoManager).
-author("yuki").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
-record(state, {}).

init(_Args) ->
  {ok, #state{}}.

handle_call(test, _From, State) ->
   io:format("dojoManager test~n"),
  {reply, test_reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({node_message, {NodePos, Message, Data}}, State) ->
  io:format("~p node ~p : ~p ~p~n",[erlang:now(), NodePos, Message, Data]),
  {noreply, State};
handle_info({net_message, {Message, Data}}, State) ->
  io:format("~p Network : ~p ~p~n",[erlang:now(), Message, Data]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
