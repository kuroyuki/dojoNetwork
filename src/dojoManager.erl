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

init(_Args) ->
    %{ok, Listen} = gen_tcp:listen(8080, [{active,once}, {reuseaddr, true}, {keepalive, true}]),
    %io:format("Listen socket is ~p~n", [Listen]),
    %{ok, Accept} = gen_tcp:accept(Listen),
    %io:format("Accept socket is ~p~n", [Accept]),
    {ok, listen}.

handle_call(_Request, _From, State) ->
  {reply, test_reply, State}.

handle_cast(_Request,  State) ->
  {noreply, State}.

handle_info({node_message, {Node, Message, Data}}, State) ->
  io:format("~p node ~p : ~p ~p~n",[erlang:now(), Node, Message, Data]),
  {noreply, State};
handle_info({net_message, {Message, Data}}, State) ->
  io:format("~p Network : ~p ~p~n",[erlang:now(), Message, Data]),
  {noreply, State};
handle_info({tcp, Socket, Msg}, Listen)->
    io:format("dojoManager receives TCP message in info ~p~n",[Msg]),
    gen_tcp:send(Socket, response("{\"an\":{\"A\":{\"label\":\"Streaming Node A\",\"size\":2}}}")),
    inet:setopts(Socket, [{active, once}]),
    {noreply, Listen};
handle_info({tcp_closed, Socket}, LSocket)->
    io:format("socket closed ~p~n",[Socket]),
    {noreply, LSocket}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
        io_lib:fwrite(
            "HTTP/1.0 201 Created\nContent-Type: application/json\nContent-Length: ~p\n\n~s",
            [size(B), B])).



