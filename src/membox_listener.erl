-module(membox_listener).

-behaviour(gen_nb_server).

-export([start_link/0]).

%% gen_nb_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, sock_opts/0]).
-export([new_connection/2]).

start_link() ->
	gen_nb_server:start_link(?MODULE, "0.0.0.0", 6379, []).

init([]) ->
	{ok, []}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Request, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

sock_opts() ->
	[{active, false},
	 {packet, line},
	 {recbuf, 16384},
	 {sndbuf, 4096},
	 {reuseaddr, true}].

new_connection(Sock, State) ->
	membox_worker:start(Sock),
	{ok, State}.
