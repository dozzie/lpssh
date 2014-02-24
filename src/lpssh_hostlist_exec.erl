%%%---------------------------------------------------------------------------
%%% @doc
%%%   Host inventory based on OS's `exec*()' function.
%%% @end
%%%---------------------------------------------------------------------------

-module(lpssh_hostlist_exec).
-behaviour(gen_lpssh_hostlist).

%% gen_lpssh_hostlist API
-export([hosts/1, hosts/2]).
-export([dependencies/0, init/1, terminate/1]).
-export([format_error/1]).

%% development
-export([]).

%%%---------------------------------------------------------------------------
%%% gen_lpssh_hostlist API
%%%---------------------------------------------------------------------------

%% @doc Return list of applications that need to be started to use this
%%   module.
%%
%% @spec dependencies() ->
%%   [atom()]

dependencies() ->
  [].

%% @doc Setup environment.
%%
%% @spec init(term()) ->
%%   {ok, State} | {error, Reason}

init(_Args) ->
  {ok, nostate}.

%% @doc Clean up on shutdown.
%%
%% @spec terminate(term()) ->
%%   any()

terminate(nostate = _State) ->
  ok.

%% @doc List all known hosts to be called.
%%
%% @spec hosts(term()) ->
%%   {ok, [string()]}

hosts(nostate = _State) ->
  % TODO: implement me
  {ok, ["localhost"]}.

%% @doc List hosts matching to the query to be called.
%%
%% @spec hosts(string(), term()) ->
%%   {ok, [string()]}

hosts(_Query, nostate = _State) ->
  % TODO: implement me
  {ok, ["localhost"]}.

%% @doc Return description of an error returned by {@link hosts/1}.
%%
%% @spec format_error({error,term()} | term()) ->
%%   string()

format_error({error,Reason} = _Error) ->
  format_error(Reason);
format_error(_) ->
  "(unrecognized error)".

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
