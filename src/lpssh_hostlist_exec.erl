%%%---------------------------------------------------------------------------
%%% @doc
%%%   Host inventory based on OS's `exec*()' function.
%%%
%%% @TODO Change {@link os:cmd/1} to ports.
%%% @end
%%%---------------------------------------------------------------------------

-module(lpssh_hostlist_exec).
-behaviour(gen_lpssh_hostlist).

%% gen_lpssh_hostlist API
-export([hosts/1, hosts/2]).
-export([dependencies/0, init/1, terminate/1]).
-export([name/0]).
-export([format_error/1]).

%% development
-export([]).

%%%---------------------------------------------------------------------------

-record(opts, {command, query_style = regexp}).

%%%---------------------------------------------------------------------------
%%% gen_lpssh_hostlist API
%%%---------------------------------------------------------------------------

%% @doc Return name and type of this plugin.
%%
%% @spec name() ->
%%   {string(), call | inventory}

name() ->
  {"exec", inventory}.

%% @doc Return list of applications that need to be started to use this
%%   module.
%%
%% @spec dependencies() ->
%%   [atom()]

dependencies() ->
  [].

%% @doc Setup environment.
%%   In this case `Args' is of form `{re, Command :: string()}' (command is
%%   run as is and the `Query' is regexp) or `{arg, Command :: string()}'
%%   (`Query' is appended to the command before running). See {@link hosts/2}
%%   for details.
%%
%% @spec init(term()) ->
%%   {ok, State} | {error, Reason}

%init({re, Command} = _Args) ->
%  {ok, #opts{re_cmd = Command}};
%init({arg, Command} = _Args) ->
%  {ok, #opts{arg_cmd = Command}}.
init(Opts) ->
  case proplists:get_value(inventory_command, Opts) of
    undefined ->
      {error, command_undefined};
    Command ->
      case proplists:get_value(query_style, Opts) of
        undefined ->
          % default one, which is regexp
          {ok, #opts{command = Command}};
        "regexp" ->
          {ok, #opts{command = Command, query_style = regexp}};
        "argument" ->
          {ok, #opts{command = Command, query_style = argument}};
        _Any ->
          {error, badarg}
      end
  end.

%% @doc Clean up on shutdown.
%%
%% @spec terminate(term()) ->
%%   any()

terminate(_State) ->
  ok.

%% @doc List all known hosts to be called.
%%
%% @spec hosts(term()) ->
%%   {ok, [string()]}

hosts(_State = #opts{command = Command}) ->
  Output = os:cmd(Command),
  Hosts = string:tokens(Output, "\r\n"),
  {ok, Hosts}.

%% @doc List hosts matching to the query to be called.
%%   Depending on {@link init/1} option, the query is either string to be
%%   appended to the command or a regexp to filter the results.
%%
%% @spec hosts(string(), term()) ->
%%   {ok, [string()]}

hosts(Query, _State = #opts{command = Command, query_style = argument}) ->
  Output = os:cmd(Command ++ " " ++ Query),
  Hosts = string:tokens(Output, "\r\n"),
  {ok, Hosts};
hosts(Query, _State = #opts{command = Command, query_style = regexp}) ->
  % TODO: handle pattern errors
  {ok, Re} = re:compile(Query),
  Output = os:cmd(Command),
  Hosts = string:tokens(Output, "\r\n"),
  {ok, [H || H <- Hosts, re:run(H, Re, [{capture, none}]) == match]}.

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
