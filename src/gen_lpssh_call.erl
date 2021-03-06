%%%---------------------------------------------------------------------------
%%% @doc
%%%   lpssh transport module for executing commands on remote hosts.
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_lpssh_call).

-export([behaviour_info/1]).

-export_type([command/0, result/0]).

%%%---------------------------------------------------------------------------

%% @type command() = string() | [string()].
%%   Command to run, either as an argument to `sh -c' or as an argument to
%%   `exec*()' function. Not all transports make difference between these two.

-type command() :: string() | [string()].

%% @type result() = {ok, ExitCode :: integer(), Output :: string()}.
%%   Result of running the command: its exit code (hopefully 0) and its
%%   output.

-type result() :: {ok, ExitCode :: integer(), Output :: string()}.

%%%---------------------------------------------------------------------------

%% @doc Behaviour description.
behaviour_info(callbacks = _Aspect) ->
  [{execute, 4}, {format_error, 1},
    {dependencies, 0}, {init, 1}, {terminate, 1},
    {name, 0}];
behaviour_info(_Aspect) ->
  undefined.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
