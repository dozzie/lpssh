%%%---------------------------------------------------------------------------
%%% @doc
%%%   Wrappers for running commands on remote hosts.
%%% @end
%%%---------------------------------------------------------------------------

-module(lpssh).

%% public API
-export([run/3, run/4]).

-export([execute/5]).

%%%---------------------------------------------------------------------------
%%% types {{{

%% @type host_name() = string().
%%
%%   Name of a host to be called.

%% @type host_list_mod() =
%%   atom() | {Module :: atom(), Args :: term()}.
%%
%%   Module to retrieve list of hosts to be called. If only the module's name
%%   specified, `Args' default to `[]'.

%% @type host_query() = string().
%%
%%   Query passed to the module specified as {@type host_list_mod()}.

%% @type call_mod() =
%%   atom() | {Module :: atom(), Args :: term()}.
%%
%%   Module used to call hosts. If only the module's name specified, `Args'
%%   default to `[]'.

%% @type command() = string().
%%
%%   Command to run on remote hosts.

%% @type exec_result() =
%%   {Host :: string(), ExitType :: ok | error | skip_error | died,
%%     Value :: term()}.
%%
%%   How did the call ended. If execution was successful on a protocol level,
%%   `ExitType' equals to `ok' and `Value' carries the value returned from
%%   call. If there was a protocol error, `ExitType' is `error' (reason to be
%%   reported is in `Value') or `skip_error' (reason was already reported and
%%   `Value' has no meaning). If `ExitType' is `died', `Value' is termination
%%   reason.

%%% }}}
%%%---------------------------------------------------------------------------
%%% public API
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% run(HostListSpec, CallSpec, Query, Command) {{{

%% @doc Run a command on a list of hosts (limited by `Query').
%%
%% @spec run(host_list_mod(), call_mod(), host_query(), command()) ->
%%   [exec_result()]

%% Add default args to `HostListSpec'
run(HostListSpec, CallSpec, Query, Command) when is_atom(HostListSpec) ->
  run({HostListSpec, []}, CallSpec, Query, Command);

%% Add default args to `CallSpec'
run(HostListSpec, CallSpec, Query, Command) when is_atom(CallSpec) ->
  run(HostListSpec, {CallSpec, []}, Query, Command);

%% List the hosts and pass them to execution
run({HLMod, HLArgs} = _HostListSpec, CallSpec, Query, Command) ->
  % TODO: handle errors
  [start_rec(App) || App <- HLMod:dependencies()],
  {ok, HLState} = HLMod:init(HLArgs),
  {ok, Hosts} = HLMod:hosts(Query, HLState),
  HLMod:terminate(HLState),
  run(Hosts, CallSpec, Command).

%% }}}
%%----------------------------------------------------------
%% run(HostListSpec, CallSpec, Command) {{{

%% @doc Run a command on a list of (specified/all listed) hosts.
%%
%% @spec run([host_name()] | host_list_mod(), call_mod(), command()) ->
%%   [exec_result()]

%% Add default args to `HostListSpec'
run(HostListSpec, CallSpec, Command) when is_atom(HostListSpec) ->
  run({HostListSpec, []}, CallSpec, Command);

%% Add default args to `CallSpec'
run(HostListSpec, CallSpec, Command) when is_atom(CallSpec) ->
  run(HostListSpec, {CallSpec, []}, Command);

%% List the hosts and pass them to execution
run({HLMod, HLArgs} = _HostListSpec, CallSpec, Command) ->
  % TODO: handle errors
  [start_rec(App) || App <- HLMod:dependencies()],
  {ok, HLState} = HLMod:init(HLArgs),
  {ok, Hosts} = HLMod:hosts(HLState),
  HLMod:terminate(HLState),
  run(Hosts, CallSpec, Command);

%% Hosts were already listed, now what's left is to spawn children and report
%% their values.
run(Hosts, {CMod, CArgs} = _CallSpec, Command)
when is_list(Hosts) ->
  % TODO: handle errors
  [start_rec(App) || App <- CMod:dependencies()],
  {ok, CState} = CMod:init(CArgs),

  CallArgs = [Command, CMod, CState, self()],

  Monitors = [
    {H, spawn_monitor(?MODULE, execute, [H | CallArgs])} ||
    H <- Hosts
  ],
  Results = receive_results(Monitors),

  CMod:terminate(CState),
  Results.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% helper functions
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% execute(Host, Command, CMod, CState, ResultTo) {{{

%% @private
%% @doc Helper function to execute command on a remote host using specified
%%   call module.
%%
%%   Function to be used together with {@link receive_results/1}.
%%
%% @spec execute(host_name(), command(), call_mod(), term(), pid()) ->
%%   ok

execute(Host, Command, CMod, CState, ResultTo) ->
  case CMod:execute(Host, Command, [], CState) of
    {ok, Result} ->
      ResultTo ! {result, self(), ok, Result};
    {error, Reason} ->
      ResultTo ! {result, self(), error, Reason};
    skip_error ->
      ResultTo ! {result, self(), skip_error, skip_error}
  end,
  ok.

%% }}}
%%----------------------------------------------------------
%% receive_results(Hosts) {{{

%% @doc Receive results sent by {@link execute/5} or process termination
%%   reason.
%%
%% @spec receive_results([ {host_name(), {pid(), reference()}} ]) ->
%%   [exec_result()]

receive_results([]) ->
  [];
receive_results([{Host, {Pid, MonRef}} | Rest] = _Children) ->
  receive
    {result, Pid, skip_error, _} ->
      erlang:demonitor(MonRef, [flush]),
      % don't report this error, the report was already sent
      [{Host, skip_error, skip_error} | receive_results(Rest)];
    {result, Pid, ExitType, Result} ->
      erlang:demonitor(MonRef, [flush]),
      error_logger:info_report(lpssh_call_result, {Host, ExitType, Result}),
      [{Host, ExitType, Result} | receive_results(Rest)];
    {'DOWN', MonRef, _Type, Pid, ExitReason} ->
      error_logger:info_report(lpssh_call_result, {Host, died, ExitReason}),
      [{Host, died, ExitReason} | receive_results(Rest)]
  end.

%% }}}
%%----------------------------------------------------------
%% start_rec(App) {{{

%% @doc Start application along with its dependencies (recursively).
%%
%% @spec start_rec(atom()) ->
%%   ok | {error, Reason}

start_rec(App) ->
  case application:start(App) of
    {error,{not_started,AppDep}} ->
      case start_rec(AppDep) of
        ok    -> start_rec(App); % try again
        Error -> Error
      end;
    {error,{already_started,App}} ->
      ok; % that's fine if an application is started already
    ok ->
      ok;
    {error,_Reason} = Error ->
      Error
  end.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
