%%%---------------------------------------------------------------------------
%%% @doc
%%%   SSH remote call module.
%%%
%%% @see lpssh_keyfile
%%% @end
%%%---------------------------------------------------------------------------

-module(lpssh_call_ssh).
-behaviour(gen_lpssh_call).

%% gen_lpssh_call API
-export([execute/3, execute/4]).

%%%---------------------------------------------------------------------------

%% @type optlist() = [{Key :: atom(), Value :: term()} | atom()].

%% @type hostname() = string() | inet:ip_address().

%% @type portnum() = integer() | default.

%% @type ssh_conn_ref() =
%%   {{Host :: hostname(), Port :: portnum()}, SSHConnRef :: term()}.

%%%---------------------------------------------------------------------------
%%% gen_lpssh_call API
%%%---------------------------------------------------------------------------

%% @doc Execute command on remote host.
%%
%%   SSH port defaults to 22.
%%
%% @spec execute(hostname(), gen_lpssh_call:command(), optlist()) ->
%%   {ok, ssh_conn_ref()} | {error, Reason}

execute(Host, Command, Opts) ->
  execute(Host, default, Command, Opts).

%% @doc Execute command on remote host.
%%
%%   SSH port defaults to 22.
%%
%% @spec execute(hostname(), portnum(), gen_lpssh_call:command(), optlist()) ->
%%   {ok, ssh_conn_ref()} | {error, Reason}

execute(Host, default, Command, Opts) ->
  execute(Host, 22, Command, Opts);
execute(_Host, _Port, _Command, _Opts) ->
  'TODO'.

%%%---------------------------------------------------------------------------
%%% internal functions
%%%---------------------------------------------------------------------------

%% @doc Connect to remote host.
%%
%% @spec ssh_connect(string(), integer(), list()) ->
%%   {ok, ssh_conn_ref()} | {error, Reason}

ssh_connect(Host, Port, _Opts) ->
  % TODO: build options dynamically
  SSHOpts = [
    {user, "root"},
    {key_cb, lpssh_ssh_keyfile},
    {user_interaction, false}, % don't ask user for password
    {silently_accept_hosts, true}
  ],
  case ssh:connect(Host, Port, SSHOpts) of
    {ok, ConnRef} ->
      {ok, {{Host, Port}, ConnRef}};
    {error, _Reason} = Error ->
      Error
  end.

%% @doc Close connection to remote host.
%%
%% @spec ssh_disconnect(ssh_conn_ref()) ->
%%   ok

ssh_disconnect({{_Host, _Port}, ConnRef} = _SSHConnRef) ->
  ssh:close(ConnRef).

ssh_execute({{_Host, _Port}, ConnRef} = SSHConnRef, Command, _Opts) ->
  SSHTimeout = 5000, % 5000ms % TODO: retrieve timeout from Opts
  % TODO: error handling ({ok, Channel} | {error, Reason})
  {ok, Channel} = ssh_connection:session_channel(ConnRef, SSHTimeout),
  % TODO: error handling (success | failure)
  success = ssh_connection:exec(ConnRef, Channel, Command, SSHTimeout),
  % TODO: read exit code
  ssh_read_output(SSHConnRef, Channel, fun print_line/3),
  ok.

ssh_read_output({{Host, Port}, ConnRef} = SSHConnRef, Channel, PrintLine) ->
  receive
    {ssh_cm, ConnRef, {data, Channel, _DataTypeCode, Data}} ->
      [PrintLine(Host, Port, Line) ||
        Line <- binary:split(Data, <<"\n">>, [global, trim])],
      ssh_read_output(SSHConnRef, Channel, PrintLine);
    {ssh_cm, ConnRef, {eof, Channel}} ->
      % TODO: wait for exit_signal or exit_status
      % {ssh_cm, ConnRef,
      %   {exit_signal, Channel, ExitSignal :: string(), _ErrorMsg, _Lang}}
      % {ssh_cm, ConnRef,
      %   {exit_status, Channel, ExitStatus :: integer()}}
      % {ssh_cm, ConnRef, {closed, Channel}}
      % after ... -> unknown
      %
      % {exit, ExitStatus}
      % {signal, ExitSignal}
      % unknown
      ok
  end.

print_line(Host, _Port, Line) ->
  io:fwrite("[~s]> ~s~n", [Host, Line]).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
