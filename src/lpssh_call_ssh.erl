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

%% SSH convenience wrappers (maybe somebody will find it useful)
-export([ssh_connect/3, ssh_disconnect/1, ssh_execute/3]).

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
%%   Complex command (list of strings) is simply joined with spaces.
%%
%%   SSH port defaults to 22.
%%
%% @spec execute(hostname(), gen_lpssh_call:command(), optlist()) ->
%%   {ok, ssh_conn_ref()} | {error, Reason}

execute(Host, Command, Opts) ->
  execute(Host, default, Command, Opts).

%% @doc Execute command on remote host.
%%   Complex command (list of strings) is simply joined with spaces.
%%
%%   SSH port defaults to 22.
%%
%% @spec execute(hostname(), portnum(), gen_lpssh_call:command(), optlist()) ->
%%   {ok, ssh_conn_ref()} | {error, Reason}

execute(Host, default, Command, Opts) ->
  execute(Host, 22, Command, Opts);
execute(Host, Port, [C | _Rest] = Command, Opts) when is_list(C) ->
  execute(Host, Port, string:join(Command, " "), Opts);
execute(Host, Port, Command, Opts) ->
  case ssh_connect(Host, Port, Opts) of
    {ok, Conn} ->
      Result = ssh_execute(Conn, Command, Opts),
      ssh_disconnect(Conn),
      case Result of
        {error, _Reason} = Error ->
          Error;
        unknown ->
          {error,unknown};
        {exit,_Code} = Result ->
          {ok, Result};
        {signal,_SigName} = Result ->
          {ok, Result}
      end;
    {error, _Reason} = Error ->
      Error
  end.

%%%---------------------------------------------------------------------------
%%% SSH convenience wrappers
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

%% @doc Execute a command on specified connection.
%%   All the output from command is printed to standard output (that is, to
%%   group leader).
%%
%%   Function returns how the remote command exited.
%%
%% @see print_line/3
%%
%% @spec ssh_execute(ssh_conn_ref(), string(), optlist()) ->
%%   {exit, integer()} | {signal, string()} | unknown | {error, Reason}

ssh_execute({{_Host, _Port}, ConnRef} = SSHConnRef, Command, _Opts) ->
  SSHTimeout = 5000, % 5000ms % TODO: retrieve timeout from Opts
  % TODO: error handling ({ok, Channel} | {error, Reason})
  {ok, Channel} = ssh_connection:session_channel(ConnRef, SSHTimeout),
  % TODO: error handling (success | failure)
  success = ssh_connection:exec(ConnRef, Channel, Command, SSHTimeout),
  ssh_read_output(SSHConnRef, Channel, fun print_line/3),
  ssh_read_exit(SSHConnRef, Channel, unknown).

%% @doc Read output from command and print it to standard output.
%%
%% @see print_line/3
%%
%% @spec ssh_read_output(ssh_conn_ref(), integer(), fun()) ->
%%   ok

ssh_read_output({{Host, Port}, ConnRef} = SSHConnRef, Channel, PrintLine) ->
  receive
    {ssh_cm, ConnRef, {data, Channel, _DataTypeCode, Data}} ->
      [PrintLine(Host, Port, Line) ||
        Line <- binary:split(Data, <<"\n">>, [global, trim])],
      ssh_read_output(SSHConnRef, Channel, PrintLine);
    {ssh_cm, ConnRef, {eof, Channel}} ->
      ok
  end.

%% @doc Read exit status of the command (signal or exit).
%%
%% @spec ssh_read_exit(ssh_conn_ref(), integer(), term()) ->
%%   {exit, integer()} | {signal, string()} | unknown | {error, Reason}

ssh_read_exit({{_Host, _Port}, ConnRef} = SSHConnRef, Channel, Result) ->
  receive
    {ssh_cm, ConnRef, {exit_signal, Channel, ExitSignal, _ErrorMsg, _Lang}} ->
      % ExitSignal :: string()
      ssh_read_exit(SSHConnRef, Channel, {signal, ExitSignal});
    {ssh_cm, ConnRef, {exit_status, Channel, ExitStatus}} ->
      % ExitStatus :: integer()
      ssh_read_exit(SSHConnRef, Channel, {exit, ExitStatus});
    {ssh_cm, ConnRef, {closed, Channel}} ->
      Result
  after 1000 ->
      {error, timeout}
  end.

%% @doc Print line to standard output.
%%
%% @spec print_line(string(), integer(), string() | binary()) ->
%%   ok

print_line(Host, _Port, Line) ->
  io:fwrite("[~s]> ~s~n", [Host, Line]).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
