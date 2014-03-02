%%%---------------------------------------------------------------------------
%%% @doc
%%%   SSH remote call module.
%%% @end
%%%---------------------------------------------------------------------------

-module(lpssh_call_ssh).
-behaviour(gen_lpssh_call).

%% gen_lpssh_call API
-export([execute/4]).
-export([dependencies/0, init/1, terminate/1]).
-export([name/0]).
-export([format_error/1]).

%% SSH convenience wrappers (maybe somebody will find it useful)
-export([ssh_connect/3, ssh_disconnect/1, ssh_execute/3]).

%%%---------------------------------------------------------------------------

%% @type optlist() = [{Key :: atom(), Value :: term()} | atom()].

%% @type hostname() = string() | inet:ip_address().

%% @type ssh_conn_ref() =
%%   {{Host :: hostname(), Port :: integer()}, SSHConnRef :: term()}.

%% @type exec_result() =
%%   {exit, Code :: integer()} | {signal, Name :: string()}.

%%%---------------------------------------------------------------------------
%%% gen_lpssh_call API
%%%---------------------------------------------------------------------------

%% @doc Return name and type of this plugin.
%%
%% @spec name() ->
%%   {string(), call | inventory}

name() ->
  {"ssh", call}.

%% @doc Return list of applications that need to be started to use this
%%   module.
%%
%% @spec dependencies() ->
%%   [atom()]

dependencies() ->
  [ssh].

%% @doc Setup environment for SSH client.
%%
%% @spec init(term()) ->
%%   {ok, State} | {error, Reason}

init(_Args) ->
  % error reports from gen_fsm (e.g. "password required" error) should not go
  % to terminal; instead, they should be nicely formatted by
  % `lpssh_ssh_connection_log_h'
  error_logger:tty(false),
  error_logger:add_report_handler(lpssh_ssh_connection_log_h, group_leader()),
  {ok, nostate}.

%% @doc Clean up on shutdown.
%%
%% @spec terminate(term()) ->
%%   any()

terminate(nostate = _State) ->
  error_logger:delete_report_handler(lpssh_ssh_connection_log_h),
  ok.

%% @doc Execute command on remote host.
%%   Complex command (list of strings) is simply joined with spaces.
%%
%% @see init/1
%% @see terminate/1
%%
%% @spec execute(hostname(), gen_lpssh_call:command(), optlist(), term()) ->
%%   {ok, exec_result()} | skip_error | {error, Reason}

execute(HostSpec, [C | _Rest] = Command, Opts, State) when is_list(C) ->
  execute(HostSpec, string:join(Command, " "), Opts, State);
execute(HostSpec, Command, Opts, nostate = _State) ->
  case string:tokens(HostSpec, ":") of
    [Host]          -> Port = 22;
    [Host, PortStr] -> Port = list_to_integer(PortStr)
  end,
  case ssh_connect(Host, Port, Opts) of
    {ok, Conn} ->
      ExecResult = ssh_execute(Conn, Command, Opts),
      ssh_disconnect(Conn),
      case ExecResult of
        {error, _Reason} = Error ->
          Error;
        unknown ->
          {error, unknown};
        {exit, _Code} ->
          {ok, ExecResult};
        {signal, _SigName} ->
          {ok, ExecResult}
      end;
    {error, Reason} when is_list(Reason) ->
      % messy error report as a string (iolist); leave the error to be printed
      % by `lpssh_ssh_connection_log_h'
      skip_error;
    {error, normal} ->
      % connection abruptly closed by the server
      {error, connection_closed};
    {error, {'EXIT',_}} ->
      {error, operational_error};
    {error, _Reason} = Error ->
      Error
  end.

%% @doc Return description of an error returned by {@link execute/4}.
%%
%% @spec format_error({error,term()} | term()) ->
%%   string()

format_error({error,Reason} = _Error) ->
  format_error(Reason);
format_error(unknown) ->
  "exec result unknown";
format_error(skip) ->
  "(messy error; printed by `error_logger' handler)";
format_error(connection_closed) ->
  "connection abruptly closed by the server";
format_error(emfile) ->
  "too many open files";
format_error(operational_error) ->
  "operational error (too many open files?)";
format_error(econnrefused) ->
  "connection refused";
format_error(etimedout) ->
  "connection timeout";
format_error(nxdomain) ->
  "unknown host";
format_error(ehostunreach) ->
  "host unreachable";
format_error(enetunreach) ->
  "network unreachable";
format_error(_) ->
  "(unrecognized error)".

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
  case ssh_connection:session_channel(ConnRef, SSHTimeout) of
    {ok, Channel} ->
      case ssh_connection:exec(ConnRef, Channel, Command, SSHTimeout) of
        success ->
          ssh_read_output(SSHConnRef, Channel, fun print_line/3),
          ssh_read_exit(SSHConnRef, Channel, unknown);
        failure ->
          {error, ssh_exec_failure}
      end;
    {error, _Reason} = Error ->
      Error
  end.

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
