%%%---------------------------------------------------------------------------
%%% @doc
%%%   {@link error_logger} log handler to catch errors in outgoing SSH
%%%   connections. Module mitigates some problems with error reporting in
%%%   Erlang's SSH library.
%%%
%%% @see lpssh_call_ssh
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(lpssh_ssh_connection_log_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

-record(opts, {}).

%%%---------------------------------------------------------------------------
%%% gen_event callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize event handler.
%%
%% @spec init(any()) ->
%%   {ok, term()}

init(_Args) ->
  {ok, #opts{}}.

%% @doc Clean up after event handler.
terminate(_Arg, _Opts) ->
  ok.

%% @doc Handle incoming events.
%%   Main task is to intercept erroneous FSM termination (due to bad return
%%   value when password was asked and no user I/O allowed).
handle_event({error, _GL,
               {_Pid, "** State machine ~p terminating" ++ _ = _Fmt,
                 [_StateM, _LastMsg, _State, Data, ExitReason]}} = _Event,
             Opts = #opts{}) ->
  % it's sort of a hack: I'm just sure the OTP (gen_fsm) will send a message
  % in this format
  FSMOpts = element(size(Data), Data),

  Host = proplists:get_value(address, FSMOpts),
  %Port = proplists:get_value(port, FSMOpts),
  %User = proplists:get_value(user, FSMOpts),

  case ExitReason of
    {bad_return_value,{no_io_allowed,read_password}} ->
      error_logger:info_report(lpssh_call_result,
                               {Host, error, password_required});
    _ ->
      error_logger:info_report(lpssh_call_result, {Host, died, ExitReason})
  end,
  {ok, Opts};

handle_event(_Event, Opts) ->
  % ignore
  {ok, Opts}.

%% @doc Handle {@link gen_event:call/3}.
handle_call(_Request, Opts) ->
  {ok, ok, Opts}.

%% @doc Handle incoming messages.
handle_info(_Message, Opts) ->
  {ok, Opts}.

%% @doc Handle code change.
code_change(_OldVsn, Opts, _Extra) ->
  {ok, Opts}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
