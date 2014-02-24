%%%---------------------------------------------------------------------------
%%% @doc
%%%   {@link error_logger} log handler to catch errors in outgoing SSH
%%%   connections. Module mitigates some problems with error reporting in
%%%   Erlang's SSH library.
%%% @end
%%%---------------------------------------------------------------------------

-module(lpssh_connection_log_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

-record(opts, {iodev}).

%%%---------------------------------------------------------------------------
%%% gen_event callbacks
%%%---------------------------------------------------------------------------

%% @doc Initialize event handler.
%%
%% @spec init(term()) ->
%%   {ok, term()}

init(IODev) ->
  {ok, #opts{iodev = IODev}}.

%% @doc Clean up after event handler.
terminate(_Arg, _Opts) ->
  ok.

%% @doc Handle incoming events.
%%   Main task is to intercept erroneous FSM termination (due to bad return
%%   value when password was asked and no user I/O allowed).
handle_event({error, _GL,
               {_Pid, "** State machine ~p terminating" ++ _ = _Fmt,
                 [_StateM, _LastMsg, _State, Data, ExitReason]}} = _Event,
             Opts = #opts{iodev = IODev}) ->
  % it's sort of a hack: I'm just sure the OTP (gen_fsm) will send a message
  % in this format
  FSMOpts = element(size(Data), Data),
  print_report(IODev, FSMOpts, ExitReason),
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

print_report(IODev, FSMOpts, ExitReason) ->
  Host = proplists:get_value(address, FSMOpts),
  %Port = proplists:get_value(port, FSMOpts),
  %User = proplists:get_value(user, FSMOpts),
  case ExitReason of
    {bad_return_value,{no_io_allowed,read_password}} ->
      io:fwrite(IODev, "connecting to ~p failed: password required~n",
                [Host]);
    _ ->
      io:fwrite(IODev, "connecting to ~p failed: ~1024p~n",
                [Host, ExitReason])
  end,
  ok.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
