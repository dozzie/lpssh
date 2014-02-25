%%%---------------------------------------------------------------------------
%%% @doc
%%%   {@link error_logger} log handler for remote execution statuses.
%%% @end
%%%---------------------------------------------------------------------------

-module(lpssh_exec_status_h).

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
%% @spec init(any()) ->
%%   {ok, term()}

init(IODev) ->
  {ok, #opts{iodev = IODev}}.

%% @doc Clean up after event handler.
terminate(_Arg, _Opts) ->
  ok.

%% @doc Handle incoming events.
%%   Events processed by this handler are of type `lpssh_call_result' and
%%   have data of following form: 
%%   `{Host :: string(), ExitType :: ok | error | died, Value :: term()}'.

handle_event({info_report, _GL, {_Pid, lpssh_call_result, Result}} = _Event,
             Opts = #opts{iodev = IODev}) ->
  % Result :: {Host :: string(), ExitType :: atom(), Value :: term()}
  case Result of
    {Host, ok, Value} ->
      io:fwrite(IODev, "[~s]= ~4096p~n", [Host, Value]),
      ok;
    {_Host, skip_error, _} ->
      % this is a bug to receive this message (it means that error for this
      % host was already sent), but let's ignore the message, as it could have
      % been sent accidentally
      ignore;
    {Host, error, Reason} ->
      io:fwrite(IODev, "connection to host ~s failed: ~4096p~n",
                       [Host, Reason]),
      ok;
    {Host, died, Reason} ->
      io:fwrite(IODev, "connection to host ~s failed (process died): ~4096p~n",
                       [Host, Reason]),
      ok;
    _Any ->
      % this should never happen, but just in case...
      ignore
  end,
  {ok, Opts};

handle_event(_Event, Opts) ->
  % ignore unknown errors
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
