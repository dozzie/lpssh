%%%---------------------------------------------------------------------------
%%% @doc
%%%   lpssh transport module for executing commands on remote hosts.
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_lpssh_call).

-export([behaviour_info/1]).

%%%---------------------------------------------------------------------------

%% @doc Behaviour description.
behaviour_info(callbacks = _Aspect) ->
  [];
behaviour_info(_Aspect) ->
  undefined.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
