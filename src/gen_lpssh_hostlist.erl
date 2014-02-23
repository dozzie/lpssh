%%%---------------------------------------------------------------------------
%%% @doc
%%%   lpssh host inventory module.
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_lpssh_hostlist).

-export([behaviour_info/1]).

%%%---------------------------------------------------------------------------

%% @doc Behaviour description.
behaviour_info(callbacks = _Aspect) ->
  [];
behaviour_info(_Aspect) ->
  undefined.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
