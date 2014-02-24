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
  [{hosts, 1}, {hosts, 2}, {format_error, 1},
    {dependencies, 0}, {init, 1}, {terminate, 1}];
behaviour_info(_Aspect) ->
  undefined.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
