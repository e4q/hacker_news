%%% ============================================================================
%%% @doc hacker_news public API
%%% @end
%%% ============================================================================

-module(hacker_news_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    hacker_news_sup:start_link().

stop(_State) ->
    ok.
