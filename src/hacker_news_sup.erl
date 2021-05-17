%%% ============================================================================
%%% @doc hacker_news top level supervisor.
%%% @end
%%% ============================================================================

-module(hacker_news_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1
    },
    ChildSpec = #{
        id => hacker_news_topstories_sup,
        start => {hacker_news_topstories_sup, start_link, []},
        type => supervisor,
        restart => permanent,
        shutdown => 5000
    },
    {ok, {SupFlags, [ChildSpec]}}.
