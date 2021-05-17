-module(hacker_news_SUITE).

%%% ==================================================================
%%% Common Tests Callbacks Exports
%%% ==================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1
]).

%%% ==================================================================
%%% Tests Exports
%%% ==================================================================

-export([
    get_top_topstories_api_must_failed/1,
    get_top_topstories_api_must_ok/1
]).

%%% ==================================================================
%%% Specification
%%% ==================================================================

-type config() :: [{atom(), term()}].

%%% ==================================================================
%%% Common Tests Callbacks
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Init all groups
%% @end
%% -------------------------------------------------------------------
-spec all() -> lists:list().

all() ->
    [
        {group, failed},
        {group, successed}
    ].

%% -------------------------------------------------------------------
%% @doc
%% Groups
%% @end
%% -------------------------------------------------------------------
-spec groups() -> lists:list().

groups() ->
    [
        {failed, [sequence], [
            get_top_topstories_api_must_failed
        ]},
        {successed, [sequence], [
            get_top_topstories_api_must_ok
        ]}
    ].

%% -------------------------------------------------------------------
%% @doc
%% Init per suite
%% @end
%% -------------------------------------------------------------------
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(hacker_news),
    Config.

%% -------------------------------------------------------------------
%% @doc
%% End per suite
%% @end
%% -------------------------------------------------------------------
-spec end_per_suite(config()) -> config().

end_per_suite(Config) ->
    Config.

%%% ==================================================================
%%% Test cases
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% get_top_topstories_api_must_failed
%% @end
%% -------------------------------------------------------------------
-spec get_top_topstories_api_must_failed(config()) -> ok.

get_top_topstories_api_must_failed(_Config) ->
    Url = application:get_env(hacker_news, topstories_url, undefined),
    ok = application:set_env(hacker_news, topstories_url, [1, 2, 3]),
    case hacker_news_topstories:get() of
        {error, _} = Error ->
            ok = application:set_env(hacker_news, topstories_url, Url),
            ct:comment("GET Topstories Errored With = ~p", [Error]);
        Any ->
            ct:fail(Any)
    end.

%% -------------------------------------------------------------------
%% @doc
%% get_top_topstories_api_must_ok
%% @end
%% -------------------------------------------------------------------
-spec get_top_topstories_api_must_ok(config()) -> ok.

get_top_topstories_api_must_ok(_Config) ->
    case hacker_news_topstories:get() of
        {ok, Topstories} when erlang:length(Topstories) =:= 50 ->
            ct:comment("GET Topstorie = ~p", [Topstories]);
        {error, Reason} ->
            ct:fail(Reason)
    end.
