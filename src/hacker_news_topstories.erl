-module(hacker_news_topstories).

%%% ============================================================================
%%% Behaviour
%%% ============================================================================

-behaviour(gen_server).

%%% ============================================================================
%%% API
%%% ============================================================================

-export([
    start_link/0,
    get/0
]).

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%%% ============================================================================
%%% Includes
%%% ============================================================================

-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Macros
%%% ============================================================================

-define(DEFAULT_STORIES_TOTAL, 50).
-define(REQ_INTERVAL, 5 * 60 * 1000).
-define(CACHE, cache_topstories).
-define(HACKER_NEWS, hacker_news).

%%% ============================================================================
%%% API functions
%%% ============================================================================

start_link() ->
    Opts = [{hibernate_after, 5000}, {spawn_opt,[{fullsweep_after, 0}]}],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

%%% ============================================================================
%%% gen_server callbacks functions
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% Get topstories
%% @end
%% -----------------------------------------------------------------------------
-spec get() -> {ok, Res :: list()} | term().

get() ->
    try 
        persistent_term:get(?CACHE)
    catch
        _:_ ->
            get_topstories()
    end.

init([]) ->
    gen_server:cast(?MODULE, ?CACHE),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(?CACHE = Key, State) ->
    Res = get_topstories(),
    ok = persistent_term:put(Key, Res),
    Interval = application:get_env(?HACKER_NEWS, topstories_interval, ?REQ_INTERVAL),
    {ok, _} = timer:apply_after(Interval, gen_server, cast, [?MODULE, Key]),
    {noreply, State#{Key => Res}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Get topstories
%% @end
%% -----------------------------------------------------------------------------
-spec get_topstories() -> {ok, Res :: list()} | term().

get_topstories() ->
    try 
        [_|_] = Url = application:get_env(?HACKER_NEWS, topstories_url, undefined),
        Total = application:get_env(?HACKER_NEWS, topstories_total, ?DEFAULT_STORIES_TOTAL),
        Retry = application:get_env(?HACKER_NEWS, topstories_retry, 10),
        ok = httpc:set_options([{keep_alive_timeout, 0}]),
        {ok, Data} = maybe_retry(Url, Retry),
        [_ | Res] = lists:sublist(string:tokens(get_http_resp_body(Data), ", "), Total + 1),
        {ok, Res}
    catch
        Class:Error ->
            Err = {Class, Error},
            ?LOG(error, "GET topstories errored: ~p", [Err]),
            Err
    end.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Returns HTTP response code
%% @end
%% -----------------------------------------------------------------------------
-spec get_http_resp_code(HttpcResult :: tuple()) -> Res :: pos_integer() | error.

get_http_resp_code({{_, Code, _}, _, _}) ->
    Code;
get_http_resp_code(_) ->
    error.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Returns HTTP response body
%% @end
%% -----------------------------------------------------------------------------
-spec get_http_resp_body(HttpcResult :: tuple()) -> Res :: list() | error.

get_http_resp_body({_, _, Body}) ->
    Body;
get_http_resp_body(_) ->
    error.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Retry N times if first call was failed
%% @end
%% -----------------------------------------------------------------------------
-spec maybe_retry(Url :: list(), Retry :: integer()) -> {ok, Res :: list()} | term().

maybe_retry(_, 0) ->
    error;
maybe_retry(Url, Retry) ->
    try 
        {ok, Data} = Res = httpc:request(get, {Url, []}, [], []),
        200 = get_http_resp_code(Data),
        Res
    catch
        _:_ ->
            ?LOG(warning, "GET topstories - was attempts left: ~p times", [Retry]),
            maybe_retry(Url, Retry - 1)
    end.
