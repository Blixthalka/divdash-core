-module(divdash_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    db:start_mnesia(),
    start_cowboy(),

    Res = divdash_sup:start_link(),
    io:format("Started Application\n"),
    Res.

start_cowboy() ->
    Dispatch = cowboy_router:compile([{'_', [
        {"/api/users", user_handler, []},
        {"/api/sessions", session_handler, []},
        {"/api/logout", logout_handler, [auth]},
        {"/api/dashboard", dashboard_handler, [auth]},
        {"/api/dividends", dividend_handler, [auth]},
        {"/api/dividends/date", date_handler, [auth]},
        {"/api/dividends/accumulated", accumulated_handler, [auth]},
        {"/api/instruments/[:isin]", instrument_handler, [auth]}
    ]}]),
    CowboyConfig = #{
        middlewares => [
            cowboy_router,
            api_authenticate,
            cowboy_handler
        ],
        env => #{
            dispatch => Dispatch
        }
    },
    CowboyOptions = [
        {port, 8082}
    ],
    {ok, _} = cowboy:start_clear(http_api, CowboyOptions, CowboyConfig).

stop(_State) ->
    ok = cowboy:stop_listener(http_api),
    ok.

%% internal functions
