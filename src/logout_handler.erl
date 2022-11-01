-module(logout_handler).

-export([
    init/2
]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"DELETE">> ->
            delete(Req, State);
        _ ->
            Req1 = cowboy_req:reply(404, #{}, Req),
            {ok, Req1, State}
    end.

delete(Req, State) ->
    UserKey = web:user_key_cookie(Req),
    ok = session_fsm:logout(UserKey),
    Req1 = web:remove_cookies(Req),
    {ok, cowboy_req:reply(200, Req1), State}.
