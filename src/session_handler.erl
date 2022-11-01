-module(session_handler).

-export([
    init/2
]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            post(Req, State);
        _ ->
            Req1 = cowboy_req:reply(404, #{}, Req),
            {ok, Req1, State}
    end.

post(Req, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    BodyMap = jiffy:decode(Body, [return_maps]),

    Email = maps:get(<<"email">>, BodyMap),
    Password = maps:get(<<"password">>, BodyMap),

    case enduser:db_find_by_email(Email) of
        undefined ->
            Resp = cowboy_req:reply(401, Req);
        User ->
            case session_fsm:login(enduser:key(User), Password) of
                {error, wrong_credentials} ->
                    Resp = cowboy_req:reply(401, Req);
                {ok, Session} ->
                    io:format("Logged in\n"),
                    Ejson = session:ejson(Session),
                    Json = jiffy:encode(Ejson),
                    Req1 = web:remove_cookies(Req),
                    Req2 = web:set_cookies(Session, Req1),
                    Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req2)
            end
    end,
    {ok, Resp, State}.
