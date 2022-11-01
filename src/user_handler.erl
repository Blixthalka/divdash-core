-module(user_handler).

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
    Password =  maps:get(<<"password">>, BodyMap),

    case enduser:db_find_by_email(Email) of
        undefined ->
            User = enduser:create(
                Email,
                Password
            ),
            {ok, UserWithKey} = enduser:db_create(User),

            {ok, Session} = session_fsm:login(enduser:key(UserWithKey), Password),
            Req1 = web:set_cookies(Session, Req),

            Json = jiffy:encode(enduser:ejson(UserWithKey)),

            Req2 = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>}, Json, Req1),
            {ok, Req2, State};
        _ ->
            Req1 = cowboy_req:reply(401, Req),
            {ok, Req1, State}
    end.