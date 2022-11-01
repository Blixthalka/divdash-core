-module(web).


-export([
    user_key_cookie/1,
    token_cookie/1,
    read_multipart_body/1,
    set_cookies/2,
    remove_cookies/1,
    add_filter_param/3
]).

-define(USER_KEY_COOKIE, <<"user">>).
-define(TOKEN_COOKIE, <<"token">>).

user_key_cookie(Req) ->
   get_cookie(?USER_KEY_COOKIE, Req).

token_cookie(Req) ->
    get_cookie(?TOKEN_COOKIE, Req).

get_cookie(CookieName, Req) ->
    case proplists:get_value(CookieName, cowboy_req:parse_cookies(Req), undefined) of
        <<"">> ->
            undefined;
        Value when is_binary(Value) ->
            Value;
        _ ->
            undefined
    end.

set_cookies(Session, Req) ->
    Req1 = cowboy_req:set_resp_cookie(?USER_KEY_COOKIE, enduser:key(session:user(Session)), Req),
    Req2 = cowboy_req:set_resp_cookie(?TOKEN_COOKIE, session:token(Session), Req1),
    Req2.

-spec remove_cookies(Req::cowboy_req:req()) -> cowboy_req:req().
remove_cookies(Req) ->
    lists:foldl(fun(K, Acc) ->
                        remove_cookie(K, Acc)
                end, Req, [?USER_KEY_COOKIE,
                           ?TOKEN_COOKIE]).

remove_cookie(Cookie, Req) ->
    cowboy_req:set_resp_cookie(Cookie, <<"">>, Req,
                                #{http_only => true,
                                    path => <<"/">>,
                                    secure => true,
                                    same_site => strict,
                                    max_age => 0}).


read_multipart_body(Req) ->
    read_multipart_body(Req, <<"">>).

read_multipart_body(Req0, Start) ->
    case cowboy_req:read_part(Req0) of
        {ok, _Headers, Req1} ->
            {ok, Body, Req} = cowboy_req:read_part_body(Req1),
            read_multipart_body(Req, <<Start/binary, Body/binary>>);
        {done, _Req} ->
            Start
    end.

add_filter_param(FilterName, QueryParams, Filter) ->
    case maps:get(FilterName, QueryParams, undefined) of
        undefined ->
            Filter;
        Value ->
            [{FilterName, Value}|Filter]
    end.