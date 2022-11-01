-module(api_authenticate).

-behaviour(cowboy_middleware).

-export([
    execute/2
]).

execute(Req, Env) ->
    Opts = maps:get(handler_opts, Env, []),
    case lists:member(auth, Opts) of
        true ->
            case {web:user_key_cookie(Req), web:token_cookie(Req)} of
                {undefined, _} ->
                    {stop, cowboy_req:reply(401, Req)};
                {_, undefined} ->
                    {stop, cowboy_req:reply(401, Req)};
                {UserKey, Token} ->
                    case session_fsm:is_authenticated(UserKey, Token) of
                        true ->
                            {ok, Req, Env};
                        false ->
                            {stop, cowboy_req:reply(401, Req)}
                    end
            end;
        false ->
            {ok, Req, Env}
    end.

