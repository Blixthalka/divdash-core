-module(session).


-export([
    create/1,
    user/1,
    token/1,
    token/2,
    ejson/1
]).

-record(session, {
    user :: enduser:user(),
    token :: binary() | undefined
}).

create(User) ->
    #session{
        user = User,
        token = undefined
    }.

user(#session{user = User}) ->
    User.

token(#session{token = Token}) ->
    Token.

token(Token, Session) ->
    Session#session{token = Token}.

ejson(#session{user = User}) ->
    {[
        {user, enduser:key(User)}
    ]}.