-module(session_fsm).

-behaviour(gen_statem).

-export([
    callback_mode/0,
    start_link/1,
    init/1,
    login/2,
    logout/1,
    is_authenticated/2,
    logged_in/3,
    logged_out/3
]).

login(UserKey, Password) ->
    try
        gen_statem:call(ref(UserKey), {login, Password})
    catch
        _:E->
            io:format("ERROR ~p\n", [E]),
            {error, wrong_credentials}
    end.

logout(UserKey) ->
    try
        gen_statem:call(ref(UserKey), {logout})
    catch
        _:_ ->
            ok
    end.

is_authenticated(UserKey, Token) ->
    try
        gen_statem:call(ref(UserKey), {is_authenticated, Token})
    catch
        _:E->
            io:format("ERROR ~p\n", [E]),
            false
    end.

callback_mode() ->
    state_functions.

start_link(User) ->
    io:format("~s ~p starting session_fsm\n", [enduser:key(User), enduser:email(User)]),
    gen_statem:start_link(ref(enduser:key(User)), ?MODULE, User, []).

init(User) ->
    Session = session:create(User),
    {ok, logged_out, Session, hibernate}.


logged_in({call, From}, {login, Password}, Session) ->
    a_login(From, Password, Session);
logged_in({call, From}, {logout}, Session) ->
    a_logout(From, Session);
logged_in({call, From}, {is_authenticated, Token}, Session) ->
    {keep_state_and_data, {reply, From, Token =/= undefined andalso Token =:= session:token(Session)}}.

logged_out({call, From}, {login, Password}, Session) ->
    a_login(From, Password, Session);
logged_out({call, From}, {logout}, _Session) ->
    {keep_state_and_data, {reply, From, ok}};
logged_out({call, From}, {is_authenticated, _Token}, _Session) ->
    {keep_state_and_data, {reply, From, false}}.

a_login(From, Password, Session) ->
    case enduser:pw_is_correct(Password, session:user(Session)) of
        true ->
            Session1 = session:token(db:generate_id(), Session),
            {next_state, logged_in, Session1, {reply, From, {ok, Session1}}};
        false ->
            Session1 = session:token(undefined, Session),
            {next_state, logged_out, Session1, {reply, From, {error, wrong_credentials}}}
    end.

a_logout(From, Session) ->
    Session1 = session:token(undefined, Session),
    {next_state, logged_out, Session1, {reply, From, ok}}.

ref(UserKey) ->
    {global, {?MODULE, UserKey}}.