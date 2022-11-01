-module(user_supervisor).

-behaviour(supervisor).

-export([
    init/1,
    start_link/1
]).

start_link(User) ->
    supervisor:start_link({global, {?MODULE, enduser:key(User)}}, ?MODULE, User).

init(User) ->
    MaxRestart = 1000,
    MaxTime = 3600,

    SessionSupervisor = {
        {session_fsm, enduser:key(User)},
        {session_fsm, start_link, [User]},
        permanent,
        10000,
        worker,
        [session_fsm]
    },

    {ok, {{one_for_one, MaxRestart, MaxTime}, [SessionSupervisor]}}.

