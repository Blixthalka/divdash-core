-module(divdash_sup).

-behaviour(supervisor).

-export([
    init/1,
    start_link/0,
    add_enduser_supervisor/1
]).

start_link() ->
    io:format("Starting divdash supervisor\n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },

    Users = enduser:db_list(),
    ChildSpecs = lists:map(fun(User) ->
        user_supervisor(User)
    end, Users),

    {ok, {SupFlags, ChildSpecs}}.


add_enduser_supervisor(User) ->
    Spec = user_supervisor(User),
    {ok, Pid} = supervisor:start_child(?MODULE, Spec),
    {ok, Pid}.


user_supervisor(User) ->
    {
        {user_supervisor, enduser:key(User)},
        {user_supervisor, start_link, [User]},
        permanent,
        10000,
        supervisor,
        [user_supervisor]
    }.

%% internal functions
