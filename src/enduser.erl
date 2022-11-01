-module(enduser).

-export([
    create/2,
    key/1,
    key/2,
    email/1,
    ejson/1,

    pw_is_correct/2,
    db_list/0,
    db_create_table/0,
    db_create/1,
    db_find_by_email/1,
    db_delete/1
]).


-record(enduser, {
    key :: binary() | undefined,
    email :: binary(),
    pw_hash :: binary()
}).

-type enduser() :: #enduser{}.

-export_type([
    enduser/0
]).

create(Email, Password) ->
    #enduser{
        key = undefined,
        email = Email,
        pw_hash = hash(Password)
    }.

key(Key, Enduser) ->
    Enduser#enduser{key = Key}.

key(#enduser{key = Key}) ->
    Key.


email(#enduser{email = Email}) ->
    Email.

pw_is_correct(Password, #enduser{pw_hash = Hash}) ->
    hash(Password) =:= Hash.

hash(Password) ->
    crypto:hash(sha256, Password).

ejson(#enduser{key = Key, email = Email}) ->
     {[
        {key, Key},
        {email, Email}
    ]}.

db_find_by_email(Email) ->
    Endusers = lists:filter(fun(Enduser) ->
        enduser:email(Enduser) =:= Email
    end, db_list()),
    case Endusers of
        [] ->
            undefined;
        [Enduser] ->
            Enduser
    end.

db_delete(Enduser) ->
    db:delete(?MODULE, Enduser).

db_list() ->
    db:list(?MODULE).

db_create_table() ->
    db:create_table(?MODULE, record_info(fields, ?MODULE)).

db_create(Enduser) ->
    Res = db:create(?MODULE, Enduser),
    case Res of
        {ok, EnduserWithKey} ->
            io:format("KEY ~p\n", [enduser:key(EnduserWithKey)]),
            divdash_sup:add_enduser_supervisor(EnduserWithKey);
        _ ->
            ok
    end,
    Res.
