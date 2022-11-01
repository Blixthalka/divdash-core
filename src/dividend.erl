-module(dividend).

-export([
    create/7,
    key/2,
    ejson/1,
    date/1,
    tag/1,
    amount/1,
    user_key/1,
    instrument_name/1,
    isin/1,
    db_create_table/0,
    db_create/1,
    db_list/0,
    db_list/1,
    db_delete/1,
    db_delete/2
]).

-record(dividend, {
    key :: binary() | undefined,
    user_key :: binary(),
    instrument_name :: binary(),
    isin :: binary(),
    currency :: binary(),
    amount :: decimal:decimal(),
    date :: binary(),
    tag :: avanza | nordnet
}).

create(UserKey, InstrumentName, ISIN, Currency, Amount, Date, Tag) ->
    #dividend {
        key = undefined,
        user_key = UserKey,
        instrument_name = InstrumentName,
        isin = ISIN,
        currency = Currency,
        amount = Amount,
        date = Date,
        tag = Tag
    }.

key(Key, Dividend) ->
    Dividend#dividend{key = Key}.

user_key(#dividend{user_key = UserKey}) ->
    UserKey.

date(#dividend{date = Date}) ->
    Date.

tag(#dividend{tag = Tag}) ->
    Tag.

instrument_name(#dividend{instrument_name = InstrumentName}) ->
    InstrumentName.

isin(#dividend{isin = ISIN}) ->
    ISIN.

amount(#dividend{amount = Amount}) ->
    Amount.

ejson(#dividend{
    instrument_name = InstrumentName,
    isin = ISIN,
    currency = Currency,
    amount = Amount,
    date = Date
}) ->
    Ejson = [
        {instrument_name, InstrumentName},
        {currency, Currency},
        {amount, calc:ejson_format(Amount)},
        {date, Date}
    ],
    case ISIN of
        undefined ->
            {Ejson};
        _ ->
            {[{isin, ISIN}|Ejson]}
    end.



db_list(SearchFilters) ->
    lists:foldl(fun({FilterName, FilterValue}, Dividends) ->
        lists:filter(fun(Dividend) ->
            filter(FilterName, Dividend, FilterValue)
        end, Dividends)
    end, db_list(), SearchFilters).

filter(_, _Dividend, undefined) ->
    true;
filter(_, _Dividend, <<>>) ->
    true;
filter(user_key, Dividend, UserKey) ->
    dividend:user_key(Dividend) =:= UserKey;
filter(from_date, Dividend, FromDate) ->
    dividend:date(Dividend) >= FromDate;
filter(to_date, Dividend, ToDate) ->
    dividend:date(Dividend) =< ToDate;
filter(tag, Dividend, Tag) ->
    dividend:tag(Dividend) =:= Tag;
filter(isin, Dividend, Search) ->
    LowerSearch = string:lowercase(Search),
    case dividend:isin(Dividend) of
        undefined ->
            false;
        ISIN ->
            string:lowercase(ISIN) =:= LowerSearch
    end;
filter(search, Dividend, Search) ->
    LowerSearch = string:lowercase(Search),
    NameMatch = binary:match(string:lowercase(dividend:instrument_name(Dividend)), LowerSearch) =/= nomatch,
    case dividend:isin(Dividend) of
        undefined ->
            NameMatch;
        ISIN ->
            NameMatch orelse string:lowercase(ISIN) =:= LowerSearch
    end.

db_delete(UserKey, Tag) ->
    lists:foreach(fun(Dividend) ->
        db_delete(Dividend)
    end, db_list([
        {user_key, UserKey},
        {tag, Tag}
    ])).

db_delete(Dividend) ->
    db:delete(?MODULE, Dividend).

db_list() ->
    db:list(?MODULE).

db_create_table() ->
    db:create_table(?MODULE, record_info(fields, ?MODULE)).

db_create(Dividend) ->
    db:create(?MODULE, Dividend).
