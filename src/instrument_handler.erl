-module(instrument_handler).

-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            case cowboy_req:binding(isin, Req) of
                undefined ->
                    get(Req, State);
                ISIN ->
                    get(Req, State, ISIN)
            end;
        _ ->
            Req1 = cowboy_req:reply(404, #{}, Req),
            {ok, Req1, State}
    end.

get(Req, State, ISIN) ->
    UserKey = web:user_key_cookie(Req),

    Filter = [
        {user_key, UserKey},
        {search, ISIN}
    ],

    Dividends = dividend:db_list(Filter),

    case length(Dividends) of
        0 ->
            Req1 = cowboy_req:reply(404, Req),
            {ok, Req1, State};
        _ ->
            FirstDividend = hd(Dividends),
            InstrumentName = dividend:instrument_name(FirstDividend),
            Amount = lists:foldl(fun(Dividend, Acc) ->
                decimal:add(dividend:amount(Dividend), Acc)
            end, calc:to_decimal(<<"0.0">>), Dividends),

        Ejson = {[
            {instrument_name, InstrumentName},
            {isin, ISIN},
            {amount, calc:ejson_format(Amount)}
        ]},
        Json = jiffy:encode(Ejson),
        Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
        {ok, Req1, State}
    end.

get(Req, State) ->
    UserKey = web:user_key_cookie(Req),

    QueryParams = cowboy_req:match_qs([
        {sort_column, [], <<"amount">>},
        {sort_direction, [], <<"desc">>},
        {from_date, [], undefined},
        {to_date, [], undefined}
    ], Req),

    SortColumn = maps:get(sort_column, QueryParams),
    SortDirection = maps:get(sort_direction, QueryParams),

    Filter0 = [
        {user_key, UserKey}
    ],
    Filter1 = web:add_filter_param(from_date, QueryParams, Filter0),
    Filter2 = web:add_filter_param(to_date,   QueryParams, Filter1),

    Dividends = dividend:db_list(Filter2),

    Instruments = lists:foldl(fun(Dividend, Acc) ->
        case dividend:isin(Dividend) of
            undefined ->
                Acc;
            ISIN ->
                case maps:get(ISIN, Acc, undefined) of
                    undefined ->
                        maps:put(ISIN, create_instrument(Dividend), Acc);
                    {Name, ISIN, Amount} ->
                        Sum = decimal:add(Amount, dividend:amount(Dividend)),
                        maps:put(ISIN, {Name, ISIN, Sum}, Acc)
                end
        end
    end, #{}, Dividends),

    SortedInstruments = lists:sort(fun({_,A}, {_, B}) ->
        sort(SortColumn, SortDirection, A, B)
    end, maps:to_list(Instruments)),

    Ejson = lists:map(fun({_Key, {Name, ISIN, Amount}}) ->
        {[
            {instrument_name, Name},
            {isin, ISIN},
            {amount, calc:ejson_format(Amount)}
        ]}
    end, SortedInstruments),
    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.


create_instrument(Dividend) ->
    {
        dividend:instrument_name(Dividend),
        dividend:isin(Dividend),
        dividend:amount(Dividend)
    }.



sort(SortColumn, <<"desc">>, A, B) ->
    not sort(SortColumn, <<"asc">>, A, B);
sort(<<"instrument_name">>, <<"asc">>, {NameA, _, _}, {NameB, _, _}) ->
    NameA =< NameB;
sort(<<"amount">>, <<"asc">>, {_, _, AmountA}, {_, _, AmountB}) ->
    decimal:fast_cmp(AmountA, AmountB) =:= -1;
sort(<<"isin">>, <<"asc">>, {_, ISINA, _}, {_, ISINB, _}) ->
    ISINA =< ISINB.

