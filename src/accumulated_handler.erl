-module(accumulated_handler).

-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            get(Req, State);
        _ ->
            Req1 = cowboy_req:reply(404, #{}, Req),
            {ok, Req1, State}
    end.


get(Req, State) ->
    UserKey = web:user_key_cookie(Req),

    Filter0 = [
        {user_key, UserKey}
    ],

    Dividends = dividend:db_list(Filter0),

    SortedDividends = lists:sort(fun(A, B) ->
        sort(<<"asc">>, A, B)
    end, Dividends),

    {Accumulated, _} = lists:foldl(fun(Dividend, {List, Sum}) ->
        NewSum = decimal:add(dividend:amount(Dividend), Sum),
        Date = dividend:date(Dividend),
        case List of
            [] ->
                {[{Date, NewSum}], NewSum};
            _ ->
                case hd(List) of
                    {Date, _} ->
                        {[{Date, NewSum}] ++ tl(List), NewSum};
                    _ ->
                        {[{Date, NewSum}|List], NewSum}
                end
        end
    end, {[], calc:zero()}, SortedDividends),

    Ejson = lists:map(fun({Date, Amount}) ->
        {[
            {date, Date},
            {amount, calc:ejson_format(Amount)}
        ]}
    end, Accumulated),
    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.




sort(<<"desc">>, A, B) ->
    not sort(<<"asc">>, A, B);
sort(<<"asc">>, A, B) ->
    dividend:date(A) =< dividend:date(B).
