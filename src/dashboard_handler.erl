-module(dashboard_handler).

-export([init/2, get_months_of_year/1]).

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

    QueryParams = cowboy_req:match_qs([
        {year, [], undefined}
    ], Req),

    Year = maps:get(year, QueryParams),

    Filter0 = [
        {user_key, UserKey}
    ],

    case Year of
        undefined ->
            Filter1 = Filter0;
        Year ->
            Filter1 = [{to_date, <<Year/binary, "-12-31">>}|Filter0]
    end,

    AllDividends = dividend:db_list(Filter1),
    Total = sum(AllDividends),

    Ejson = [
        {total, calc:ejson_format(Total)}
    ],

    case maps:get(year, QueryParams) of
        undefined ->
            RollingDividends = dividend:db_list([{from_date, date_util:shift(date_util:today(), -1, years)}|Filter0]),
            Rolling = sum(RollingDividends),
            RollingMonthly = calc:divide(Rolling, calc:to_decimal(<<"12">>)),
            Ejson1 = [
                {rolling, calc:ejson_format(Rolling)},
                {rolling_monthly, calc:ejson_format(RollingMonthly)}
            ] ++ Ejson;
        Year ->
            YearlyDividends = dividend:db_list([
                {from_date, <<Year/binary, "-01-01">>},
                {to_date, <<Year/binary, "-12-31">>}|Filter0]),
            Yearly = sum(YearlyDividends),
            YearlyMonthly = calc:divide(Yearly, get_months_of_year(Year)),
            Ejson1 = [
                {year, calc:ejson_format(Yearly)},
                {monthly, calc:ejson_format(YearlyMonthly)}
            ] ++ Ejson
    end,

    Json = jiffy:encode({Ejson1}),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.

get_months_of_year(Year) ->
    case Year =:= binary:part(date_util:today(), 0, 4) of
        false ->
            calc:to_decimal(<<"12">>);
        true ->
            calc:to_decimal(binary:part(date_util:today(), 5, 2))
    end.



sum(Dividends) ->
    lists:foldl(fun(Dividend, Acc) ->
        decimal:add(Acc, dividend:amount(Dividend))
    end, calc:zero(), Dividends).
