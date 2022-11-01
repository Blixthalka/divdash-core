-module(date_handler).

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

    QueryParams = cowboy_req:match_qs([
        {from_date, [], undefined},
        {to_date, [], undefined},
        {isin, [], undefined},
        {type, [], <<"year">>}
    ], Req),

    Type = maps:get(type, QueryParams),

    Filter0 = [
        {user_key, UserKey}
    ],
    Filter1 = web:add_filter_param(isin, QueryParams, Filter0),
    Filter2 = web:add_filter_param(from_date, QueryParams, Filter1),
    Filter3 = web:add_filter_param(to_date,   QueryParams, Filter2),

    Dividends = dividend:db_list(Filter3),

    ByYear = lists:foldl(fun(Dividend, Acc) ->
        Amount =  maps:get(get_key(Type, Dividend), Acc, calc:zero()),
        Sum = decimal:add(Amount, dividend:amount(Dividend)),
        maps:put(get_key(Type, Dividend), Sum, Acc)
    end, #{}, Dividends),

    SortedByYear = lists:sort(fun({A,_}, {B, _}) ->
        sort(<<"asc">>, A, B)
    end, maps:to_list(ByYear)),

    Ejson = lists:map(fun({Key, Amount}) ->
        {[
            {Type, Key},
            {amount, calc:ejson_format(Amount)}
        ]}
    end, SortedByYear),
    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.

get_key(Type, Dividend) ->
    case Type of
        <<"year">> ->
            binary:part(dividend:date(Dividend), 0, 4);
        <<"month">> ->
            binary:part(dividend:date(Dividend), 0, 7);
        _ ->
            throw(bad_request)
    end.


sort(<<"desc">>, A, B) ->
    not sort(<<"asc">>, A, B);
sort( <<"asc">>, LabelA, LabelB) ->
    LabelA =< LabelB.

