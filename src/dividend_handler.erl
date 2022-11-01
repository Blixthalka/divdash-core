-module(dividend_handler).

-export([init/2]).

init(Req, State) ->
    io:format("Request DIVIDEND"),
    case cowboy_req:method(Req) of
        <<"GET">> ->
            get(Req, State);
        <<"POST">> ->
            post(Req, State);
        _ ->
            Req1 = cowboy_req:reply(404, #{}, Req),
            {ok, Req1, State}
    end.

get(Req, State) ->
    UserKey = web:user_key_cookie(Req),

    QueryParams = cowboy_req:match_qs([
        {sort_column, [], <<"date">>},
        {sort_direction, [], <<"desc">>},
        {search, [], undefined},
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
    Filter3 = web:add_filter_param(search,   QueryParams, Filter2),

    Dividends = dividend:db_list(Filter3),
    Sorted = lists:sort(fun(A, B) ->
        sort(SortColumn, SortDirection, A, B)
    end, Dividends),

    Ejson = lists:map(fun(D) -> dividend:ejson(D) end, Sorted),
    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.

sort(SortColumn, <<"desc">>, A, B) ->
    not sort(SortColumn, <<"asc">>, A, B);
sort(<<"date">>, <<"asc">>, A, B) ->
    dividend:date(A) =< dividend:date(B);
sort(<<"isin">>, <<"asc">>, A, B) ->
    dividend:isin(A) =< dividend:isin(B);
sort(<<"amount">>, <<"asc">>, A, B) ->
    decimal:fast_cmp(dividend:amount(A), dividend:amount(B)) =:= -1;
sort(<<"instrument_name">>, <<"asc">>, A, B) ->
    dividend:instrument_name(A) =< dividend:instrument_name(B).

post(Req, State) ->
    QueryParams = cowboy_req:match_qs([
        tag
    ], Req),

    Tag = maps:get(tag, QueryParams),

    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {<<"multipart">>, <<"form-data">>, _} ->
            Body = web:read_multipart_body(Req),
            case Tag of
                <<"avanza">> ->
                    dividend_file_parser:avanza(web:user_key_cookie(Req), Body);
                _ ->
                    throw(bad_request)
            end,
            {ok, cowboy_req:reply(200, Req), State};
        _ ->
            {ok, cowboy_req:reply(400, Req), State}
    end.

