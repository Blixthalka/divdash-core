-module(dividend_file_parser).


-export([
    avanza/2
]).



avanza(UserKey, FileContent) ->
    CleanedFileContent = binary:replace(FileContent, <<239,187,191>>, <<"">>),
    Lines = binary:split(CleanedFileContent, <<"\n">>, [global]),

    case length(Lines) of
        0 ->
            {error, no_file_content};
        _ ->
            Headers = split(hd(Lines)),
            Rows = tl(Lines),

            DividendRows = lists:filter(
                fun(<<>>) ->
                        false;
                    (Row) ->
                Zipped = lists:zip(Headers, split(Row)),
                row_value(<<"Typ av transaktion"/utf8>>, Zipped) =:= <<"Utdelning">>
            end, Rows),

            NewDividends = lists:map(fun(Row) ->
                Zipped = lists:zip(Headers, split(Row)),

                InstrumentName = row_value(<<"Värdepapper/beskrivning"/utf8>>, Zipped),
                Currency = <<"SEK">>,
                Amount = calc:to_decimal(row_value(<<"Belopp"/utf8>>, Zipped)),
                Date = row_value(<<"Datum"/utf8>>, Zipped),

                case row_value(<<"ISIN"/utf8>>, Zipped) of
                    <<"-">> ->
                        ISIN = undefined;
                    Value ->
                        ISIN = Value
                end,

                dividend:create(UserKey, InstrumentName, ISIN, Currency, Amount, Date, avanza)
            end, DividendRows),

            dividend:db_delete(UserKey, avanza),

            lists:foreach(fun(Dividend) ->
                {ok, _} = dividend:db_create(Dividend)
            end, NewDividends),
            ok
    end.


row_value(_Header, []) ->
    throw({error, header_not_found});
row_value(Header, [{Header, Value}|_]) ->
    Value;
row_value(Header, [_|ZippedTail]) ->
    row_value(Header, ZippedTail).


split(Binary) ->
    binary:split(Binary, <<";">>, [global]).

