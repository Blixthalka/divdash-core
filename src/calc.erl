-module(calc).

-export([
    to_decimal/1,
    to_binary/1,
    round/1,
    ejson_format/1,
    zero/0,
    divide/2
]).


to_decimal(Binary) ->
    Replaced = binary:replace(Binary, <<",">>, <<".">>),
    decimal_conv:from_binary(Replaced).

to_binary(Decimal) ->
    decimal_conv:to_binary(Decimal, #{ pretty => true }).

ejson_format(Decimal) ->
    binary:replace(to_binary(calc:round(Decimal)), <<".0">>, <<>>).

round(Decimal) ->
    decimal:round(round_half_up, Decimal, 0).

zero() ->
    calc:to_decimal(<<"0.0">>).

divide(A, B) ->
    decimal:divide(A, B,  #{ precision => 0, rounding => round_half_up }).