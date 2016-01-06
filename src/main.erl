-module(main).
-export([talk/1, price/0]).
talk(Msg) ->
    case httpc:request(get, {Msg, []}, [{timeout, 1000}], []) of
	{ok, {_, _, R}} -> R;
	{error, timeout} ->
	    {error, timeout};
	{error, {failed_connect, _}} ->
	    {error, failed_connect}
    end.

talk_decode([]) -> [];
talk_decode([X|T]) -> [talk(X)|talk_decode(T)].
unpack([], D) -> D;
unpack([{<<"bid">>, Value}|T], D) -> 
    N = dict:store("buy", Value, D),
    unpack(T, N);
unpack([{<<"buy">>, Value}|T], D) -> 
    N = dict:store("buy", Value, D),
    unpack(T, N);
unpack([{<<"high24h">>, Value}|T], D) -> 
    N = dict:store("high", Value, D),
    unpack(T, N);
unpack([{<<"high">>, Value}|T], D) -> 
    N = dict:store("high", Value, D),
    unpack(T, N);
unpack([{<<"lastPrice">>, Value}|T], D) -> 
    N = dict:store("last", Value, D),
    unpack(T, N);
unpack([{<<"last">>, Value}|T], D) -> 
    N = dict:store("last", Value, D),
    unpack(T, N);
unpack([{<<"last_price">>, Value}|T], D) -> 
    N = dict:store("last", Value, D),
    unpack(T, N);
unpack([{<<"low24h">>, Value}|T], D) -> 
    N = dict:store("low", Value, D),
    unpack(T, N);
unpack([{<<"low">>, Value}|T], D) -> 
    N = dict:store("low", Value, D),
    unpack(T, N);
unpack([{<<"ask">>, Value}|T], D) -> 
    N = dict:store("sell", Value, D),
    unpack(T, N);
unpack([{<<"sell">>, Value}|T], D) -> 
    N = dict:store("sell", Value, D),
    unpack(T, N);
unpack([{<<"volume24h">>, Value}|T], D) -> 
    N = dict:store("vol", Value, D),
    unpack(T, N);
unpack([{<<"volume">>, Value}|T], D) -> 
    N = dict:store("vol", Value, D),
    unpack(T, N);
unpack([{<<"vol">>, Value}|T], D) -> 
    N = dict:store("vol", Value, D),
    unpack(T, N);
unpack([{<<"vol_cur">>, Value}|T], D) -> 
    N = dict:store("vol", Value, D),
    unpack(T, N);
unpack([{_K, _}|T], D) -> 
    %io:fwrite("badkey "),
    %io:fwrite(K),
    %io:fwrite("\n"),
    unpack(T, D).
un_binary([]) -> [];
un_binary([{<<"pair">>, _}|T]) -> un_binary(T);
un_binary([{<<"serverTimeUTC">>, _}|T]) -> un_binary(T);
un_binary([{Key, Value}|T]) when is_float(Value) or is_integer(Value)-> 
    [{Key, Value}|un_binary(T)];
un_binary([{Key, Value}|T]) when is_list(Value)-> 
    [{Key, list_to_number(Value)}|un_binary(T)];
un_binary([{Key, Value}|T]) when is_binary(Value)-> 
    [{Key, list_to_number(binary_to_list(Value))}|un_binary(T)].
    
list_to_number(X) ->
    case string:to_float(X++".0") of
	{_, []} ->
	    list_to_integer(X);
	{_, ".0"} ->
	    list_to_float(X)
    end.
json_step(X, Y) -> json_step(X, Y, []).
json_step([],[], Out) -> Out;
json_step([{error, _}|D], [Name|N], Out) ->
    io:fwrite(Name++"\n"),
    io:fwrite("Error"),
    io:fwrite("\n\n"),
    json_step(D, N, Out);
json_step([Data|D], ["btce"|N], Out) ->
    %io:fwrite("btce\n"),
    %io:fwrite(Data),
    J = jiffy:decode(Data),
    K = hd(element(1, J)),
    L = element(1, element(2, K)),
    %io:fwrite(L),
    M = proplists:delete(<<"vol">>, L),
    OK = unpack(M, dict:new()),
    json_step(D, N, [{"btce", OK}|Out]);
json_step([Data|D], ["btcchina"|N], Out) ->
    %io:fwrite("btcchina\n"),
    %io:fwrite(Data),
    J = jiffy:decode(Data),
    K = hd(element(1, J)),
    L = element(1, element(2, K)),
    %io:fwrite(L),
    OK = unpack(un_binary(L), dict:new()),
    json_step(D, N, [{"btcchina", OK}|Out]);
json_step([Data|D], ["okcoin"|N], Out) ->
    %io:fwrite("okcoin\n"),
    %io:fwrite(Data),
    J = jiffy:decode(Data),
    K = hd(tl(element(1, J))),
    L = element(1, element(2, K)),
    %io:fwrite(L),
    OK = unpack(un_binary(L), dict:new()),
    json_step(D, N, [{"okcoin", OK}|Out]);
json_step([Data|D], ["bitfinex"|N], Out) ->
    %io:fwrite("bitfinex\n"),
    %io:fwrite(Data),
    J = jiffy:decode(Data),
    OK = unpack(un_binary(element(1, J)), dict:new()),
    json_step(D, N, [{"bitfinex", OK}|Out]);
json_step([Data|D], ["bitstamp"|N], Out) ->
    %io:fwrite("bitstamp\n"),
    %io:fwrite(Data),
    J = jiffy:decode(Data),
    OK = unpack(un_binary(element(1, J)), dict:new()),
    json_step(D, N, [{"bitstamp", OK}|Out]);
json_step([Data|D], ["itbit"|N], Out) ->
    %io:fwrite("itbit\n"),
    %io:fwrite(Data),
    J = jiffy:decode(Data),
    OK = unpack(un_binary(element(1, J)), dict:new()),
    json_step(D, N, [{"itbit", OK}|Out]);
json_step([Data|D], [Name|N], Out) ->
    io:fwrite("json_step, unwritten market\n"),
    io:fwrite(Name++"\n"),
    io:fwrite(Data),
    io:fwrite("\n\n"),
    json_step(D, N, Out).

% buy, sell, high, low, vol, vwap, last, 
% sell == ask
% bid == buy <-- lower
-define(file, "prices.csv").
price() ->
    Urls = [
	    "https://data.btcchina.com/data/ticker?market=btccny",
	    "https://www.okcoin.com/api/v1/ticker.do?symbol=btc_usd",
	    "https://api.bitfinex.com/v1/pubticker/btcusd",
	    "https://www.bitstamp.net/api/ticker/",
	    "https://api.itbit.com/v1/markets/XBTUSD/ticker",
	    "https://btc-e.com/api/3/ticker/btc_usd"
						%"https://poloniex.com/public?command=returnTicker"
						%"https://www.gatecoin.com/api/Public/LiveTickers"
	   ],
    Names = [
	     "btcchina",
	     "okcoin",
	     "bitfinex",
	     "bitstamp",
	     "itbit",
	     "btce"
						%"poloniex"
						%"gatecoin"
	     ],
    A = length(Urls),
    A = length(Names),
    N = json_step(talk_decode(Urls), Names),
    file:delete(?file), %returns either ok, or {error, Reason}.
    {ok, File} = file:open(?file, [write, append, raw]),
    write_keys(keys(), File),
    write_csv(N, File),
    file:close(File).
keys() -> ["exchange", "buy", "high", "last", "low", "sell", "vol"].
write_keys([], File) -> 
    file:write(File, "\n");
write_keys([Key|K], File) ->
    file:write(File, Key++", "),
    write_keys(K, File).
write_csv([], File) -> file:write(File, "\n");
write_csv([{K, V}|T], File) ->
    file:write(File, K++", "),
    write_row(tl(keys()), V, File),
    write_csv(T, File).
write_row([], _, File) ->
    file:write(File, "\n");
write_row([Key|K], D, File) ->
    file:write(File, number_to_list(dict:fetch(Key, D))++", "),
    write_row(K, D, File).
number_to_list(K) when is_float(K) ->
    hd(io_lib:format("~.2f", [K]));
number_to_list(K) when is_integer(K) ->
    integer_to_list(K);
number_to_list(K) -> K.
