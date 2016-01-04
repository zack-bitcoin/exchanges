-module(main).
-export([talk/1, test/0, start/0]).

start() ->
    ssl:start(),
    application:start(inets).

talk(Msg) ->
    case httpc:request(get, {Msg, []}, [{timeout, 1000}], []) of
	{ok, {_, _, R}} -> R;
	{error, timeout} ->
	    {error, timeout}
    end.

talk_decode([]) -> [];
talk_decode([X|T]) -> [jiffy:decode(talk(X))|talk_decode(T)].

    
test() ->
    start(),
    Urls = ["https://www.okcoin.com/api/v1/ticker.do?symbol=btc_usd",
	    "https://api.bitfinex.com/v1/pubticker/btcusd",
	    "https://www.bitstamp.net/api/ticker/",
	    "https://api.itbit.com/v1/markets/XBTUSD/ticker"],
    talk_decode(Urls).


