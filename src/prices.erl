-module(prices).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,add_row/2,doit/0,doit3/0,ticker_check/1]).
-define(exchanges, exchanges()).    
exchanges() -> 
    Urls = [
	    "https://data.btcchina.com/data/ticker?market=btccny",
	    "https://www.okcoin.com/api/v1/ticker.do?symbol=btc_usd",
	    "https://api.bitfinex.com/v1/pubticker/btcusd",
	    "https://www.bitstamp.net/api/ticker/",
	    "https://api.itbit.com/v1/markets/XBTUSD/ticker",
	    "https://btc-e.com/api/3/ticker/btc_usd"
	   ],
    K = ["btcchina", "okcoin", "bitfinex", "bitstamp", "itbit", "btce"],
    store_many(K, Urls, dict:new()).
store_many([], [], D) -> D;
store_many([Key|K], [Url|U], D) ->
    store_many(K, U, dict:store(Key, Url, D)).
-define(file, "prices.csv").
init(ok) -> {ok, [keys(), dict:new()]}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.
handle_cast({add, D, Title}, [Keys, Dict]) -> 
    NewDict = dict:store(Title, D, Dict),
    
    CSV = write_keys(keys()) ++ write_csv(NewDict, dict:fetch_keys(NewDict)),
    ok = file:delete(?file), %returns either ok, or {error, Reason}.
    {ok, File} = file:open(?file, [write, append, raw]),
    file:write(File, CSV),
    file:close(File),
    {noreply, [Keys, NewDict]}.
write_keys([]) -> "\n";
write_keys([Key|K]) ->
    Key++", "++write_keys(K).
write_csv(_, []) -> "\n";
write_csv(D, [Key|K]) ->
    V = dict:fetch(Key, D),
    Key++", "++write_row(tl(keys()), V)++write_csv(D, K).
write_row([], _) -> "\n";
write_row([Key|K], D) ->
    %number_to_list(proplists:lookup(Key, D))++", "++write_row(K, D).
    number_to_list(dict:fetch(Key, D))++", "++write_row(K, D).
number_to_list(K) when is_float(K) ->
    hd(io_lib:format("~.2f", [K]));
number_to_list(K) when is_integer(K) ->
    integer_to_list(K);
number_to_list(K) -> K.
add_row(C, Title) ->
    J = jiffy:decode(C),
    K = element(1, J),
    D = decode_json(K, Title),
    E = unpack(D),
    gen_server:cast(?MODULE, {add, E, Title}).
keys() -> ["exchange", "last", "sell", "buy", "high", "low", "vol"].
talk(Msg) ->
    case httpc:request(get, {Msg, []}, [{timeout, 1000}], []) of
	{ok, {_, _, R}} -> R;
	{error, timeout} ->
	    {error, timeout};
	{error, {failed_connect, _}} ->
	    {error, failed_connect}
    end.
check_ticker(E) ->
    URL = dict:fetch(E, exchanges()),
    talk(URL).
decode_json({error, _}, Title) ->
    io:fwrite(Title++"\nError\n\n");
decode_json(B, "btce") ->
    K = hd(B),
    L = element(1, element(2, K)),
    proplists:delete(<<"vol">>, L);
decode_json(B, "btcchina") ->
    K = hd(B),
    L = element(1, element(2, K)),
    unbinary(L);
decode_json(B, "okcoin") ->
    A = hd(tl(B)),
    C = element(1, element(2, A)),
    unbinary(C);
decode_json(B, "bitfinex") ->
    unbinary(B);
decode_json(B, "bitstamp") ->
    unbinary(B);
decode_json(B, "itbit") ->
    unbinary(B);
decode_json(B, Name) ->
    io:fwrite("decode json unprogrammed\n"++Name++"\n"++B++"\n\n").
unbinary([]) -> [];
unbinary([{<<"pair">>, _}|T]) -> unbinary(T);
unbinary([{<<"serverTimeUTC">>, _}|T]) -> unbinary(T);
unbinary([{Key, Value}|T]) when is_float(Value) or is_integer(Value)-> 
    [{Key, Value}|unbinary(T)];
unbinary([{Key, Value}|T]) when is_list(Value)-> 
    [{Key, list_to_number(Value)}|unbinary(T)];
unbinary([{Key, Value}|T]) when is_binary(Value)-> 
    [{Key, list_to_number(binary_to_list(Value))}|unbinary(T)].
list_to_number(X) ->
    case string:to_float(X++".0") of
	{_, []} ->
	    list_to_integer(X);
	{_, ".0"} ->
	    list_to_float(X)
    end.
unpack(D) -> unpack(D, dict:new()).
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
    unpack(T, D).
ticker_check(Title) ->
    case check_ticker(Title) of
	{error, timeout} ->
	    {error, timeout};
	C -> add_row(C, Title)
    end.
doit() -> spawn(prices, doit3, []).
doit3() ->
    E = exchanges(),
    Es = dict:fetch_keys(E),
    doit2(Es),
    timer:sleep(2500),
    doit2(Es),
    timer:sleep(2500),
    doit2(Es),
    timer:sleep(2500).
    
doit2([]) -> ok;
doit2([Title|T]) -> 
    spawn(prices, ticker_check, [Title]),
    %ticker_check(Title),
    doit2(T).
test() ->
    Title = "okcoin",
    C = check_ticker(Title),
    io:fwrite(C),
    add_row(C, Title),
    doit3(),
    success.
