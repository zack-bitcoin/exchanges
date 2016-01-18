-module(info).
-export([test/0]).

noww() ->
    {A, B, C} = erlang:timestamp(),
    D = (A*1000000) + B,
    E = (D*1000000) + C,
    E div 1000.

test() ->
    URL = "https://api.bitfinex.com/v1/account_infos",
    N = list_to_binary(integer_to_list(noww())),
    Pay = {[{<<"request">>, <<"/v1/account_infos">>},
		{<<"nonce">>, N}]},
    Payload = jiffy:encode(Pay),
    P = base64:encode(Payload),
    D = keys:get(),
    Secret = hd(tl(dict:fetch("bitfinex", D))),
    Hmac1 = crypto:hmac_init(sha384, Secret),
    Hmac2 = crypto:hmac_update(Hmac1, P),
    Bin = crypto:hmac_final(Hmac2),
    Signature = hex:bin_to_hexstr(Bin),
    Format = "application/json",
    Header = [{"X-BFX-PAYLOAD", binary_to_list(Payload)},
	      {"X-BFX-SIGNATURE", Signature},
	      {"X-BFX-APIKEY", Secret}],
    io:fwrite("here"),
    case httpc:request(post, {URL, Header, Format, P}, [{timeout, 1000},{connect_timeout, 1000}], []) of
	{ok, {_,_, R}}  -> R;
	R -> io:fwrite("2\n\n\n"), R
    end.




