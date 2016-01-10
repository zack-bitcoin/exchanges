-module(write_keys).
-export([doit/1, test/0]).
-define(file, "keys.csv").
% D is dict. {"bitfinex":[Pub, Priv], ...}
% CSV line is like: 
% bitfinex, Pub, Priv,
doit(D) ->
    CSV = make_csv(D),
    ok = file:delete(?file),
    {ok, F} = file:open(?file, [write, append, raw]),
    file:write(F, CSV),
    file:close(F).

make_csv(D) ->
    Keys = dict:fetch_keys(D),
    make_rows(Keys, D).
make_rows([], _) -> [];
make_rows([Key|K], D) -> 
    [Pub, Priv] = dict:fetch(Key, D),
    Key++", "++Pub++", "++Priv++",\n"++make_rows(K, D).

test() ->
    A = dict:new(),
    B = dict:store("abc", ["pub", "priv"], A),
    make_csv(B).
