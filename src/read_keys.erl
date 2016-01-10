-module(read_keys).
-export([doit/0]).
-define(file, "keys.csv").
doit() ->
    %returns a dictionary with keys like "bitfinex", and values like [pubkey, privkey].
    {ok, F} = file:open(?file, [read]),
    read_rows(F, dict:new()).
read_rows(File, Lines) ->
    case read_row(File, [], []) of
	[[]] -> Lines;
	L -> read_rows(File, dict:store(hd(L), tl(L), Lines))
    end.
read_row(File, Letters, Words) ->
    case file:read(File, 1) of
	{ok, Letter} ->
	    read_row2(File, Letter, Letters, Words);
	eof -> Words ++ [reverse(Letters)]
    end.
read_row2(_, "\n", [], Words) -> Words;
read_row2(_, "\n", Letters, Words) -> Words++[reverse(Letters)];
read_row2(File, " ", Letters, Words) ->
    read_row(File, Letters, Words);
read_row2(File, ",", Letters, Words) ->
    read_row(File, [], Words ++ [reverse(Letters)]);
read_row2(File, Letter, Letters, Words) ->
    read_row(File, Letter++Letters, Words).
reverse(L) -> reverse(L, []).
reverse([], X) -> X;
reverse([H|T], X) -> reverse(T, [H|X]).

    
