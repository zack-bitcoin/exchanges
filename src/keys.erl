-module(keys).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, add/3,read/0,remove/1,unlock/2,change_password/4,read_keys/2,test/0]).
-define(file, "keys.db").
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({change_password, OldPub, OldPriv, NewPub, NewPriv}, _) ->
    Y = read_keys(OldPub, OldPriv),
    write_keys(NewPub, NewPriv, Y),
    {noreply, Y};
handle_cast({unlock, Pub, Priv}, _) -> 
    Y = read_keys(Pub, Priv),
    {noreply, Y};
handle_cast({remove, Title}, X) -> 
    Y = dict:erase(Title, X),
    {MPub, MPriv} = password:get(),
    write_keys(MPub, MPriv, Y),
    {noreply, Y};
handle_cast({add, Title, Pubkey, Privkey}, X) -> 
    Y = dict:store(Title, [Pubkey, Privkey], X),
    {MPub, MPriv} = password:get(),
    write_keys(MPub, MPriv, Y),
    {noreply, Y}.
handle_call(get, _From, X) -> {reply, X, X}.
read() -> gen_server:call(?MODULE, get).
add(Title, Pubkey, Privkey) ->
    gen_server:cast(?MODULE, {add, Title, Pubkey, Privkey}).
remove(Title) ->
    gen_server:cast(?MODULE, {remove, Title}).
read_keys(Pub, Priv) ->
    {ok, FB} = file:read_file(?file),
    case FB of
	<<"">> -> dict:new();
	F -> 
	    G = binary_to_term(F),
	    encryption:get_msg(G, Pub, Priv)
    end.
write_keys(Pub, Priv, D) ->
    M = encryption:send_msg(D, Pub, Priv),
    N = term_to_binary(M),
    file:write_file(?file, N, [binary]).
unlock(Pub, Priv) ->
    gen_server:cast(?MODULE, {unlock, Pub, Priv}).
change_password(Pub, Priv, OldPub, OldPriv) ->
    gen_server:cast(?MODULE, {change_password, OldPub, OldPriv, Pub, Priv}).

test() ->
    D = dict:new(),
    D = read(),
    {Pub, Priv} = password:new_key(),
    unlock(Pub, Priv),
    add(1, 2, 3),
    read_keys(Pub, Priv).
