-module(password).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, set/2,get/0,unlock/1]).
-define(file, "master_key.db").
init(ok) -> {ok, {"pub", "priv"}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({unlock, Password}, _) -> 
    {Pub, Priv} = 
	case filelib:is_file(?file) of
	    false -> 1=2;
	    true ->
		{ok, Enc} = file:read_file(?file),
		encryption:sym_dec(Password, Enc)
	end,
    keys:unlock(Pub, Priv),
    {noreply, {Pub, Priv}};
handle_cast({set, _, Password}, {"pub", "priv"}) ->
    {Pub, Priv} = encryption:new_key(),
    Enc = encryption:sym_enc(Password, {Pub, Priv}),
    file:write_file(?file, Enc, [binary]),
    {noreply, {Pub, Priv}};
handle_cast({set, OldPassword, Password}, _) ->
    {ok, F} = file:read_file(?file),
    {OldPub, OldPriv} = encryption:sym_dec(OldPassword, F),
    {Pub, Priv} = set_password(Password, OldPub, OldPriv),
    {noreply, {Pub, Priv}}.
handle_call(get, _From, X) -> {reply, X, X}.
get() -> gen_server:call(?MODULE, get).
set(OldPassword, Password) -> gen_server:cast(?MODULE, {set, OldPassword, Password}).
set_password(Password, OldPub, OldPriv) ->
    {Pub, Priv} = encryption:new_key(),
    Enc = encryption:sym_enc(Password, {Pub, Priv}),
    keys:change_password(Pub, Priv, OldPub, OldPriv),
    file:write_file(?file, Enc, [binary]),
    {Pub, Priv}.
unlock(Password) -> gen_server:cast(?MODULE, {unlock, Password}).
    
