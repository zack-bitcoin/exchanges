-module(keys).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, add/3,get/0,test/0]).
init(ok) -> {ok, read_keys:doit()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Title, Pubkey, Privkey}, X) -> 
    Y = dict:store(Title, [Pubkey, Privkey], X),
    write_keys:doit(Y),
    {noreply, Y}.
handle_call(get, _From, X) -> {reply, X, X}.

get() -> gen_server:call(?MODULE, get).
add(Title, Pubkey, Privkey) ->
    gen_server:cast(?MODULE, {add, Title, Pubkey, Privkey}).

test() ->
    ok.
