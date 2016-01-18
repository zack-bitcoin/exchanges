-module(encryption).
-export([test/0,bin_enc/2,bin_dec/2,send_msg/3,get_msg/3,msg/1,sym_enc/2,sym_dec/2,new_key/0]).
-record(msg, {sig = "", msg = ""}).
-record(emsg, {key = "", msg = ""}).
msg(Msg) -> Msg#msg.msg.

si(Key) -> crypto:stream_init(rc4, crypto:hmac(sha256, "", Key)).
bin_enc(Key, Bin) ->
    {_, X} = crypto:stream_encrypt(si(Key), Bin),
    X.
bin_dec(Key, Msg) ->
    {_, Y} = crypto:stream_decrypt(si(Key), Msg),
    Y.
sym_enc(Key, Msg) -> bin_enc(Key, term_to_binary(Msg)).
sym_dec(Key, Emsg) -> binary_to_term(bin_dec(Key, Emsg)).
raw_sign(S, _) when not is_binary(S) -> "not binary";
raw_sign(S, Priv) -> en(crypto:sign(ecdsa, sha256, S, [de(Priv), params()])).
en(X) -> base64:encode(X).
de(X) -> base64:decode(X).
params() -> crypto:ec_curve(secp256k1).
send_msg(M, Pub, Priv) -> 
    {EphPub, EphPriv} = new_key(),
    Msg = #msg{sig=raw_sign(EphPub, Priv), msg=M},
    SS = shared_secret(Pub, EphPriv),
    Emsg = sym_enc(SS, Msg),
    #emsg{key=EphPub, msg=en(Emsg)}.
get_msg(Msg, Pubkey, Privkey) ->
    SS = shared_secret(Msg#emsg.key, Privkey),
    Sig = sym_dec(SS, de(Msg#emsg.msg)),
    true = verify_sig(Msg#emsg.key, Sig#msg.sig, Pubkey),
    Sig#msg.msg.
shared_secret(Pub, Priv) -> 
    P1 = de(Pub),
    P2 = de(Priv),
    en(crypto:compute_key(ecdh, P1, P2, params())).
verify_sig(S, Sig, Pub) -> crypto:verify(ecdsa, sha256, S, de(Sig), [de(Pub), params()]).
new_key() -> 
    {Pub, Priv} = crypto:generate_key(ecdh, params()),
    {en(Pub), en(Priv)}.

test() ->
    Val = <<"1234">>,
    Binary = <<2,3,4>>,
    true = bin_dec("abc", bin_enc("abc", Val)) == Val,
    true = bin_dec("abc", bin_enc("abc", Binary)) == Binary,
    Record = {f, Binary},
    {Pub, Priv} = new_key(),
    A = raw_sign(Binary, Priv),
    true = verify_sig(Binary, A, Pub),
    S = send_msg(Record, Pub, Priv),
    %io:fwrite(S),
    Sig = get_msg(S, Pub, Priv),
    true = Sig#msg.msg == Record,
    success.
