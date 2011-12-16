-module(cb_who_utils).
-export([
    random_salt/0,
    hashed_value/3
]).

random_salt() ->
    Now = lists:concat(tuple_to_list(now())),
    Random = integer_to_list(random:uniform(10000000)),
    Now ++ Random.

hashed_value(Secret, Value, Salt) ->
    SaltedValue = Value ++ Salt,
    <<SessionIdHash:160/integer>> = crypto:sha_mac(Secret, SaltedValue),
    lists:flatten(io_lib:format("~40.16.0b", [SessionIdHash])).

