./rebar compile #this line checks if any modules were modified, and recompiles them if they were. only needed during testing.
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(exchanges)"

