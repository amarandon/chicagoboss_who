-module(account, [Id, Username, Email, HashedPassword]).
-compile(export_all).

validation_tests() ->
    [
        { fun() -> length(Email) > 0 end, 
            "Please provide a valid email address" },
        { fun() -> length(Username) > 1 end, 
            "Username must be more than 1 characters long" }
    ].
