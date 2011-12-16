-module(cb_who_account_controller, [Req, SessionID]).
-compile(export_all).
-default_action(signin).

signup('GET', []) ->
    {ok, []};

signup('POST', []) ->
    ValidationList = [
        { fun check_password_confirmation/1, "Passwords don't match" },
        { fun(Req) -> check_password_length(Req, 5) end, 
            "Password should be more than 5 characters long" }
    ],
    Errors = lists:foldl(fun({ValidationFun, ErrorMessage}, AccIn) ->
                case ValidationFun(Req) of 
                    true -> AccIn;
                    false -> [ErrorMessage|AccIn]
                end
        end, [], ValidationList),
    HashedPassword = Req:post_param("password"),
    Account = account:new(id, Req:post_param("username"), Req:post_param("email"), 
        HashedPassword),
    Resp = case {Errors, Account:save()} of 
        {[], {ok, SavedAccount}} ->
            {redirect, "/account/signin"};
        {_, {ok, SavedAccount}} ->
            {ok, [{errors, Errors}, {account, Account}]};
        {_, {error, ModelErrors}} ->
            {ok, [{errors, ModelErrors ++ Errors}, {account, Account}]}
    end,
    Resp.

signin('GET', []) ->
    {ok, []};

signin('POST', []) ->
    Username = Req:post_param("username"),
    Password = Req:post_param("password"),
    MatchingAccounts = boss_db:find(account, [username = Username, hashed_password = Password]),
    case MatchingAccounts of
        [] ->
            {ok, [{error, "Invalid password"}]};
        [Account|_] ->
            boss_session:set_session_data(SessionID, username, Username),
            {redirect, "/"}
    end.

signout('GET', []) ->
    boss_session:remove_session_data(SessionID, username),
    {redirect, "/"}.

% Internal functions
check_password_confirmation(Req) ->
    Req:post_param("password") =:= Req:post_param("password-confirm").

check_password_length(Req, Length) ->
    io:format("Password: ~p~n", [Req:post_param("password")]),
    length(Req:post_param("password")) > Length.

