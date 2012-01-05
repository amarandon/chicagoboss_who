-module(cb_who_account_controller, [Req, SessionID]).
-compile(export_all).
-default_action(login).

signup('GET', []) ->
    {ok, []};

signup('POST', []) ->
    Errors = validate([
        { fun check_password_confirmation/1, "Passwords don't match" },
        { fun(Req) -> check_password_length(Req, 5) end, 
            "Password should be more than 5 characters long" }
    ]),
    Account = create_account(),
    Resp = case {Errors, Account:save()} of 
        {[], {ok, SavedAccount}} -> 
            confirm_account_creation(SavedAccount);
        {_, {ok, SavedAccount}} ->
            {ok, [{errors, Errors}, {account, Account}]};
        {_, {error, ModelErrors}} ->
            {ok, [{errors, ModelErrors ++ Errors}, {account, Account}]}
    end,
    Resp.


login('GET', []) ->
    {ok, []};

login('POST', []) ->
    Username = Req:post_param("username"),
    Password = Req:post_param("password"),
    MatchingAccounts = boss_db:find(account, [username = Username, hashed_password = Password]),
    case MatchingAccounts of
        [] ->
            {ok, [{error, "Invalid password"}]};
        [Account|_] ->
            boss_session:set_session_data(SessionID, username, Username),
            {redirect, login_redirect()}
    end.

logout('GET', []) ->
    boss_session:remove_session_data(SessionID, username),
    {redirect, login_redirect()}.

% Internal functions
login_redirect() ->
    boss_env:get_env(cb_who, login_redirect, "/").

activation_required() ->
    boss_env:get_env(cb_who, activation_required, true).

validate(ValidationList) ->
    lists:foldl(fun({ValidationFun, ErrorMessage}, AccIn) ->
                    case ValidationFun(Req) of 
                        true -> AccIn;
                        false -> [ErrorMessage|AccIn]
                    end
                end, [], ValidationList).

check_password_confirmation(Req) ->
    Req:post_param("password") =:= Req:post_param("password-confirm").

check_password_length(Req, Length) ->
    length(Req:post_param("password")) > Length.

activation_token() ->
    Secret = boss_env:get_env(boss, session_key, cb_who_utils:random_salt()),
    Value = Req:post_param("username") ++ Req:post_param("email"),
    Salt = cb_who_utils:random_salt(),
    cb_who_utils:hashed_value(Secret, Value, Salt).

create_account() ->
    HashedPassword = Req:post_param("password"),
    Account = case activation_required() of 
        true -> 
            account:new(id, Req:post_param("username"), Req:post_param("email"), 
                HashedPassword, false, activation_token(), erlang:now());
        false ->
            account:new(id, Req:post_param("username"), Req:post_param("email"), 
                HashedPassword, true, nil, nil)
    end.

confirm_account_creation(Account) ->
    case Account:activated() of
        true ->
            {redirect, [{controller, "account"}, {action, "login"}]};
        false ->
            boss_mail:send_template(cb_who, confirmation, [Account]),
            {render_other, [{controller, "account"}, 
                            {action, "confirmation_sent"}],
                           [{email, Account:email()}]}
    end.
