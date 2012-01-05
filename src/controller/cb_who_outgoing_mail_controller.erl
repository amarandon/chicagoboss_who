-module(cb_who_outgoing_mail_controller).
-compile(export_all).

confirmation(Account) ->
  FromAddress = boss_env:get_env(boss, mail_default_from, "no-reply@example.com"),
  ToAddress = Account:email(),
  Subject = "Please confirm your email address",
  HeaderFields = [{"From", FromAddress}, {"To", ToAddress}, 
    {"Subject", "Email confirmation"}],
  Variables = [{email, ToAddress}],
  {ok, FromAddress, ToAddress, HeaderFields}.
