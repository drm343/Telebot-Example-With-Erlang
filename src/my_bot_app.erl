-module(my_bot_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


-include("telegram_bot.hrl").

start(_Type, _Args) ->
    {ok, Config} = file:consult("etc/bot.config"),
    {name, Name} = lists:keyfind(name, 1, Config),
    {token, Token} = lists:keyfind(token, 1, Config),
    BotName = unicode:characters_to_binary(Name),
    BotToken = unicode:characters_to_binary(Token),
    State = #state{name = BotName, token = BotToken},
    my_bot_sup:start_link(State).

stop(_State) ->
    ok.
