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
    pe4kin:launch_bot(State#state.name, State#state.token, #{receiver => true}),
    pe4kin_receiver:start_http_poll(State#state.name, #{limit=>100, timeout=>60}),
    my_bot_sup:start_link(State).

stop(_State) ->
    ok.
