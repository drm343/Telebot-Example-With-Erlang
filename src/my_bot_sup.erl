-module(my_bot_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(State) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [State]).


init([State]) ->
    ChildSpecs = [#{id => my_bot_command,
                    start => {my_bot_command, start_link, [State]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [my_bot_command]}],
    % 在 5 秒內 restart 超過 3 次，整個程式會死亡，避免無限回圈
    {ok, {{one_for_one, 3, 5}, ChildSpecs}}.
