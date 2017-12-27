-module(my_bot_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % 一次啟動 1 個 server，一共啟動 5 個，死掉的 server 會自動重開一個新的
    ChildSpecs = [#{id => my_bot_command,
                    start => {my_bot_command, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [my_bot_command]}],
    {ok, {{one_for_one, 1, 5}, ChildSpecs}}.
