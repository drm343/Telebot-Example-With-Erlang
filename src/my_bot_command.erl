%%% my_bot_command 會啟動一個專門處理 telegram 訊息的 server(後面簡稱 command
%%% server)。
%%%
%%% 在 start_link 中有一行寫 pe4kin_receiver:subscribe，從這行可以猜測 pe4kin
%%% 本身也是用 gen_server 實作的 OTP 程式，因為 command server 的 process 跟
%%% pe4kin server 的 process 不同，透過這行將 command server 的 PID 註冊給
%%% pe4kin server，pe4kin server 即可轉發訊息。
%%%
%%% Erlang gen_server 轉發訊息給其他 PID 時，會丟給 handle_info 這個函數，而不是
%%% 正常的 handle_call 或 handle_cast 這兩個函數，所以需要自己先簡單處理過訊息，
%%% 再丟給上面兩個函數處理。
%%%
%%% 需要同步的就走 handle_call，需要非同步的就走 handle_cast，在 telegram bot 的
%%% 狀況下，基本上不用考慮處理同步的狀況，只需要用非同步方式處理即可，如果是自
%%% 己新增模組 api 什麼的，那就可以考慮走 handle_call。
-module(my_bot_command).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2]).
%%% 尚未實作的 2 個函數，不實作也不影響使用，但如果需要升級 app，則需要實作
%%%
%%% 這個函數跟程式碼升級、降級有關，hotswap 時使用。
%%% code_change/3,
%%%
%%% 用在 server 啟動失敗或 callback function 錯誤。
%%% terminate/2]).


-include("telegram_bot.hrl").


%% 讓 supervisor 用來連結 server 用的函數，完成後會 callback 執行這個模組內的
%% init 函數。
start_link(State) ->
    pe4kin_receiver:subscribe(State#state.name, ?MODULE),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [State], []).


stop() ->
    gen_server:cast(?MODULE, stop).


init([State]) ->
    {ok, State}.


%% 如果給使用者的 api 需要 server 處理完的回傳訊息(同步)，就把處理方式寫在
%% handle_call 這邊。
handle_call(_, _, State) ->
    {noreply, State}.


%% Request 訊息的拆包格式為 {{command, _, is_group?, _}, message, chat_id}。
%%
%% 處理非群組中發 /start command 給 bot 的情況，handle_info 沒有進一步處理
%% Request，所以要在這段再拆出自己要的資料來用。
handle_cast({bot_message, true, <<"/start">>, true, Message, ChatId}, State) ->
    % 從 dictionary 中取出發訊息者的 First Name，否則就當成匿名者。
    From = maps:get(<<"first_name">>, maps:get(<<"from">>, Message, #{}), <<"Anonumous">>),
    % 記得要把訊息處理成 unicode 的 binary 格式，否則中文可能會出錯。
    ResponseText = unicode:characters_to_binary(["Hello, 私人聊天的 ", From]),
    % 透過 pe4kin 轉發訊息給 telegram
    {ok, _} = pe4kin:send_message(State#state.name, #{chat_id => ChatId, text => ResponseText}),
    {noreply, State};
%% 處理群組中的使用者發 /start command 給 bot 的情況。
handle_cast({bot_message, true, <<"/start">>, false, Message, ChatId}, State) ->
    From = maps:get(<<"first_name">>, maps:get(<<"from">>, Message, #{}), <<"Anonumous">>),
    ResponseText = unicode:characters_to_binary(["Hello, 群組中的 ", From]),
    {ok, _} = pe4kin:send_message(State#state.name, #{chat_id => ChatId, text => ResponseText}),
    {noreply, State};
%% 處理使用者發 /macross command 給 bot 的情況，不分群組或個人。
handle_cast({bot_message, true, <<"/macross">>, _, Message, ChatId}, State) ->
    From = maps:get(<<"first_name">>, maps:get(<<"from">>, Message, #{}), <<"Anonumous">>),
    HeartEmoji = pe4kin_emoji:name_to_char('heart'),
    ResponseText = unicode:characters_to_binary([From, HeartEmoji, " Macross! "]),
    {ok, _} = pe4kin:send_message(State#state.name, #{chat_id => ChatId, text => ResponseText}),
    {noreply, State}.


%% 專門處理其他 server 轉發訊息用的函數。
%%
%% pe4kin_update 是 pe4kin 會轉發過來的訊息類型，除非有處理 pe4kin 以外的 server
%% 轉發過來的訊息，不然只要處理 pe4kin_update 就夠了。
handle_info({pe4kin_update, _, Update}, State) ->
    % 只處理 text 類型的訊息
    #{<<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}} = Message} = Update,
    text = pe4kin_types:message_type(Message),
    case pe4kin_types:is_bot_command(Message) of
        true ->
            {Command, _, Is_Group, _} = pe4kin_types:message_command(State#state.name, Message),
            BotMessage = #bot_message{
                            is_command = true,
                            command = Command,
                            is_group = Is_Group,
                            message = Message,
                            chat_id = ChatId
                           },
            gen_server:cast(?MODULE, BotMessage), % 把訊息整理後再次轉發給 command server 根據分類處理。
            {noreply, State};
        false ->
            {noreply, State}
    end.
