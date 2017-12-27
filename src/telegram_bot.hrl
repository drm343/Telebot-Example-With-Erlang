%% state 這個 record 會儲存 bot 的名稱跟 token，因為直接抓回傳資料可能會發生
%% bot 名稱不同而無法發出訊息或程式掛掉的情況，所以需要自己手動儲存
-record(state, {name, token}).
