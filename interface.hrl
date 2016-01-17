-record(logon,{client_pid, username}).
-record(request,{client_pid, request}).

-record(abort_client,{message}).

-record(server_reply,{word, meaning}).

-record(request_word,{word}).

-record(server_alert,{message}).

-record(node, {key, val, left = nil, right = nil, height = 1}).