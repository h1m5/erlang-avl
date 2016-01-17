-module(dict_client).
-export([client/2]).
-include("interface.hrl").

client(Server_Node, Name) ->
	{dictionary, Server_Node} ! #logon{client_pid=self(), username=Name},
	await_result(),
	client(Server_Node).

client(Server_Node) ->
	receive
		logoff ->
			exit(normal);
		#request_word{word=AWord} ->
			{dictionary, Server_Node} !
				#request{client_pid=self(), request=AWord},
			await_result;
		#server_reply{word=AWord, meaning=Meaning} ->
			io:format("~p: ~p~n", [AWord, Meaning])
	end,
	client(Server_Node).

await_result() ->
	receive
		#abort_client{message=Why} ->
			io:format("~p~n", [Why]),
			exit(normal);
		#server_alert{message=What} ->
			io:format("~p~n", [What])
	after 5000 ->
			io:format("No response from server~n", []),
			exit(timeout)
	end.