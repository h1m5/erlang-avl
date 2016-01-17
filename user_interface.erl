-module(user_interface).
-export([logon/1, logoff/0, request/1]).
-include("interface.hrl").
-include("config.hrl").

logon(Name) ->
	case whereis(dict_client) of
		undefined ->
			register(dict_client,
					spawn(dict_client, client, [?server_node, Name]));
		_ -> already_logged_on
	end.

logoff() ->
	dict_client ! logoff.

request(AWord) ->
	case whereis(dict_client) of
		undefined ->
			not_logged_on;
		_ -> dict_client ! #request_word{word=AWord},
			ok
	end.