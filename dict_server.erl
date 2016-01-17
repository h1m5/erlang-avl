-module(dict_server).
-export([start/0, server/0]).
-include("interface.hrl").
-include("config.hrl").

server() ->
	process_flag(trap_exit, true),
	server([]).

setupBtree() ->
	case whereis(btree) of
		undefined ->
			io:format("here~n"),
			register(btree, spawn(btree, start, []))
	end.
	

server(User_List) ->
	io:format("User list = ~p~n", [User_List]),
		receive
			#logon{client_pid=From, username=Name} ->
				New_User_List = server_logon(From, Name, User_List),
				server(New_User_List);
			{'EXIT', From, _} ->
				New_User_List = server_logoff(From, User_List),
				server(New_User_List);
			#request{client_pid=From, request=Request} ->
				server_transfer(From, Request, User_List),
				server(User_List)
		end,
		io:format("Terminated~n").

start() ->
	register(dictionary, spawn(?MODULE, server, [])),
	setupBtree().

server_logon(From, Name, User_List) ->
	case lists:keymember(Name, 2, User_List) of
		true ->
			From ! #abort_client{message=user_exixts_at_another_node},
			User_List;
		false ->
			From ! #server_alert{message=logged_on},
			link(From),
			[{From, Name} | User_List]
	end.

server_logoff(From, User_List) ->
	lists:keydelete(From, 1, User_List).

server_transfer(From, Request, User_List) ->
	case lists:keysearch(From, 1, User_List) of
		false ->
			From ! #abort_client{message=you_ar_not_logged_on};
		{value, {_, Name}} ->
			%search through dictionary for requested word
			io:format("~p requested ~p~n", [Name, Request]),
			btree ! {fetch_word, Request, self()},
			receive
				Definition ->  
					From ! #server_reply{word=Request, meaning=Definition}
			end
	end.