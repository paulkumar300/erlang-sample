%% ====================================================================
%% 	Module contains functions which provides get, post and list
%%	Functionalities.
%% ====================================================================
-module(cloud_server).

%% ====================================================================
%% Exported Functions
%% ====================================================================
-export([ 	
		start_link/0, 
		server_process/0
	]).

%% --------------------------------------------------------------------
%% 	This function is called by application supervisor and it starts
%%	the cloud backup server process.
%%
%%	Function has no argument
%%
%%	Function returns : { ok, process-id of cloud backup server }.
%%
%%	This return format is important becuase the application supervisor
%%	which calls this function expects return value in the above given
%%	format
%% ---------------------------------------------------------------------

start_link() ->

	Pid = erlang:spawn_link(?MODULE, server_process, []),
	{ ok, Pid }.

%% --------------------------------------------------------------------
%%  	This is the server process which listens on the port number
%%    	which is read from the application environment variable.
%%
%%	The function has no argument and no return value
%% --------------------------------------------------------------------

server_process() ->

	case application:get_env( cloud_backup_server, port_number) of
	
		undefined ->
			%io:format(" Connection Port is not available~n"),
			server_process();

		{ ok, Port} ->
			io:format(" Connection Port ~p~n",[Port]),
			case gen_tcp:listen(Port, [list, {packet, 0}, {active, false}]) of

				{ok, Listening_socket} -> 
					wait_for_request( Listening_socket );

				{ error, Reason } ->
					io:format("Failed to listen on Port: ~p due to error: ~p~n",[ Port, Reason ])
			end
	end.

%% --------------------------------------------------------------------
%% 	This function waits for the tcp connection requests made by
%%	clients. Once the connection is accepted it calls the message
%%	receive looping function.
%%
%%	The function is in a never ending loop and when any client closes
%%	its connection then waits for other clients connection request
%%
%%	The function receives the listening socket number
%%
%%	The function does not return anything
%% --------------------------------------------------------------------

wait_for_request( Listening_socket ) ->

	case gen_tcp:accept(Listening_socket) of
		{ok, Socket} ->
			tcp_message_receive_loop(Socket),
			wait_for_request( Listening_socket );

		{error, Reason} ->
			io:format(" Connection failed because ~p~n",[Reason]),
			wait_for_request( Listening_socket )
	end.

%% --------------------------------------------------------------------
%% 	The function waits for the request send by the client
%%	and performs operations based on the request 
%%
%%	The message format is: 
%%		{ tcp::atom(), Socket::listening socket, Request/Data}
%%
%%	Currently function supports following requests
%%		1. post
%%		2. list
%%		3. get 
%%
%%	When a port is closed then it receives a message in the below
%%	given format
%%		{tcp_closed,Socket}
%%
%%	The function receives Listening socket as argument
%% --------------------------------------------------------------------

tcp_message_receive_loop(Socket) ->

	inet:setopts(Socket,[{active,once}]),

    	receive
		{tcp,Socket,"post"} ->

			io:format("post Message is received ~n"),

			inet:setopts(Socket,[{active,false}]),
			Reply = receive_file_name_and_data( Socket ),
			gen_tcp:send(Socket, Reply);

		{tcp,Socket,"list"} ->

			List_of_file_names = get_name_of_all_files(),
			case List_of_file_names of
				[] ->
					io:format("list of files ~p ~n",[List_of_file_names]),
					gen_tcp:send(Socket, "no_files_present" );

				_files_are_present ->
					io:format("list of files ~p ~n",[List_of_file_names]),
					gen_tcp:send(Socket, string:join( List_of_file_names, ",") )
			end;

		{tcp,Socket,"get"} ->
			inet:setopts(Socket,[{active,false}]),
			case gen_tcp:recv(Socket, 0) of
				{ok, File_name} ->
					send_requested_file_to_client( File_name, Socket );

				{error, closed} ->
					io:format("Socket ~w closed [~w]~n",[Socket,self()]),
            				ok
			end;

		{tcp_closed,Socket} ->
		    io:format("Socket ~w closed [~w]~n",[Socket,self()]),
		    ok
	    end.

%% --------------------------------------------------------------------
%% 	This function is used to read the file name and data to write
%%	into that file through tcp port.
%%
%%	This function is called when the message receive loop receives a
%%	post request. Because post request is expecting file name and content
%%	to write into the file.
%%
%%	This function receives Listening socket number as argument
%%
%%	Function returns an atom 'ok'	
%% --------------------------------------------------------------------

receive_file_name_and_data( Socket ) ->

	case gen_tcp:recv(Socket, 0) of
		{ok, File_name} ->
			io:format("File_name ~p ~n",[File_name]),
			case gen_tcp:recv(Socket, 0) of
				{ok, Data_to_write_into_file} ->
					io:format("Data_to_write_into_file ~p ~n",[Data_to_write_into_file]),
					save_file(Data_to_write_into_file, File_name );

				{error, closed} ->
					io:format("Socket ~w closed [~w]~n",[Socket,self()]),
					"Error-connection closed"
			end;

		{error, closed} ->
			io:format("Socket ~w closed [~w]~n",[Socket,self()]),
    			"Error-connection closed"
	end.

%% --------------------------------------------------------------------
%% 	The function recives the file name and data to write into the file
%%	as argument and saves it mentioned location.
%%	
%%	The argument Data_to_write_into_file should contain binary value only and
%%	The argument File_name should contain the name of the file in a string
%%	eg : "some_file.txt"
%%
%%	Function write the content into the file and save the file into the
%%	directory given in the target directory environment variable set through
%%	vm.args
%%
%%	Function returns an atom 'ok' 	
%% --------------------------------------------------------------------
save_file(Data_to_write_into_file, File_name ) ->

	case application:get_env( cloud_backup_server, target_folder_name) of
	
		undefined ->
			io:format("~n Set target folder name in VM.args file~n"),
			"Error-upload failed";

		{ ok, Target_folder_name} ->
			io:format("~n Filename: ~p~p~n",[ Target_folder_name, File_name ]),
			{ok, Fd} = file:open(Target_folder_name++"/"++File_name, write),
			file:write(Fd, erlang:list_to_binary(Data_to_write_into_file)),
			file:close(Fd),
			"upload_successful"
	end.

%% --------------------------------------------------------------------
%% 	The function reads name of all the files present in the directory 
%%	given in the target directory environment variable set through
%%	vm.args
%%
%%	Function returns list of file names separated by commas
%%	eg : "some_file1.txt, sample_file2.txt"		
%% --------------------------------------------------------------------

get_name_of_all_files() ->

	case application:get_env( cloud_backup_server, target_folder_name) of
	
		undefined ->
			io:format("~n Set target folder name in VM.args file~n"),
			[];

		{ ok, Target_folder_name} ->
			io:format("~n Storage folder is: ~p~n",[ Target_folder_name ]),
			case file:list_dir_all(Target_folder_name) of
				{ok, Result } -> 
					Result;
				{error, _Reason} ->
					[]
			end
	end.

%% --------------------------------------------------------------------
%% 	The function reads and sends the file requested file to the client
%%
%%	Function takes file name and Listening socket as arguments and
%%	reads the file from the target directory and send to client.
%%
%%	Function returns an atom 'ok'
%% --------------------------------------------------------------------
send_requested_file_to_client( File_name, Socket ) ->

	case application:get_env( cloud_backup_server, target_folder_name) of
	
		undefined ->
			io:format("~n Set target folder name in VM.args file~n"),
			gen_tcp:send(Socket, "Error-file_is_not_present_in_server");

		{ ok, Target_folder_name} ->

			case lists:member( File_name, get_name_of_all_files()) of
				true->
					Ret=file:sendfile(Target_folder_name++"/"++File_name, Socket), 
					io:format("result ~p ~n", [ Ret ]);

				false ->
					gen_tcp:send(Socket, "Error-file_is_not_present_in_server"),
					io:format("File: ~p is not available ~n", [ File_name ])
			end
	end.

