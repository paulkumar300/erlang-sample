%% ====================================================================
%% 	Module contains client functions to access get, post and list
%%	Functionalities offered by server
%% ====================================================================
-module(cloud_client).

%% ====================================================================
%% Exported Functions
%% ====================================================================
-export([
	get/2, 
	post/1, 
	list/0 ]).

%% --------------------------------------------------------------------
%% 	This function used to upload file in server
%%
%%	Function receives the file path argument as string as shown below
%%
%%	Example for File_location : "/home/user/source/sample.txt"
%%
%%	on successful uploading function returns
%%		{success, "success"}
%%
%%	if the upload fails then function returns
%%		{error, Reason for failure}
%% ---------------------------------------------------------------------

post( File_location ) ->

	List_of_environment_variables = application:get_all_env(cloud_backup_client),
	
	{ port_number, Port} = lists:keyfind( port_number, 1, List_of_environment_variables),
	{ ip_address, IP_address} = lists:keyfind( ip_address, 1, List_of_environment_variables),

    	case gen_tcp:connect(IP_address,Port,[{active,false}, {packet,0}]) of

		{ok,Socket} ->
			gen_tcp:send(Socket, "post"),
			timer:sleep(1000),
			File_name = lists:last(string:tokens(File_location, "/")),
			gen_tcp:send(Socket, File_name),

			Ret=file:sendfile(File_location, Socket), 
			io:format("result ~p ~n", [ Ret ]),

			{ok, Return} = gen_tcp:recv(Socket,0),
			io:format("~n Acknowledgement: ~p ~n", [Return]),

		    	gen_tcp:close(Socket),
			case Return of
				"upload_successful" ->
					{ success, Return };
				Error ->
					{ error, Error }
			end;

		{ error, Reason } ->
			io:format("~n Failed to connect to port:~p Because of:~p ~n", [Port, Reason]),
			{ error, Reason }
	end.

%% --------------------------------------------------------------------
%% 	This function used download a files present in server
%%
%%	Function receives the File name and Directory path where the 
%%	file has to be downloaded as argument as shown below
%%
%%	Example for File_name : "sample.txt"
%%
%%	Example for Location_to_download : "/home/user/target/"
%%
%%	on successful downloadin function returns
%%		{success, "download_successful"}
%%
%%	if the download fails then function returns
%%		{error, Reason for failure}
%% ---------------------------------------------------------------------

get( File_name, Location_to_download ) ->

	List_of_environment_variables = application:get_all_env(cloud_backup_client),
	
	{ port_number, Port} = lists:keyfind( port_number, 1, List_of_environment_variables),
	{ ip_address, IP_address} = lists:keyfind( ip_address, 1, List_of_environment_variables),

    	case gen_tcp:connect(IP_address,Port,[{active,false}, {packet,0}]) of

		{ok,Socket} ->

			gen_tcp:send(Socket, "get" ),
			timer:sleep(1000),
			gen_tcp:send(Socket, File_name ),

			{ok, Data_to_write_into_file} = gen_tcp:recv(Socket,0),

			case Data_to_write_into_file of
				"Error-file_is_not_present_in_server" ->
					{ error, "Error-file_is_not_present_in_server" };

				_Valid_data ->
					io:format("~nFilename: ~p ~n", [Location_to_download++File_name]),
					{ok, Fd} = file:open(Location_to_download++File_name, write),
					file:write(Fd, Data_to_write_into_file),
					file:close(Fd),

				    	gen_tcp:close(Socket),
					{ sucess, "download_successful"}
			end;

		{ error, Reason } ->
			io:format("~n Failed to connect to port:~p Because of:~p ~n", [Port, Reason]),
			{ error, Reason }
	end.

%% --------------------------------------------------------------------
%% 	This function used list all files present in server
%%
%%	Function doesnot have any argument
%%
%%	if the request is successful function returns
%%		{success, List_of_file_names}
%%
%%	Example for List_of_file_names : 
%%		[ "sample1.txt", "sample2.txt"] 
%%
%%	if the request fails then function returns
%%		{error, Reason for failure}
%% ---------------------------------------------------------------------

list() ->

	List_of_environment_variables = application:get_all_env(cloud_backup_client),
	
	{ port_number, Port} = lists:keyfind( port_number, 1, List_of_environment_variables),
	{ ip_address, IP_address} = lists:keyfind( ip_address, 1, List_of_environment_variables),

	io:format("~n Port number:~p, IP address:~p ~n", [Port, IP_address]),

    	case gen_tcp:connect(IP_address,Port,[{active,false}, {packet,0}]) of

		{ok,Socket} ->
			gen_tcp:send(Socket, "list" ),
			{ok, List_of_file_names} = gen_tcp:recv(Socket,0),
			gen_tcp:close(Socket),
			case List_of_file_names of
				"no_files_present" ->
					{success, [ ] };

				_file_names ->
					io:format("~n List of Filename: ~p ~n", [string:tokens(List_of_file_names,",")]),
					{success, string:tokens(List_of_file_names,",")}
			end;

		{ error, Reason } ->
			io:format("~n Failed to connect to port:~p Because of:~p ~n", [Port, Reason]),
			{ error, Reason }
	end.

