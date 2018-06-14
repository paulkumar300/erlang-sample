%% -------------------------------------------------------------
%% 	Module contains test cases to test get, post and list
%%	Functionalities.
%% -------------------------------------------------------------
-module(cloud_server_tests).
-include_lib("eunit/include/eunit.hrl").


all_test_() ->
    	{setup, fun setup/0, fun teardown/1,
     	[
		{timeout, 10, [ fun post_function/0, 
				fun list_function/0,  
				fun get_function_success/0,
				fun get_function_error/0  ] }
	]}.

setup() ->
    ok = application:start(cloud_backup_server),

	application:set_env( cloud_backup_server, port_number, 5678),
	application:set_env( cloud_backup_server, target_folder_name, "./storage"),

	file:make_dir("./source"),
	file:write_file("./source/sample.txt", <<"hai....">>),
	file:make_dir("./storage").

teardown(_) ->
	io:format("tearing down"),
	file:del_dir("./storage"),
	file:del_dir("./source"),
	ok = application:stop(cloud_backup_server).


post_function() ->
    	?assertMatch("upload_successful",post("./source/sample.txt")).

post(File_location) ->
	
	{ok,Sock} = gen_tcp:connect("localhost",5678,[{active,false}, {packet,0}]),
	gen_tcp:send(Sock, "post"),
	timer:sleep(1000),
	File_name = lists:last(string:tokens(File_location, "/")),
	gen_tcp:send(Sock, File_name),
	timer:sleep(1000),
	file:sendfile(File_location, Sock), 
	{ok, Reply_from_server} = gen_tcp:recv(Sock,0),
	gen_tcp:close(Sock),
	Reply_from_server.


list_function() ->
    	?assertMatch("sample.txt",list_files()).

list_files() ->
	{ok,Sock} = gen_tcp:connect("localhost",5678,[{active,false}, {packet,0}]),
	gen_tcp:send(Sock, "list" ),
	{ok, List_of_file_names} = gen_tcp:recv(Sock,0),
    	gen_tcp:close(Sock),
	List_of_file_names. 


get_function_success() ->
    	?assertMatch("hai....",get_file("sample.txt")).

get_function_error() ->
    	?assertMatch("Error-file_is_not_present_in_server",get_file("sam.txt")).

get_file(File_name) ->
	{ok,Sock} = gen_tcp:connect("localhost",5678,[{active,false}, {packet,0}]),
	gen_tcp:send(Sock, "get"),
	timer:sleep(1000),
	gen_tcp:send(Sock, File_name ),
	{ok, Data_to_write_into_file} = gen_tcp:recv(Sock,0),
	gen_tcp:close(Sock),
	Data_to_write_into_file.

