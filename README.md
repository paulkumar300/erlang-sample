## Erlang Sample Project

Create a server application that users can treat as cloud backup.
The server understands 3 cmds, `post`, `get` and `list`.
Define and document the packet protocol for the same.

A light weight client application can be invoked from any client machine.
This should connect to the server and help perform the 3 operations listed above.

Both the server and client application needs to have unit tests (probably use googletest or anything of your choice).

Both server and client should not have any parameters hardcoded. If parameters are needed, they
should be defined part of a configuration file and source from it.

A top level makefile should exist, with below targets.
* make server
* make client
* make clean/clean_server/clean_client/
* make server_tests // should run unit test framework on server
* make client_tests // should run unit test framework on clients.

Proper documentation of server/client/unit test frameworks and how to run them are essential. You can edit this readme to add your own content like build instruction, dependencies to be installed and instruction to test and run the program.

---
This project contains a server application `cloud_backup_server` which can be treated as a cloud backup and a client application `cloud_backup_client`. Both client and server applications are developed using Erlang functional programming language. The server and client applications can be tested, build and started separately. 

## Prerequisites ##
A prior knowledge about erlang functional programming language would be benificial for better understanding of the project.

## Dependancy ##
Erlang is the only dependancy to build and run the application, so you should install Erlang in your personal computer before heading to start the application. You can get the  pre-built binary packages for OS X, Windows, Ubuntu, Debian, Fedora, CentOS, Raspbian and other operating systems from [here](https://www.erlang-solutions.com/resources/download.html). I personally suggest you to download erlang OTP version 18.0 or above.

## Server and Client Configurations ##

There is configuration file present in `./config` directory present in both `cloud_backup_server` and `cloud_backup_client` project directory. The name of the file is `vm.args`.

Server configurations are:
```
-cloud_backup_server port_number 5678
-cloud_backup_server target_folder_name '"./storage/"'

```
For server the tcp port number and the directory for storing the files can be changed according to the user requirements.
For example if i want my server to listen at port number 9090 and the storage location is /home/user/safeplace, then the configuration parameters will look like as given below:
```
-cloud_backup_server port_number 9090
-cloud_backup_server target_folder_name '"/home/user/safeplace"'

```
Client configurations are:
```
-cloud_backup_client ip_address '"localhost"'
-cloud_backup_client port_number 5678

```
The IP address and port number indicates the `server IP address and port number` where the request hans to be sent. if the localhost is mentioned as the IP address indicates both client ans server are running on same PC. If the server is running at an IP address 192.168.10.1 and Port number 9090 then the configuration values will look as shown below,
```
-cloud_backup_client ip_address {192,168,10,1}
-cloud_backup_client port_number 9090

```

# Build release #
The project can be cloned to your system by running the following command:
```
 $ git clone git@github.com/jega-msys/.git

```
change directory to project directory:
```
   $ cd 
   
``` 
Run the following command to Build the server application:
```
   $ make server
   
``` 
The above command will create the build release for server application. If we want to start the application from the current directory we need to run the following command:
```
   $./cloud_backup_server/_build/default/rel/cloud_backup_server/bin/cloud_backup_server console
   
``` 
 Run the following command to Build the client application:
```
   $ make client
   
```   
Run the following command to start the client application:
```
   $ ./cloud_backup_client/_build/default/rel/cloud_backup_client/bin/cloud_backup_client console
   
``` 
### Note: ###
Once the build release is made for client or server application, application release will be present in `./cloud_backup_client/_build/default/` or `./cloud_backup_server/_build/default/` . The release folder can be copied to any other system where erlang vm is present(ie. Erlang is installed) and can start the applications by running the following commands,

For Client application
```
   $ ./rel/cloud_backup_client/bin/cloud_backup_client console
```
For server application
```
   $ ./rel/cloud_backup_server/bin/cloud_backup_server console
```

# Usage #
The server application understands 3 cmds, `post`, `get` and `list`. The client application contains client API for accessing the services offered by server application.

Start the client and server applications, All the functions given below has to be executed at the client console

### Get ###
How client get function is called.?
```
cloud_client:get(Name_of_the_file_to_be_downloaded, Path_where_file_has_to_be_downloaded).

Eg : cloud_client:get("sampl.txt", "/home/user/safe_directory").
```
The function returns:
```
{ sucess, "download_successful"}

{ error, "Error-file_is_not_present_in_server" }

{ error, Some_unexpected_errors }
```

### Post ###
How client post function is called.?
```
cloud_client:post( Complete_path_to_the_file).

Eg : cloud_client:post("/home/user/source_directory/sample.txt").
```
Function returns:
```
{ success, "upload_successful" };

{ error, "connection_closed"}

{ error, Some_unexpected_errors }
```

### List ###
How client list function is called.?
```
cloud_client:list().
```
Function returns:
```
{ success, List of file names };

{success, [ ] }

{ error, Some_unexpected_errors }
```
## How to run Unit test ##

There is a target available in make file to run unit test cases of server and client separately, here are they

```
$ make server_test

$ make client_test
```
