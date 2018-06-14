
.PHONY: server client clean clean_server clean_client server_test client_test

all: server client


server:
	 cd ./cloud_backup_server && ./rebar3 release

client:
	cd ./cloud_backup_client && ./rebar3 release

server_test:
	cd ./cloud_backup_server && ./rebar3 eunit --application=cloud_backup_server

client_test:
	cd ./cloud_backup_server && ./rebar3 eunit --application=cloud_backup_client

clean:
	make clean_server clean_client

clean_server:
	@ - rm -rf ./cloud_backup_server/_build
	@ - rm -rf ./cloud_backup_server/storage
	@ - rm -rf ./cloud_backup_server/source

clean_client:
	@ - rm -rf ./cloud_backup_client/_build

