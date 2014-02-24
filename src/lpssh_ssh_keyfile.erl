%%%---------------------------------------------------------------------------
%%% @doc
%%%   Callback module for SSH key handling. This module is responsible for
%%%   loading private RSA/DSA key, finding host's public key and adding host's
%%%   key to known hosts list.
%%%
%%% @see lpssh_call_ssh
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(lpssh_ssh_keyfile).

%% private key loading
-export([private_host_dsa_key/2, private_host_rsa_key/2]).
%% hosts' public key management
-export([lookup_host_key/3, add_host_key/3]).

%%%---------------------------------------------------------------------------
%%% private key
%%%---------------------------------------------------------------------------

%% @doc Load user's private DSA key for authentication.
%%   Typically, the key is stored in `~/.ssh/id_dsa' file.
%%
%% @spec private_host_dsa_key(system | user | {remoteuser, User}, list()) ->
%%   {ok, SSHKey} | {error, Reason}

private_host_dsa_key(Type, Opts) ->
  ssh_file:private_host_dsa_key(Type, Opts).

%% @doc Load user's private RSA key for authentication.
%%   Typically, the key is stored in `~/.ssh/id_rsa' file.
%%
%% @spec private_host_rsa_key(system | user | {remoteuser, User}, list()) ->
%%   {ok, SSHKey} | {error, Reason}

private_host_rsa_key(Type, Opts) ->
  ssh_file:private_host_rsa_key(Type, Opts).

%%%---------------------------------------------------------------------------
%%% hosts' public key management
%%%---------------------------------------------------------------------------

%% @doc Find and read host's public key from `known_hosts' file.
%%
%%   `Alg' is either `"ssh-dss"' or `"ssh-rsa"'.
%%
%% @spec lookup_host_key(string(), string(), list()) ->
%%   {ok, SSHKey :: term()} | {error, Reason}

lookup_host_key(Host, Alg, Opts) ->
  ssh_file:lookup_host_key(Host, Alg, Opts).

%% @doc Add host's public key to `known_hosts' file.
%% @spec add_host_key(string(), term(), list()) ->
%%   ok | {error, Reason}

add_host_key(Host, Key, Opts) ->
  ssh_file:add_host_key(Host, Key, Opts).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
