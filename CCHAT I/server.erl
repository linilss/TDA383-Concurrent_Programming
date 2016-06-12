-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{channel = [], user = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

% Connect to server
handle(St, {connect, Nick, Pid}) ->
	NewState = St#server_st{user = [{Nick, Pid} | St#server_st.user]},
	% Checks if nick is already taken	
	case lists:keymember(Nick, 1, St#server_st.user) of
		true ->
			{reply, already_connected, St};
		false ->
			{reply, ok, NewState}
	end;

% Disconnect from server
handle(St, {disconnect, Nick, Pid}) ->
	NewState = St#server_st{user = lists:delete({Nick, Pid}, St#server_st.user)},
	% Checks if the user exists in server list
	case lists:member({Nick, Pid}, St#server_st.user) of
		true ->
			{reply, ok, NewState};
		false ->
			{reply, not_connected, St}
	end;

% Join channel
handle(St, {join, Channel}) ->
	NewState = St#server_st{channel = [Channel | St#server_st.channel]},
	% Checks if the channel already exists or should creat a new one
	case lists:member(Channel, St#server_st.channel) of
		false ->
			% Starts a new channel
			genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:handle/2),
			{reply, join, NewState};
		true ->
			{reply, join, St}
	end.


