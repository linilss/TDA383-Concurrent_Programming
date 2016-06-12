-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st {gui = GUIName,
    			nick = Nick,
    			server = not_connected,
    			channel = []}.

% 	Connect to server
%
% 	Attempts to connect to a server. If it succeeds, ok is returned and ok, 
%	NewState is returned to GUI. Otherwise, if the user is already connected
%	user_already_connected is returned, or if some other error occurs, 
%	server_not_reached is returned.
handle(St, {connect, Server}) ->
	NewState = St#client_st{server = Server},
	ServerAtom = list_to_atom(Server),
	Data = {connect, St#client_st.nick, self()},
	case catch(genserver:request(ServerAtom, Data)) of
		ok ->
	    	{reply, ok, NewState};
	    already_connected ->
	    	{reply, {error, user_already_connected, "The user is already connected!"}, St};
	    {'EXIT', _} ->
	    	{reply, {error, server_not_reached, "Server is not reachable!"}, St}
    end;

% 	Disconnect from server
%
% 	Uses two case statements to filter out possible errors that might occur 
%	when trying to disconnect from a server, e g if there is no  server to 
%	disconnect from, the program returns the atom error, user_not_connected.
% 	Or if the user still is connected to any channel, client returns the atom
%	leave_channels_first. Otherwise returns ok,NewState if the disconnect is 
%	successful, meaning the user has been removed from the server and the 
%	server in client_st has been set to the atom not_connected.
handle(St, disconnect) ->
	NewState = St#client_st{server = not_connected},
	Data = {disconnect, St#client_st.nick, self()},
    case St#client_st.channel of
    	[] ->
    		case St#client_st.server of
    			not_connected ->
    				{reply, {error, user_not_connected, "You are not connected to any server!"}, St};
    			_ ->
    				ServerAtom = list_to_atom(St#client_st.server),
    				case catch(genserver:request(ServerAtom, Data)) of
    					ok ->
    						{reply, ok, NewState};
    					{'EXIT', _} ->
    						{reply, {error, server_not_reached, "Could not disconnect from the server!"}, St}
    				end
    		end;
    	_ ->
    		{reply, {error, leave_channels_first, "You need to leave all channels first!"}, St}
    end;

% 	Join channel
%
% 	A couple of case statements catches errors such	as if the user is not
%	connected to any server, not_connected is retrieved from the first case
%	statement and thus the user can't join any channel and client returns
%	user_not_connected to GUI. Also, if 'EXIT' is returned from the server
%	server_not_reached is returned to GUI. Otherwise the client requests
%	an ok from channel via genserver. If so, NewState is returned to GUI
% 	with the channel added in client_st. If channel instead returns 
%	user_already_exists, user_already_joined is returned to GUI.
handle(St, {join, Channel}) ->
	NewState = St#client_st{channel = [Channel | St#client_st.channel]},
	ChannelAtom =list_to_atom(Channel),
	SData = {join, Channel},
	CData = {join, St#client_st.nick, self()},
	case St#client_st.server of
		not_connected ->
			{reply, {error, user_not_connected, "You are not connected to any server!"}, St};
		_ ->
			ServerAtom = list_to_atom(St#client_st.server),
			case catch(genserver:request(ServerAtom, SData)) of
				join ->
					case catch(genserver:request(ChannelAtom, CData)) of	
						user_already_exists ->
							{reply, {error, user_already_joined, "User is already connected to channel!"}, St};
						ok ->
							{reply, ok, NewState}
					end;
				{'EXIT', _} ->
					{reply, {error, server_not_reached, "Server is not reachable!"}, St}
			end
	end;			

% 	Leave channel
%
% 	A case statement keeps track of whether the user is connected to the given
%   channel or not. That is done by requesting an OK from channel via 
%	genserver. If the user isn't connected to the channel, it returns an atom 
%	user_not_existing and thus atom user_not_joined is returned to GUI.
handle(St, {leave, Channel}) ->
    NewState = St#client_st{channel = lists:delete(Channel, St#client_st.channel)},
    ChannelAtom = list_to_atom(Channel),
    Data = {leave, St#client_st.nick, self()},
    case catch(genserver:request(ChannelAtom, Data)) of
    	ok ->
    		{reply, ok, NewState};
    	user_not_existing ->
    		{reply, {error, user_not_joined, "User not joined to channel!"}, St}
    end;

% 	Sending messages
%
%	Passes Data to the Channel, through genserver, to be computed. If the user
% 	is not connected to the server it tries to print in, an atom 
% 	user_not_existing is returned from channel and user_not_joined is 
%	returned to GUI, otherwise ok is returned to GUI.
handle(St, {msg_from_GUI, Channel, Msg}) ->
	ChannelAtom = list_to_atom(Channel),
	Data = {send, self(), St#client_st.nick, Msg},
	case catch(genserver:request(ChannelAtom, Data)) of
		ok ->
			{reply, ok, St};
		user_not_existing ->
			{reply, {error, user_not_joined, "User not joined to channel!"}, St}
	end;

%	Get current nick
%
%	Returns the nick of the client's user, stored in the client_st.
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%   Change nick
%
%	Attempts to change the nick of the user of the client. Since it's only 
%	possible to change nick as long as the user isn't connected, the program 
%	checks if the user is connected to a server. If not, it returns the new
% 	with the new nick, otherwise returns atom user_already_connected.
handle(St, {nick, Nick}) ->
	NewState = St#client_st{nick = Nick},
	case St#client_st.server of
		not_connected ->
			{reply, ok, NewState};
		_ ->
			{reply, {error, user_already_connected, "Can't change nick while connected to a server!"}, St}
		end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.