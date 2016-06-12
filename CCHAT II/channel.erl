-module(channel).
-export([handle/2, initial_state/1]).

-include_lib("./defs.hrl"). 

initial_state(Channel) -> 
	#channel_st{channel = Channel, user = []}. 

% User connect
handle(St, {join, Nick, Pid}) ->
	NewState = St#channel_st{user = [{Nick, Pid} | St#channel_st.user]},
	% Checks if the nick is already taken
	case lists:keymember(Nick, 1, St#channel_st.user) of
		true ->
			{reply, user_already_exists, St};
		false ->
			{reply, ok, NewState}
	end;

% User disconnect
handle(St, {leave, Nick, Pid}) ->
	NewState = St#channel_st{user = lists:delete({Nick, Pid}, St#channel_st.user)},
	% Checks if the user exists in channel list
	case lists:member({Nick, Pid}, St#channel_st.user) of
		true ->
			{reply, ok, NewState};
		false ->
			{reply, user_not_existing, St}
	end;

% Send message to all clients
handle(St, {send, Pid, Nick, Msg}) ->
	Data = {incoming_msg, St#channel_st.channel, Nick, Msg},
	% Checks if the user exists in channel list
	case lists:member({Nick, Pid}, St#channel_st.user) of 
		true ->
			% Spawns a list of every process except for the person that sends the message 
			[spawn(fun() -> genserver:request(X, Data) end) || {_, X} <- St#channel_st.user, X /= Pid],
			{reply, ok, St};
		false ->
			{reply, user_not_existing, St}
	end.
