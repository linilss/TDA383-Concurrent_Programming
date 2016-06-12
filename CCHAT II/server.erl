-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{channel = [], user = [], result = [], length = 0, pid = undefined}.

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
	end;

% Send tasks to connected clients
handle(St, {job, F, List, Pid}) ->
	NewState = St#server_st{length = length(List), result = [], pid = Pid},
	AssignedJobs = assign_tasks(St#server_st.user, List),
	OrderedList = lists:seq(1, length(List)),
	OrderedJobs = lists:zip(OrderedList, AssignedJobs),
	[spawn(fun() -> genserver:request(Client, {job, F, Element, ID}) end) || {ID ,{{_, Client}, Element}} <- OrderedJobs],
	{reply, ok, NewState};

% Receives message from client
handle(St, {message, Result}) ->
	NewState = St#server_st{result = [Result | St#server_st.result]},
	case (length(St#server_st.result) + 1) == St#server_st.length of
		true -> 
			St#server_st.pid ! final(NewState, return);
		false ->
			{reply, ok, NewState}
	end.

% Returns desired output
final(St, return) ->
	OrderedList = lists:keysort(1, St#server_st.result),
	FinalList = [X || {_, X} <- OrderedList],
	{reply, FinalList, St}.

% Assign tasks to every client
assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  	[  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
  	|| {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].


