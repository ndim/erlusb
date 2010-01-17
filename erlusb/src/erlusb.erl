%%%-------------------------------------------------------------------
%%% File    : erlusb.erl
%%% Author  : Hans Ulrich Niedermann <hun@n-dimensional.de>
%%% Description : 
%%%
%%% Created : 18 Jan 2010 by Hans Ulrich Niedermann <hun@n-dimensional.de>
%%%-------------------------------------------------------------------
-module(erlusb).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Service API
-export([get_device_list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("erlusb_internal.hrl").

-record(state, {manager_port}).


%%====================================================================
%% Service API
%%====================================================================

get_device_list() ->
    gen_server:call(?MODULE, get_device_list).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Prog = "erlusb-manager",
    PrivDir = code:priv_dir(?APP_NAME),
    ExtProg = filename:join(PrivDir, Prog),
    ManagerPort = open_port({spawn, ExtProg},
			    [
			     {cd, PrivDir}, % is there a better dir to run in?
			     %% {env, [{'PATH'...}]},
			     use_stdio,
			     {packet, 2},
			     hide,
			     binary
			    ]),
    {ok, #state{manager_port=ManagerPort}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(get_device_list, _From, State) ->
    #state{manager_port=ManagerPort} = State,
    ManagerPort ! {self(), {command, term_to_binary({get_device_list})}},
    receive
	{ManagerPort, {data, Data}} ->
	    {reply, {ok, binary_to_term(Data)}, State};
	{'EXIT', ManagerPort, Reason} ->
	    {stop, {port_terminated, Reason}, State}
    after 2000 ->
	{stop, timeout_waiting_for_port, State}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
