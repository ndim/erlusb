%%%-------------------------------------------------------------------
%%% File    : erlusb_sup.erl
%%% Author  : Hans Ulrich Niedermann <hun@n-dimensional.de>
%%% Description : 
%%%
%%% Created : 18 Jan 2010 by Hans Ulrich Niedermann <hun@n-dimensional.de>
%%%-------------------------------------------------------------------
-module(erlusb_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Child = {erlusb,{erlusb,start_link,[]},
	     permanent,2000,worker,[erlusb]},
    {ok,{{one_for_all,1,1}, [Child]}}.

%%====================================================================
%% Internal functions
%%====================================================================
