%% @author Administrator
%% @doc @todo Add description to apns_sender_sup.


-module(apns_sender_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,start_child/0]).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link()->_.
start_link() ->
    	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child()->_.
start_child()->
	supervisor:start_child(?MODULE,[]).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
	AChild = {apns_sender, {apns_sender,start_link,[]}, temporary, 2000, worker, [apns_sender] },
	{ok,{{simple_one_for_one,5,10}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


