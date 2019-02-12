%% @author Administrator
%% @doc @todo Add description to lt_ios_provider_sup.


-module(manager_tool_sup).
-behaviour(supervisor).
-export([start/0, init/1]).


-define(APNS_POOL, apns_pool).
-define(CHILDWORK(Mod, Fun, Args), {Mod, {Mod, Fun, Args}, permanent, 5000, worker, [Mod]}).
-define(CHILDSUP(Mod, Fun, Args), {Mod, {Mod, Fun, Args}, permanent, infinity, supervisor, [Mod]}).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start() -> _R.
start() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
  {ok, {{one_for_one, 10, 10}, [
    ?CHILDWORK(apns_pool, start_link, [?APNS_POOL]),
    %?CHILDWORK(emysql_loader, start_link, []),
    ?CHILDWORK(push_msg_manager, start, [])
  ]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


