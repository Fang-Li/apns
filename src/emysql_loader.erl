%% @author Administrator
%% @doc @todo Add description to emysql_loader.


-module(emysql_loader).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("emysql/include/emysql.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,add_pool/0,do_sql/1,spawn_sql/1]).

-spec start_link()->_R.
start_link()->
	application:start(crypto),
	application:start(emysql),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
-spec add_pool()->_R.
add_pool() ->
	gen_server:call(?MODULE, add_pool, infinity).

-spec do_sql(Sql::string())->_R.
do_sql(Sql)->
%% 	Sql = io_lib:format("SELECT devicetoken FROM pre_common_member WHERE uid = ~s", ["1"]),
	Pool = get_pool(),
	case (catch emysql:execute(Pool#pool.pool_id, list_to_binary(Sql))) of
			Result when is_record(Result, result_packet) ->
				Result#result_packet.rows;
			{'EXIT',connection_lock_timeout} ->
				ok;
			{'EXIT',mysql_timeout} ->
				ok;
			Err ->
				log4erl:info("error: ~p~n", [Err])
		end.
-spec spawn_sql(Sql::string())->_R.
spawn_sql(Sql)->
	spawn(fun() ->
		Pool = get_pool(),
		case (catch emysql:execute(Pool#pool.pool_id, list_to_binary(Sql))) of
			Result when is_record(Result, result_packet) ->
				ok;
			{'EXIT',connection_lock_timeout} ->
				ok;
			{'EXIT',mysql_timeout} ->
				ok;
			Err ->
				log4erl:info("error: ~p~n", [Err])
		end
	end).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(add_pool, _From, State) ->
	NewPoolId = 
		case lists:reverse(lists:usort([PoolId || #pool{pool_id=PoolId} <- emysql_conn_mgr:pools()])) of
			[] ->
				apns1;
			[Last|_] ->
				"apns" ++ Rest = atom_to_list(Last),
				list_to_atom(lists:concat(["apns", list_to_integer(Rest)+1]))
		end,
	{ok, User} = application:get_env(emysql, username),
	{ok, Password} = application:get_env(emysql, password),
	{ok, Host} = application:get_env(emysql, host),
	{ok, Port} = application:get_env(emysql, port),
	{ok, Database} = application:get_env(emysql, database),
	{ok, Encoding} = application:get_env(emysql, encoding),
	{ok,Size} = application:get_env(emysql, pool_size),
	log4erl:info("mysql config~p", [{NewPoolId, Size, User, Password, Host, Port, Database, Encoding}]),
	Res = emysql:add_pool(NewPoolId, Size, User, Password, Host, Port, Database, Encoding),
	{reply, Res, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
get_pool() ->
	case emysql_conn_mgr:pools() of
		[] -> undefined;
		[Pool|_] -> Pool
	end.

