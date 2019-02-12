-module(apns_pool).

-behaviour(gen_server).

-include_lib("erl4apns/include/apns.hrl").

%% API
-export([start_link/1,checkout/1,checkin/2,free_conn/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pool::term(),apns_size::term()}).

%% 清理连接的时间间隔
-define(CleanTime,1000*30*1).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(_)->_.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%% 获取一个连接
-spec checkout(_)->_.
checkout(PoolName)->
	gen_server:call(PoolName,checkout,15000).

%% 归还一个连接
-spec checkin(_,_)->_.
checkin(PoolName,Conn) ->
	gen_server:cast(PoolName,{checkin,Conn}).

%% 异常链接重连
-spec free_conn(_,_)->_.
free_conn(PoolName,Conn)->
	gen_server:cast(PoolName, {free,Conn}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% [{host,"192.168.2.12"},{port,6379},{size,10}]
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
%% 	Conf = dict:from_list(),
%% 	{ok,APNS_CERT} = dict:find(apns_cert,Conf),
%% 	{ok,APNS_KEY} = dict:find(apns_key,Conf),
%% 	{ok,APNS_SIZE} = dict:find(apns_size,Conf),
%% 	{ok,APNS_HOST} = dict:find(apns_host,Conf),
%% 	{ok,APNS_FHOST} = dict:find(apns_fhost,Conf),
%% 	{ok,APNS_PWD} = dict:find(apns_pwd,Conf),
	APNS_SIZE = 5,
	Pools = build_pool(APNS_SIZE,queue:new()),									
    { ok , #state{pool = Pools,apns_size = APNS_SIZE}}.
    %{ ok , #state{pool = Pools,apns_size = APNS_SIZE} , ?CleanTime }.
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
handle_call(checkout, _From, #state{pool = Pool}=State) ->
	log4erl:info("apns:checkout~p~n", [Pool]),
	case queue:out(Pool) of 
		{empty,_} ->
		  case new_conn("new") of
		    {ok,Conn}->
			Pool3 = queue:in(Conn,Pool),
			{reply,{ok,Conn},State#state{pool=Pool3}};
		    Error->
			{reply,Error,State}
		  end; 
		{{value,Conn},Pool2} ->
			Pool3 = queue:in(Conn,Pool2),
    		{reply, {ok,Conn}, State#state{pool=Pool3}} 
	end.

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
handle_cast({checkin,Conn}, #state{apns_size=Size,pool=Pool}=State) ->
	Pool2 = case queue:len(Pool) >= Size of 
		true ->
			close_conn(Conn),
			Pool;
		false ->
			log4erl:info("checkin~p", [{Conn,Pool}]),
			queue:in(Conn,Pool)
	end,	
    {noreply,State#state{pool=Pool2}};

handle_cast({free,Conn},#state{pool = Pool,apns_size = Apns_size} = State)->
	PoolN = 
	queue:filter(fun(C)->
							if
								Conn =:= C ->
									false;
								true->
									true
							end
						end, Pool),
%% 	close_conn(Conn),
 	NewPool = 
 	case queue:len(PoolN) < Apns_size of 
 		true->
			try
 			  {ok,Nconn} = new_conn("reconnectnew"),
 			  queue:in(Nconn,PoolN)
			catch
                          _:_->
			   PoolN
			end;
 		_->
 			PoolN
 	end,
	{noreply,State#state{pool=NewPool}};
	
	
	

handle_cast(stop, State) -> 
	{stop,normal,State}.

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
handle_info(timeout,#state{pool=Pool}=State) ->
    try
	case net_adm:ping('alarm@10.232.19.3') of
	  pong->
		log4erl:warn("alarm heat_beat !!!~n",[]),
		rpc:cast('alarm@10.232.19.3',alarm_apns,heat_beat,[]);
	  pang->
		log4erl:warn("alarm cant connect !!!~n",[])
	end
    catch
	E:R->
	 log4erl:error("alarm heatbeat error !!!~p",[{E,R}])
    end,
    {noreply, State#state{pool=Pool}, ?CleanTime};
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
terminate(_Reason, _State) -> ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_pool(N,Q) when N > 0 ->
	try
		{ok,Conn} = new_conn(N),
		build_pool(N-1,queue:in(Conn,Q))
	catch
		E:R->
			log4erl:error("new_conn:fail.N:~p",[{N,E,R}]),
			build_pool(N-1,Q)
	end;
build_pool(0,Q) ->
	Q.

%% TODO 创建连接 
new_conn(Nindex) ->
%% 	?WARNING_MSG("apns:connect---~p",[Nindex]),
	Result = erl4apns:connect(fun log_error/2, fun log_feedback/1),
	log4erl:warn("erl4apns:succes~p~n", [{Nindex,Result}]),
	Result.
%% 	apns:connect(#apns_connection{apple_host = APNS_HOST,feedback_host = APNS_FHOST,
%% 										   cert_file = APNS_CERT,key_file = APNS_KEY,cert_password = APNS_PWD,
%% 											error_fun = fun log_error/2,feedback_fun = fun log_feedback/1}).

close_conn(Conn) ->
	log4erl:info("erl4apns:close~p~n", [Conn]),
	erl4apns:disconnect(Conn).

%% TODO 释放连接，如果连接数超过 Size
%% free(Pool,_Size) ->
%% 	Pool3 = 
%% 		case queue:out(Pool) of 
%% 			{empty,_} ->
%% 				try
%% 					case new_conn("newfree1") of
%% 						{ok,Newconn}->
%% 							queue:in(Newconn,Pool);
%% 						_->
%% 							Pool
%% 					end
%% 				catch
%% 					E:R->
%% 						log4erl:error("new_conn:fail.free:~p",[{E,R,Pool}]),
%% 						Pool
%% 				end;
%% 			{{value,Conn},Pool2} ->
%% 				close_conn(Conn),
%% 				try
%% 					case new_conn("newfree2") of
%% 						{ok,Newconn}->
%% 							queue:in(Newconn,Pool2);
%% 						_->
%% 							Pool2
%% 					end
%% 				catch
%% 					E:R->
%% 						log4erl:error("new_conn:fail.free:~p",[{E,R,Pool2}]),
%% 						Pool2
%% 				end
%% 		end,
%% 	Pool3.


log_error(MsgId, Status) ->
  log4erl:info("Error on msg ~p: ~p~n", [MsgId, Status]).

log_feedback(Token) ->
  log4erl:info("Device with token ~p removed the app~n", [Token]).
