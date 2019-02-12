-module(push_msg_manager).  
-behavior(gen_server).  
-export([start/0,stop/0,
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).  
  

	-define(CHECKTIME,60*1000).
%%====================================================================  
%% server interface function  
%%====================================================================  
start()->  
    gen_server:start_link({local,?MODULE}, ?MODULE,[],[]).  
  
stop()->  
    gen_server:call(?MODULE,stop).  

%%====================================================================  
%% gen_server callbacks  
%%====================================================================  
init([]) ->  
	Init=ets:new(?MODULE,[set,named_table,public]),  
	io:format("init ~p ~p~n",[Init,?MODULE]),  
	erlang:send_after(?CHECKTIME, ?MODULE, check_push),
	{ok, Init}.  

handle_call({register_msg,{PushId,PushData}},_From, State)->
  %%TODO 
    TimeInfo = [],
	ets:insert(?MODULE,{PushId, PushData,TimeInfo}),
	Reply = ok, 
	{reply,Reply,State};

handle_call({push_msg,PushId}, _From,  State)->
	Reply = 
	case ets:lookup(?MODULE, PushId) of
		[]->
			not_found;
		{PushId, PushData, TimeInfo}->
			%%TODO push msg
			ok;
		_->
			please_re_register
	end,
	{reply, Reply, State}.
  
handle_cast(_Msg, State) ->  
	{noreply, State}.  
  
handle_info(check_push, State) ->  
	case ets:tab2list(?MODULE) of
		[]->
			skip;
		PushMsg when is_list(PushMsg) ->
			%%TODO check_push_msg(PushMsg)
			[];
		_->
			skip
	end,
	erlang:send_after(?CHECKTIME, ?MODULE, check_push),
	{noreply, State}.  
  
terminate(_Reason, _State) ->  
	io:format("is terminate.",[]),  
	ok.  
  
code_change(_OldVsn, State, _Extra) ->  
	{ok, State}.  
