%% @author Administrator
%% @doc @todo Add description to apns_sender.


-module(apns_sender).
-behaviour(gen_server).
-include_lib("erl4apns/include/localized.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(APNS_POOL,apns_pool).

-define(HTTPHead,"application/x-www-form-urlencoded").

-record(apns_push,
{
  msgid=undefined   ::term(),   %%消息id
  from=undefined    ::term(),   %%来源jid
  to=undefined      ::term(),   %%目标jid
  msgtype=undefined ::term(),   %%消息类型
  msgbody=undefined ::term(),   %%消息体
  num=undefined     ::term(),   %%数量
  json=undefined                %%消息二进制体
}).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-export([send/1,start_link/0,start/0]).

-spec start_link()->_R.
start_link()->
  gen_server:start_link(?MODULE, [], []).
-spec start()->_.
start()->
  apns_sender_sup:start_child().



-spec send(Msg::term())->ok.
send(Msg)->
  %log4erl:info("apns_sender:send~p",[{Msg}]),
  {ok,Pid} = start(),
  gen_server:cast(Pid, {send,Msg}).

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
handle_cast({send,MessageTuple},State)->
  try
    #apns_push{to =To,num = Num} = MessageTuple,
    [Toid|_] = string:tokens(To,"@"),
    case deel_apns_msg(MessageTuple) of
      skip->
        skip;
      Msg->
        {ok,HttpServer} = application:get_env(deveice_token_server,http_server),
        ResBody = apns_http_util:request(HttpServer,<<"getdevicetoken">>,[{"username",list_to_binary(Toid)}]),
        DeviceRes = 
          case ResBody of
            {ok,ResObj} -> apns_http_util:get_deviceToken(ResObj);
            {error,Error} -> log4erl:info("request fail:~p~n",[Error]),<<>>
          end,
        
        case DeviceRes of
          {error,_Error} ->
            skip;
          {ok,<<>>} ->
            skip;
          {ok,DeviceToken} ->
            case apns_pool:checkout(?APNS_POOL) of
              {ok,Apnsconn}->
                Res = 
                case Msg of
                  {tup,Amsg,Rmsg}->
                    %% log4erl:error("apns_sender .. ~nMessageTuple=~p",[MessageTuple]),
                    erl4apns:send_message(Apnsconn, binary_to_list(DeviceToken), list_to_binary(Amsg), Num, <<"default">>, 5000, Rmsg);
                  _->
                    SendMsg = 
                      if
                        erlang:byte_size(Msg)>= 160->
                          list_to_binary(string:sub_string(Msg, 0, 160)++"...");
                        true->
                          list_to_binary(Msg)
                      end,
                    erl4apns:send_message(Apnsconn, binary_to_list(DeviceToken), SendMsg, Num, <<"default">>)
                end,
                log4erl:info("apns result:~p~n",[Res]);
              R1->
                log4erl:error("apns_pool:checkout error~p",[R1])
            end;
          _ ->
            skip
        end
    end
  catch
    E:R->
      log4erl:warn("apns error send:~p", [{E,R,erlang:get_stacktrace()}]),
      skip
  end,
  {stop, normal, State};

handle_cast(stop, State) ->
  {stop, normal, State};
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


get_apns_str(Type,Msgtype)->
  Typestr = if
      is_integer(Type)->
        integer_to_list(Type);
      true->
        binary_to_list(Type)
    end,
  Key = "apns_"++Msgtype++"_"++Typestr,
  log4erl:info("apns_key~p", [Key]),
  case  ets:lookup(translations, {"zh", Key}) of
    [{_, Trans}]->
      binary_to_list(Trans);
      %% get_apns_str2(<<"apns_show">>, "all");
    _->
      skip
  end.

%% get_apns_str2(Type,Msgtype) ->
%%   Typestr = if
%%           is_integer(Type)->
%%             integer_to_list(Type);
%%           true->
%%             binary_to_list(Type)
%%         end,
%%   Key = "apns_"++Msgtype++"_"++Typestr,
%%   log4erl:info("apns_key~p", [Key]),
%%   case  ets:lookup(translations, {"zh", Key}) of
%%       [{_, Trans}]->
%%       binary_to_list(Trans);
%%     _->
%%       skip
%%   end.
  
token_format(Cstr,Argstrs)->
    Clist  = string:tokens(Cstr,"~s"),
    do_token_format(Clist,Argstrs,[]).

do_token_format([],_,Res)->
    Res;
do_token_format([HClist|LClist],Argstrs,Res) when is_list(HClist) ->
    NewRes = 
    if
        Argstrs =:= []->
            LArgstrs = [],
            Res ++ lists:append([HClist|LClist]);
        true->
            [HArgstrs|LArgstrs] = Argstrs,
            if
                is_binary(HArgstrs) ->
                    Res ++ HClist ++ binary_to_list(HArgstrs);
                true->
                    Res ++ HClist ++ HArgstrs
            end
    end,
    do_token_format(LClist, LArgstrs, NewRes);
do_token_format(Clist,_,Res)->
    Res ++ Clist.


%% 该方法对deel_apns_msg方法做了兼容处理，当不用推送详情的时候，推送一条默认提示消息
deel_apns_msg(Message) ->
  BStr = deel_apns_msg2(Message),
  case BStr of
    skip ->
      skip;
    _ ->
      #apns_push{msgtype = Msgtype, to = To, msgbody = JO, json = _JsonMsg } = Message,
      %%=======================================
      %% 用户正在直播消息推送 
      %% 20170223 lifang
      %% 由于需求需要，需要放开此处设置，默认推送详情
      %%=======================================
      Apns_Show = ej:get({"apns_show"},JO, <<"1">>), 
      
      [Uid|_T] = string:tokens(To,"@"),
      case {Apns_Show,lists:member(Uid,["1680028","29","419323"])} of 
        {<<"1">>,_} ->
          BStr;
        {<<"0">>,true} ->
          log4erl:error("deel_apns_msg .. ~nMsgtype=~p,~nMessageTuple=~p",[Msgtype,BStr]),
          BStr;
        _ ->
          BStr2 = get_apns_str(<<"apns_show">>, "all"),
          case BStr2 of
            skip ->
              skip;
            _ ->
              io_lib:format(BStr2, [])
          end
      end
  end.
          
      
%%推送消息格式处理
deel_apns_msg2(Message)->
  #apns_push{msgtype = Msgtype, msgbody = JO, json = JsonMsg } = Message,
  Type = ej:get({"type"},JO),
  Mark = ej:get({"mask"},JO,<<"0">>),
  _Apns_Show = ej:get({"apns_show"},JO,<<"0">>),
  
  %% ==============================================================================
  %% 当免打扰的的时候，不推送，这个在aa_offline_mod里面已经限制过一次了，到不了这里
  %% 当非免打扰状态时，且不允许推送详情时，推送默认提示，即"收到消息提示"
  %% 此处有bug，当不满足type要求的时候，是不推送消息的
  %% 所以要分散到三种类型中的各个type里面
  %% Mark =:= <<"0">> andalso Apns_Show =:= <<"0">> ->
  %%   case get_apns_str(<<"apns_show">>, "all") of
  %%     skip ->
  %%       skip;
  %%     BStr ->
  %%       io_lib:format(BStr, [])
  %%   end;
  %% 当正常聊天时，且非免打扰，推送详情，此时默认是允许推送详情，包括apns_show=<<"1">>,或者没有设置默认为1
  %% =====================================================================================================================
  
  if
    Msgtype =:= "normalchat",Mark =:= <<"0">> ->
      if
        Type =:= <<"0">>;Type =:= 0->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          ContentB = ej:get({"content"},JO),
          Content = binary_to_list(ContentB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              %% Name++BStr++Content
              io_lib:format(BStr,[Name,Content])
          end;
        Type =:= <<"1">>;Type =:= 1->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"2">>;Type =:= 2->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"4">>;Type =:= 4->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"5">>;Type =:= 5->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"6">>;Type =:= 6->
          NameB = ej:get({"username"},JO),
          INameB = ej:get({"name"},JO),
          Name = binary_to_list(NameB),
          IName = binary_to_list(INameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name,IName])
          end;
        Type =:= <<"7">>;Type =:= 7->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"8">>;Type =:= 8->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"9">>;Type =:= 9->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"10">>;Type =:= 10->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"11">>;Type =:= 11->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"12">>;Type =:= 12->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, Name)
          end;
        Type =:= <<"13">>;Type =:= 13->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"15">>;Type =:= 15->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"16">>;Type =:= 16->
          NameB = ej:get({"name"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;

        true->
          skip
      end;
    Msgtype =:= "groupchat",Mark =:= <<"0">> ->
      if
        Type =:= <<"0">>;Type =:= 0->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          %% io:format("groupid------------------------> ~p ~n",[rfc4627:get_field(JO, "groupid")]),
          ContentB = ej:get({"content"},JO),
          Content = binary_to_list(ContentB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name,Content])
          end;
        Type =:= <<"1">>;Type =:= 1->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"2">>;Type =:= 2->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"4">>;Type =:= 4->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"5">>;Type =:= 5->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"6">>;Type =:= 6->
          NameB = ej:get({"username"},JO),
          INameB = ej:get({"name"},JO),
          Name = binary_to_list(NameB),
          IName = binary_to_list(INameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name,IName])
          end;
                    Type =:= <<"15">>;Type =:= 15->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"16">>;Type =:= 16->
          NameB = ej:get({"name"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        true->
          skip
      end;
    Msgtype =:= "super_groupchat",Mark =:= <<"0">> ->
      if
        Type =:= <<"1000">>;Type =:= 1000->
          NameB = ej:get({"username"},JO),
          NameGB = ej:get({"groupname"},JO),
          Name = binary_to_list(NameB),
          NameG = binary_to_list(NameGB),
          ContentB = ej:get({"content"},JO),
          Content = binary_to_list(ContentB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name,NameG,Content])
          end;
        Type =:= <<"1001">>;Type =:= 1001->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"1002">>;Type =:= 1002->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"1004">>;Type =:= 1004->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"1005">>;Type =:= 1005->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"1006">>;Type =:= 1006->
          NameB = ej:get({"username"},JO),
          INameB = ej:get({"name"},JO),
          Name = binary_to_list(NameB),
          IName = binary_to_list(INameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name,IName])
          end;
        Type =:= <<"1013">>;Type =:= 1013->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
                  skip->
                    skip;
                  BStr->
                    io_lib:format(BStr,[Name])
          end;
        Type =:= <<"1015">>;Type =:= 1015->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"1016">>;Type =:= 1016->
          NameB = ej:get({"name"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        true->
          skip
      end;
    Msgtype =:= "system",Mark =:= <<"0">> ->
      if
        Type =:= <<"1">>;Type =:= 1 ->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"3">>;Type =:= 3 ->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"4">>;Type =:= 4 ->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"5">>;Type =:= 5 ->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"6">>;Type =:= 6 ->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"7">>;Type =:= 7 ->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"8">>;Type =:= 8 ->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"9">>;Type =:= 9 ->
          %% NameB = ej:get({"username"},JO),
          %% Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[])
          end;
        Type =:= <<"10">>;Type =:= 10 ->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"11">>;Type =:= 11 ->
          NameB = ej:get({"username"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[Name])
          end;
        Type =:= <<"12">>;Type =:= 12 ->
          NameB = ej:get({"invitename"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name])
          end;
        Type =:= <<"13">>;Type =:= 13 ->
          NameB = ej:get({"invitename"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name])
          end;
        Type =:= <<"14">>;Type =:= 14 ->
          NameB = ej:get({"groupname"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name])
          end;
        Type =:= <<"15">>;Type =:= 15 ->
          NameB = ej:get({"groupname"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name])
          end;
        Type =:= <<"21">>;Type =:= 21 ->
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[])
          end;
        Type =:= <<"101">>;Type =:= 101 ->
          NameB = ej:get({"groupname"},JO),
          Invitednameb = ej:get({"invitename"},JO),
          Name = binary_to_list(NameB),
          Invitedname = binary_to_list(Invitednameb),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Invitedname,Name])
          end;
        Type =:= <<"102">>;Type =:= 102 ->
          NameB = ej:get({"groupname"},JO),
          Beinvitednameb = ej:get({"beinvitedname"},JO),
          Name = binary_to_list(NameB),
          Beinvitedname = binary_to_list(Beinvitednameb),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Beinvitedname,Name])
          end;
        Type =:= <<"103">>;Type =:= 103 ->
          NameB = ej:get({"groupname"},JO),
          Beinvitednameb = ej:get({"beinvitedname"},JO),
          Name = binary_to_list(NameB),
          Beinvitedname = binary_to_list(Beinvitednameb),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Beinvitedname,Name])
          end;
        Type =:= <<"104">>;Type =:= 104 ->
          NameB = ej:get({"groupname"},JO),
          InvitenameB = ej:get({"invitename"},JO),
          Name = binary_to_list(NameB),
          Invitename = binary_to_list(InvitenameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Invitename,Name])
          end;
        Type =:= <<"105">>;Type =:= 105 ->
          NameB = ej:get({"groupname"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name])
          end;
        Type =:= <<"106">>;Type =:= 106 ->
          NameB = ej:get({"groupname"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name])
          end;
        Type =:= <<"113">>;Type =:= 113 ->
          NameB = ej:get({"groupname"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name])
          end;
        Type =:= <<"114">>;Type =:= 114 ->
          NameB = ej:get({"groupname"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name])
          end;
        Type =:= <<"115">>;Type =:= 115 ->
          NameB = ej:get({"groupname"},JO),
          LefttimesB = ej:get({"lefttimes"},JO),
          Name = binary_to_list(NameB),
          Lefttimes = binary_to_list(LefttimesB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name,Lefttimes])
          end;
        Type =:= <<"116">>;Type =:= 116 ->
          NameB = ej:get({"groupname"},JO),
          LefttimesB = ej:get({"lefttimes"},JO),
          Name = binary_to_list(NameB),
          Lefttimes = binary_to_list(LefttimesB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name,Lefttimes])
          end;
        Type =:= <<"117">>;Type =:= 117 ->
          NameB = ej:get({"groupname"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name])
          end;
        Type =:= <<"118">>;Type =:= 118 ->
          %% NameB = ej:get({"groupname"},JO),
          %% Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [])
          end;
        Type =:= <<"119">>;Type =:= 119 ->
          NameB = ej:get({"groupname"},JO),
          Name = binary_to_list(NameB),
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr, [Name])
          end;
        Type =:= <<"120">>;Type =:= 120 ->
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[])
          end;
        Type =:= <<"121">>;Type =:= 121 ->
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[])
          end;
        Type =:= <<"122">>;Type =:= 122 ->
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[])
          end;
        Type =:= <<"123">>;Type =:= 123 ->
          case get_apns_str(Type,Msgtype) of
            skip->
              skip;
            BStr->
              io_lib:format(BStr,[])
          end;
          
        %%=======================================
        %% 用户正在直播消息推送 
        %% 20170223 lifang
        %%=======================================
        
        Type =:= <<"130">>;Type =:= 130 ->
          JMap  = jiffy:decode(JsonMsg,[return_maps]),
          RoomNumB = maps:get(<<"number">>, JMap),
          _Anchorname = maps:get(<<"username">>, JMap),
          LiveTitle = maps:get(<<"title">>, JMap),
          Liveuname = maps:get(<<"name">>, JMap),
           case get_apns_str(Type,Msgtype) of
             skip->
               skip;
             BStr->
               %% Content  = token_format(BStr,[binary_to_list(Liveuname),binary_to_list(LiveTitle)]),
               Content  = lists:foldl(fun(X,Acc)-> re:replace(Acc,"~s",X, [{return, list}]) end,BStr,[binary_to_list(Liveuname),binary_to_list(LiveTitle)]),
               Extra = [{<<"roomnum">>,RoomNumB}],
               {tup,Content,Extra}
           end;

        true->
          skip
      end;
    true->
      skip
  end.




