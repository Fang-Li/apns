-module(apns_http_util).
-compile(export_all).

-define(HTTP_HEAD,"application/x-www-form-urlencoded").

request(Address,Interface,Params)->
  {M,S,SS} = now(),
  SN = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
  ParamObj = {obj,[ {"sn",list_to_binary(SN)}, {"service",<<"ejabberd">>}, {"method",Interface},{"params",{obj,Params}}]}, 
  ReqBody = "body="++rfc4627:encode(ParamObj),
  case httpc:request(post,{ Address ,[], ?HTTP_HEAD , ReqBody },[],[] ) of
    {ok, {_,_,ResBody}} ->
      ResBodyObject = rfc4627:decode(ResBody),
      case get_body(ResBodyObject) of
        {ok,SucessBody} ->
          {ok,SucessBody};
        {error,Error} ->
          log4erl:error("request: ~p##~p##~p##~p",[SN,Address,Interface,Params]),
          log4erl:error("[http response format error sn=~p ; response=~p  ; error=~s",[SN,ResBody,Error]),
          {error,Error}
      end;
    {error, Reason} ->
       log4erl:error("request error: ~p##~p##~p##~p",[SN,Address,Interface,Params]),
       log4erl:error("response: error reason sn=~p ; response=~p",[SN,Reason]),
      {error,Reason};
    _ ->
      {error, unkown}
  end.
  
get_body(Res) ->
  case Res of
    {ok,Obj,_Re} -> 
      case rfc4627:get_field(Obj,"success") of
        {ok,true} ->
          {ok,Obj};
        _ ->
          case rfc4627:get_field(Obj,"entity") of
            {ok,Reason} ->
              {error,Reason};
            _ ->
              {error,error_json}
          end
      end;
    Error -> 
      {error,Error}
  end.

  
  
get_deviceToken(BodyObj) ->
  case rfc4627:get_field(BodyObj,"devicetoken") of
    {ok,DeviceToken} ->
      case DeviceToken of
        [] ->
          {error,devicetoken_is_null};
        _ ->
          {ok,DeviceToken}
      end;
    _ ->
      {error,devicetoken_key_inexistence}
  end.
