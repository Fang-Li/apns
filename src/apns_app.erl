-module(apns_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    log4erl:conf("priv/log4e.conf"),
    manager_tool_sup:start(),
    translate:start(),
    apns_sender_sup:start_link().
   % lt_apns_sup:start_link().

stop(_State) ->
    ok.
