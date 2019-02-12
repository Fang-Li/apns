-module(apns).
-compile(export_all).

start()->
    _ = application:start(ssl),
    _ = application:start(public_key),
    _ = application:start(crypto),
    _ = application:start(log4erl),
    _ = application:start(erl4apns),
    _ = application:start(apns).


