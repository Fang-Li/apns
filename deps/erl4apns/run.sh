#!/bin/sh
erl -name apns_test@localhost +K true +P 1500000 -sasl errlog_type error -boot start_sasl -config elog.config -smp auto -env ERL_MAX_PORTS 200000 -pa ebin -pa deps/*/ebin -s crypto -s inets -s ssl -s apns -config priv/app.config -setcookie CQYWMBXMSIDNHOFNVNNY 
#erl  -name apns@10.232.19.3 +K true +P 1500000 -noshell -sasl errlog_type error -boot start_sasl -config elog.config -smp auto -env ERL_MAX_PORTS 200000 -pa ebin -pa deps/*/ebin -s crypto -s inets -s ssl -s apns -config priv/app.config -setcookie MQYWMBXMSIDNHOFNVNNY & > log/console.log
