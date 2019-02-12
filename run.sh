#!/bin/sh
erl \
-name apns@127.0.0.1 \
+K true +P 1500000 \
-sasl errlog_type error \
-boot start_sasl \
-smp auto \
-env ERL_MAX_PORTS 200000 \
-pa ebin \
-pa deps/*/ebin \
-s crypto \
-s inets \
-s ssl \
-s apns \
-config priv/apns.config \
-setcookie mahjong \

