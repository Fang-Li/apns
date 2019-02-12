#PROJECT=apns
#
#CONFIG?=priv/app.config
#
#DEPS = jiffy sync katana eper log4erl mochiweb emysql
#dep_jiffy = git https://github.com/davisp/jiffy 0.13.3
#dep_sync = git https://github.com/inaka/sync.git 0.1
#dep_katana =  git https://github.com/inaka/erlang-katana 0.2.0
#dep_eper = git https://github.com/massemanet/eper.git 0.90.0
#dep_log4erl = git git://github.com/ahmednawras/log4erl.git d6a14cc1f4dbc21cf9c30753bfccd96a02c84cf4
#dep_mochiweb = git https://github.com/mochi/mochiweb.git 87d6eb15490729698fd192228b8fc963b3777c51
#dep_emysql = git git@github.com:Eonblast/Emysql.git c7e2103f8b737667f0128802e8de2f0d5ed2fa5c
#
#ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
#ERLC_OPTS += +warn_export_vars +warn_exported_vars +debug_info

# Commont Test Config

REBAR = $(shell pwd)/rebar

all: deps compile 

compile:
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	rm -f ./deps/*/ebin/*.beam
	@./rebar clean
