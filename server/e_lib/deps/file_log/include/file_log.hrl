%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 五月 2015 19:29
%%%-------------------------------------------------------------------
-author("yaohong").
-ifndef(_file_log_hrl__).

-define(_file_log_hrl__, true).
-define(FILE_LOG_ERROR(Data, Arg),
	log4erl:error({{self(), ?MODULE, ?LINE}, Data}, Arg)).
-define(FILE_LOG_WARNING(Data, Arg),
	log4erl:warn({{self(), ?MODULE, ?LINE}, Data}, Arg)).
-define(FILE_LOG_INFO(Data, Arg),
	log4erl:info({{self(), ?MODULE, ?LINE}, Data}, Arg)).
-define(FILE_LOG_DEBUG(Data, Arg),
	log4erl:debug({{self(), ?MODULE, ?LINE}, Data}, Arg)).

-define(MAIL_LOG_ERROR(Logger, Data, Arg),
	log4erl:error(Logger, {{self(), ?MODULE, ?LINE}, Data}, Arg)).
-define(MAIL_LOG_WARNING(Logger, Data, Arg),
	log4erl:warn(Logger, {{self(), ?MODULE, ?LINE}, Data}, Arg)).
-define(MAIL_LOG_INFO(Logger, Data, Arg),
	log4erl:info(Logger, {{self(), ?MODULE, ?LINE}, Data}, Arg)).
-define(MAIL_LOG_DEBUG(Logger, Data, Arg),
	log4erl:debug(Logger, {{self(), ?MODULE, ?LINE}, Data}, Arg)).

-endif.