%% @author Dmitry Chernyak <losthost@narod.ru>
%% @copyright Copyleft 2009 

%% @doc Config database
%%
%% Uses Mnesia storage.

-module(enge2_conf).

-export([init/0, option/1, option/2]).

-define(CONFIG_PATH, "conf/enge2.cfg").
-define(CONFIG_LOCAL, "conf/enge2.local").

-record(config, {option, value}).

%
% interface
%

%% @spec init() -> ok
%% @doc read config file
init() ->
    mnesia:delete_table(config),
    mnesia:create_table(config, [{attributes, record_info(fields, config)},
                                 {disc_copies, [node()]}]),
    read_config(?CONFIG_PATH),
    read_config(?CONFIG_LOCAL).

read_config(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            DbData = lists:map(fun({K, V}) -> #config{option = K, value = V} end,
                               Terms),
            F = fun() ->
                    lists:foreach(fun mnesia:write/1, DbData)
                end,
            tr(F);
        _ -> ok
    end.

%% @spec option(option()) -> value()
option(Option) ->
    F = fun() ->
        case mnesia:read({config, Option}) of
            [#config{value = V}] -> V;
            [] -> undefined
        end
    end,
    tr(F).

%% @spec option(option(), value()) -> ok
option(Option, Value) ->
    F = fun() ->
        mnesia:write(#config{option = Option, value = Value})
    end,
    tr(F).

%
% Internals
%

tr(F) ->
    {atomic, Result} = mnesia:transaction(F),
    Result.
