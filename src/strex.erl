%% @author Dmitry Chernyak <losthost@narod.ru>
%% @copyright Copyleft 2009 

%% @doc String parsing support library

-module(strex).
-export([split_str/2, extract_meta/3, parse/2]).

%% @spec split_str(string(), string()) -> {Left::string(), Right::string()} | string()
%% @doc Split Str::string() by Delim to Left and Right parts
split_str(Delim, Str) ->
    N = string:str(Str, Delim),
    if
        N > 0 ->
            {Left, Right} = lists:split(N - 1, Str),
            Right1 = lists:nthtail(length(Delim), Right),
            {Left, Right1};
        true -> Str
    end.

%% @spec extract_meta(string(), string(), string()) -> {Meta::string(), Before::string(), After::string()} | {none, Str::string(), []}
%% @doc Extract one string Meta between Start and End tokens.
%%
%%      Return Meta and the two parts of Str around Meta (delimiters are excluded).
%%      <br/>Function is not greedy.
extract_meta(Start, End, Str) ->
    
    case split_str(Start, Str) of
        {BeforeMeta, Rest} ->
            {Meta, AfterMeta} = split_str(End, Rest),
            {Meta, BeforeMeta, AfterMeta};
        _ ->
            {none, Str, ""}
    end.

%% @spec parse(string(), string()) -> list()
%% @doc Split Str::string() by Delim::string(), return the list of parts
parse(Delim, Str) ->
    parse(Delim, Str, []).

parse(Delim, Str, Acc) ->
    N = string:str(Str, Delim),
    if
        N > 0 ->
            parse(Delim, lists:nthtail(N + length(Delim) - 1, Str), [lists:sublist(Str, N - 1) | Acc]);
        true ->
            lists:reverse([Str | Acc])
    end.

