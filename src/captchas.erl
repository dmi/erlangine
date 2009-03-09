-module(captchas).
-export([make_href/3]).

password(Secret, Random, Alphabet, Letters) ->
    Request = [Secret, Random], % [Secret, Random, ":", Alphabet, ":", Letters],
    MD5 = erlang:md5(Request),
    {HeadLetters, _Tail} = split_binary(MD5, Letters),
    AbLength = length(Alphabet),
    %L = lists:map(fun(X) -> X rem AbLength end, binary_to_list(HeadLetters)),
    %io:format("~p - ~p - ~p - ~p - ~p - ~p~n",[Random, Request, MD5, HeadLetters, AbLength, L]),
    lists:map(fun(X) -> lists:nth(X rem AbLength + 1, Alphabet) end,
              binary_to_list(HeadLetters)).

%% @type user() = string(). Account name for captchas.net, or "demo" for testing.
%% @type password() = string(). Account pasword for captchas.net, or "secret" for testing.
%% @type width() = integer(). Captcha code length.
%% @type link() = binary(). Hyperlink to captcha image.
%% @type code() = binary(). Code on captcha image.

%% @spec make_href(user(), password(), width()) -> {link(), code()}
%% @doc make hyperlink to to captcha image, return link and image's code
make_href(User, Password, Width) ->
    Random = session:guid(),
    Pw = password(Password, Random ,"abcdefghijklmnopqrstuvwxyz", 6),
    Link = ["http://image.captchas.net/?client=", User, "&random=", Random, "&width=", io_lib:format("~p", [Width])],
    {list_to_binary(Link), list_to_binary(Pw)}.

test_captchas() ->
    make_href("demo", "secret", 200).
