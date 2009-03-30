%% This is an smtp client program sending mail with and without passwords
%% Copyright (C) 2006 Michael Bradford, mike@blackpeas.co.uk
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 2
%% of the License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software Foundation,
%% Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

%% file: email_msg.erl
%% author: Michael Bradford <michael.bradford@t-mobile.uk.net>
%% description: a very simple module that creates a non-multipart
%% email message.
%% Doesn't support MIME or non-ascii characters
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Usage Example
%%
%% 2> c(email_msg).
%% {ok,email_msg}
%% 3> From = "michael.bradford@t-mobile.uk.net".
%% "michael.bradford@t-mobile.uk.net"
%% 4> To = "test@test.co.uk".
%% "test@test.co.uk"
%% 5> Subject = "Testing !!!!".
%% "Testing !!!!"
%% 6> Content = "Hi Mike, this is a test, bye".
%% "Hi Mike, this is a test, bye"
%% 7> Msg = email_msg:simp_msg(From,To,Subject,Content).
%% "from: michael.bradford@t-mobile.uk.net\r\nto: test@test.co.uk\r\n
%% subject: Testi ng !!!!\r\n\r\nHi Mike, this is a test, bye\r\n"
%% 8> 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
-module(email_msg).
-vsn('v1.0').

-export([simp_msg/4]).

simp_msg(From, To, Subject, Message) ->
	FromStr = ["from: ", From, "\r\n"],
	ToStr = ["to: ", To, "\r\n"],
	SubjStr = ["subject: ", Subject, "\r\n"],
	MsgStr = ["\r\n", Message],
	list_to_binary([FromStr, ToStr, SubjStr, MsgStr, "\r\n"]).
