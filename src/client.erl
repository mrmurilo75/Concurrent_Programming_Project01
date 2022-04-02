-module(client).
-export([send_request/1, send_request/2]).


%% Polynomial format: List of pairs { Coef, Factors }, where
%% "Factors" is a list of variable names with their respective power.
%% For example, [{ 2, [{ x, 2 }, { y, 1 }] }] represents
%% 2 * x^2 * y^1.

%% Example of how to execute:
%% erl
%% c(client).
%% client:send_request(Host, { sum, { [{ 2, [{ x, 2 }] }], [{ 1, [{ x, 2 }] }] } }).

send_request(Req) ->
	send_request("localhost", Req).

send_request(Host, Req) ->
	{ok, Socket} =
		gen_tcp:connect(Host , 2345,
				[binary, {packet, 4}]),
	ok = gen_tcp:send(Socket, term_to_binary(Req)),
	receive
		{tcp,Socket,Bin} ->
			%% io:format("Client received binary = ~p~n" ,[Bin]),
			%% Val = binary_to_term(Bin),
			%% io:format("Client result = ~p~n" ,[Val]),

			gen_tcp:close(Socket),
			binary_to_term(Bin)
	end.

