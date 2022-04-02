-module(client).
-export([request_sum/2, request_sum/3, request_subtract/2, request_subtract/3, request_multiply/2, request_multiply/3]).


%% Polynomial format: List of pairs { Coef, Factors }, where
%% "Factors" is a list of variable names with their respective power.
%% For example, [{ 2, [{ x, 2 }, { y, 1 }] }] represents
%% 2 * x^2 * y^1.

%% Example of how to execute:
%% erl
%% c(client).
%% client:send_request(Host, { sum, { [{ 2, [{ x, 2 }] }], [{ 1, [{ x, 2 }] }] } }).

send_request(Host, Req) ->
	{ok, Socket} =
		gen_tcp:connect(Host , 2345,
				[binary, {packet, 4}]),
	ok = gen_tcp:send(Socket, term_to_binary(Req)),
	receive
		{tcp,Socket,Bin} ->
			gen_tcp:close(Socket),
			binary_to_term(Bin)
	end.

request_sum(Poly1, Poly2) ->
	send_request("localhost", {sum, {Poly1, Poly2}}).
request_sum(host, Poly1, Poly2) ->
	send_request(host, {sum, {Poly1, Poly2}}).

request_subtract(Poly1, Poly2) ->
	send_request("localhost", {subtract, {Poly1, Poly2}}).
request_subtract(host, Poly1, Poly2) ->
	send_request(host, {subtract, {Poly1, Poly2}}).

request_multiply(Poly1, Poly2) ->
	send_request("localhost", {multiply, {Poly1, Poly2}}).
request_multiply(host, Poly1, Poly2) ->
	send_request(host, {multiply, {Poly1, Poly2}}).

