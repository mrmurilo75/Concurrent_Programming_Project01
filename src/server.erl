-module(server).
-export([start/0, calculate/0]).

%% Example of how to execute:
%% erl
%% c(server).
%% Pid = server:start().

calculate() ->
    receive
        { From, { Operation, { Poly1, Poly2 } } } ->
            NormPoly1 = poly:normalize(Poly1),
            NormPoly2 = poly:normalize(Poly2),
            From ! (
              case Operation of
                  sum -> { success, poly:add(NormPoly1, NormPoly2) };
                  subtract -> { success, poly:subtract(NormPoly1, NormPoly2) };
                  multiply -> { success, poly:multiply(NormPoly1, NormPoly2) };
                  _ -> { error, invalid_operation }
              end
             );
        { From, _ } ->
            From ! { error, bad_request };
        _ -> ok %% idk, ignore I guess
    end, calculate().

send_request(Server, Request) ->
    Server ! { self(), Request },
    receive Response -> Response end.

start() ->
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
						{reuseaddr, true},
						{active, true}]),
	Pid = spawn(?MODULE, calculate, []),
	sequential(Listen, Pid).

sequential(Listen, Pid) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	loop(Socket, Pid),
	sequential(Listen, Pid).

loop(Socket, Pid) ->
	receive
		{tcp, Socket, Bin} ->
			Str = binary_to_term(Bin),
			Reply = send_request(Pid, Str),
			gen_tcp:send(Socket, term_to_binary(Reply)),
			loop(Socket, Pid);
		{tcp_closed, Socket} ->
			ok
	end.

