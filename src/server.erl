-module(server).
-export([start/0]).

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
	{ok, Socket} = gen_tcp:accept(Listen),
	gen_tcp:close(Listen),
	loop(Socket).

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			%% io:format("Server received binary = ~p~n" ,[Bin]),
			Str = binary_to_term(Bin),
			{sum, Req} = Str,
			io:format("Server (unpacked) ~p~n" ,[is_tuple(Req)]),
			Pid = spawn(?MODULE, calculate, []),
			io:format("Pid = ~p~n" ,[Pid]),
			Reply = send_request(Pid, Str),
			%% io:format("Server replying = ~p~n" ,[Reply]),
			gen_tcp:send(Socket, term_to_binary(Reply)),
			loop(Socket);
		{tcp_closed, Socket} ->
			io:format("Server socket closed~n" )
	end.

