-module(server).
-export([server/0]).

server() -> 
	receive
		{From, sum, {Pol1, Pol2} } -> 
			From ! {error, not_implemented},
			server();
		{From, subtract, {Pol1, Pol2} } -> 
			From ! {error, not_implemented},
			server();
		{From, multiply, {Pol1, Pol2} } -> 
			From ! {error, not_implemented},
			server();
		{From, _, _, _} -> 
			From ! {error, bad_request},
			server();
		{From, _, _} -> 
			From ! {error, bad_request},
			server();
		_ -> 
			server() % idk, ignore i guess
	end.



