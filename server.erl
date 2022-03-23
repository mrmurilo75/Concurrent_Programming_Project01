-module(server).
-export([server/0]).

% why does this not work ? : 
% .. spawn(server, sum, [{From, Caller, Pol1, Pol2}]) ..
% sum({From, Caller, Pol1, Pol2}) -> ..

sum() -> 
	receive 
		{From, Caller, Pol1, Pol2} -> From ! {calculation_result, Caller, not_implemented}
	end.

server() -> 
	receive
		{From, sum, {Pol1, Pol2} } -> 
			spawn(server, sum, []) ! {self(), From, Pol1, Pol2},
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



