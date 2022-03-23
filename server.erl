-module(server).
-export([calculate/0]).

calculate() -> 
	receive
		{From, sum, Pol1, Pol2} -> 
			From ! {error, not_implemented},
			calculate();
		{From, subtract, Pol1, Pol2} -> 
			From ! {error, not_implemented},
			calculate();
		{From, multiply, Pol1, Pol2} -> 
			From ! {error, not_implemented},
			calculate();
		{From, _, _, _} -> 
			From ! {error, not_implemented},
			calculate()
	end.

