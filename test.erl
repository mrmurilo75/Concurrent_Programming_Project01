-module(test).
-import(server, [start/0, send_request/2]).
-export([test_sum/0]).



% sum test: (2*x^2 + 3x^2)
test_sum() ->
	Pid = server:start(),
	server:send_request(Pid, { sum, {
				     [{ 2, [{ x, 2 }] }],
				     [{ 3, [{ x, 2 }] }] 
				    } }).

% sum test: (4*x^4 + 2*x^2 + 3*x^1) + (3*x^3 + 2*x^1)
%	server:send_request(Pid, { sum, {
%				     [{ 4, [{ x, 4 }] }, 
%				      { 2, [{ x, 2 }] },
%				      { 3, [{ x, 1 }] }],
%				     [{ 3, [{ x, 3 }] },
%				      { 2, [{ x, 1 }] }]
%				    }).
%
