-module(server).
-export([start/0, server/0, send_request/2]).


%% Polynomial format: List of pairs { Coef, Factors }, where
%% "Factors" is a list of variable names with their respective power.
%% For example, [{ 2, [{ x, 2 }, { y, 1 }] }] represents
%% 2 * x^2 * y^1.

%% Example of how to execute:
%% erl
%% c(server).
%% Pid = server:start().
%% server:send_request(Pid, { sum, { [{ 2, [{ x, 2 }] }], [{ 1, [{ x, 2 }] }] } }).

start() -> spawn(?MODULE, server, []).

%% Should reduce factors before passing polynomials to sum and diff,
%% 'cause otherwise some factors may not be reduced.
server() ->
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
    end, server().

send_request(Server, Request) ->
    Server ! { self(), Request },
    receive Response -> Response end.
