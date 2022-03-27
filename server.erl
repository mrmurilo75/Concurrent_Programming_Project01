-module(server).
-export([start/0, server/0, send_request/2]).

%% why does this not work ? :
%% .. spawn(server, sum, [{ From, Caller, Pol1, Pol2 }]) ..
%% sum({ From, Caller, Pol1, Pol2 }) -> ..

%% Polynomial format: List of pairs { Coef, Factors }, where
%% "Factors" is a list of variable names with their respective power.
%% For example, [{ 2, [{ x, 2 }, { y, 1 }] }] represents
%% 2 * x^2 * y^1.

%% Example of how to execute:
%% erl
%% c(server).
%% Pid = server:start().
%% server:send_request(Pid, { sum, { [{ 2, 3 }], [{ 1, 3 }] } }).

start() -> spawn(?MODULE, server, []).

server() ->
    receive
        { From, { sum, { Poly1, Poly2 } } } ->
            From ! { success, sum(Poly1, Poly2) };
        { From, { subtract, { Poly1, Poly2 } } } ->
            From ! { success, diff(Poly1, Poly2) };
        { From, { multiply, { Pol1, Pol2 } } } ->
            From ! { error, not_implemented };
        { From, _ } ->
            From ! { error, bad_request };
        _ -> ok %% idk, ignore I guess
    end, server().

send_request(Server, Request) ->
    Server ! { self(), Request },
    receive Response -> Response end.

find_and_remove(Elem, [Head | List], IsEqual) ->
    case IsEqual(Elem, Head) of
        true -> { Head, List };
        _ -> case find_and_remove(Elem, List, IsEqual) of
                 { Something, Rest } -> { Something, [Head | Rest] }
             end
    end;

find_and_remove(_, [], _) ->
    { false, [] }.

%% Check if the factors are the same, for example, x^2 * y =:= y * x^2
%% Missing implementation...
lists_are_equal(_, _) -> true.

%% Don't forget to account for repeated factors
%% (x^2 * x^2, for example), should be done when multiplying,
%% probably.
reduce_poly([Head = { Coef1, Factors } | Poly]) ->
    case find_and_remove(
           Head, Poly, fun (X, Y) -> lists_are_equal(X, Y) end
          ) of
        { false, _ } ->
            [Head | reduce_poly(Poly)];
        { { Coef2, _ }, Rest } ->
            reduce_poly([{ Coef1 + Coef2, Factors } | Rest])
    end;

reduce_poly(_) -> [].

sum(Left, Right) ->
    reduce_poly(lists:append(Left, Right)).

diff(Left, Right) ->
    reduce_poly(
      lists:append(
        Left,
        lists:map(fun ({ Coef, Something }) ->
                          { -Coef, Something } end, Right)
       )).
