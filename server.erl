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
%% server:send_request(Pid, { sum, { [{ 2, [{ x, 2 }] }], [{ 1, [{ x, 2 }] }] } }).

start() -> spawn(?MODULE, server, []).

server() ->
    receive
        { From, { sum, { Poly1, Poly2 } } } ->
            From ! { success, sum(Poly1, Poly2) };
        { From, { subtract, { Poly1, Poly2 } } } ->
            From ! { success, diff(Poly1, Poly2) };
        { From, { multiply, { Poly1, Poly2 } } } ->
            From ! { error, mul(Poly1, Poly2) };
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

is_subset([Elem | Left], Right) ->
    lists:member(Elem, Right) and is_subset(Left, Right);

is_subset([], _) -> true.

have_same_factors(Left, Right) ->
    is_subset(Left, Right) and is_subset(Right, Left).

%% Don't forget to account for repeated factors
%% (x^2 * x^2, for example), should be done when multiplying,
%% probably.
combine_terms([Head = { Coef1, Factors } | Poly]) ->
    case find_and_remove(
           Head,
           Poly,
           fun ({ _, X }, { _, Y }) -> have_same_factors(X, Y) end) of
        { false, _ } ->
            [Head | combine_terms(Poly)];
        { { Coef2, _ }, Rest } ->
            combine_terms([{ Coef1 + Coef2, Factors } | Rest])
    end;

combine_terms(_) -> [].

combine_factors([Head = { Var, ThisPower } | Factors]) ->
    case find_and_remove(
           Head,
           Factors,
           fun ({ Left, _ }, { Right, _ }) -> Left =:= Right end) of
        { false, _ } ->
            [Head | combine_factors(Factors)];
        { { _, OtherPower }, Rest } ->
            combine_factors([{ Var, ThisPower + OtherPower } | Rest])
    end;

combine_factors([]) -> [].

sum(Left, Right) ->
    combine_terms(lists:append(Left, Right)).

diff(Left, Right) ->
    combine_terms(
      lists:append(
        Left,
        lists:map(fun ({ Coef, Something }) ->
                          { -Coef, Something } end, Right)
       )).

mul(_, _) ->
    unimplemented.
