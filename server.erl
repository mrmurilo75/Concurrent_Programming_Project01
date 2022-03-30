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
        { From, { sum, { Poly1, Poly2 } } } ->
            From ! { success, sum(Poly1, Poly2) };
        { From, { subtract, { Poly1, Poly2 } } } ->
            From ! { success, diff(Poly1, Poly2) };
        { From, { multiply, { Poly1, Poly2 } } } ->
            From ! { success, mul(Poly1, Poly2) };
        { From, _ } ->
            From ! { error, bad_request };
        _ -> ok %% idk, ignore I guess
    end, server().

send_request(Server, Request) ->
    Server ! { self(), Request },
    receive Response -> Response end.

% put it in a single polynomial and reduce (add terms with equal factors)
sum(Left, Right) ->
    combine_terms(lists:append(Left, Right)).

% same as sum, but flip coefs
diff(Left, Right) ->
    combine_terms(
      lists:append(
        Left,
        lists:map(fun ({ Coef, Something }) ->
                          { -Coef, Something } end, Right)
       )).

mul(Left, Right) ->
    combine_terms(distribute(Left, Right)).

% TODO (we can implement this to accept unordered input and do faster sum)
% implementing sort for factor list (comparing then will be trivial)
% sort the polynomial by factor
% reduce equal factors

% TODO multiplication
% highly paralelable
% after sorting as above
% spawn new node for each term of Pol1:
% 	multiply coefs
% 	gets this factor, add it to the list, then sort/reduce factors
% 	repeat for each term of Pol2
% add all results

% add coef of terms with equal Factors ( ex.: [{x,2}, {y,3}] )
combine_terms([Head = { Coef1, Factors } | Poly]) ->
    case find_and_remove(
           Head,
           Poly,
           fun ({ _, X }, { _, Y }) -> have_same_factors(X, Y) end)
    of
        { false, _ } ->
            [Head | combine_terms(Poly)];
        { { Coef2, _ }, Rest } ->
            combine_terms([{ Coef1 + Coef2, Factors } | Rest])
    end;

combine_terms(_) -> [].


find_and_remove(Elem, [Head | List], IsEqual) ->
    case IsEqual(Elem, Head) of
        true -> { Head, List };
        _ -> case find_and_remove(Elem, List, IsEqual) of % cant we just : -> find_and_remove() end 
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

distribute([{ Coef1, Factors1 } | Left], Right) ->
    lists:map(
      fun ({ Coef2, Factors2 }) ->
              { Coef1 * Coef2, combine_factors(
                                 lists:append(Factors1, Factors2)
                                ) } end, Right) ++ distribute(Left, Right);

distribute([], _) -> [].


