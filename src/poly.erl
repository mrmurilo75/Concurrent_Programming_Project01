-module(poly).
-export([add/2, subtract/2, multiply/2, normalize/1, print/1]).

% put it in a single polynomial and reduce (add terms with equal factors)
add(Left, Right) ->
    combine_terms(lists:append(Left, Right)).

% same as sum, but flip coefs
subtract(Left, Right) ->
    combine_terms(
      lists:append(
        Left,
        lists:map(fun ({ Coef, Something }) ->
                          { -Coef, Something } end, Right)
       )).

multiply(Left, Right) ->
    combine_terms(distribute(Left, Right)).

normalize(Poly) ->
    combine_terms(
      lists:map(
        fun ({ Coef, Factors }) -> { Coef, combine_factors(Factors) } end, Poly
       )
     ).

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
    if
        Coef1 =:= 0 -> combine_terms(Poly);
        true ->
            case find_and_remove(
                   Head,
                   Poly,
                   fun ({ _, X }, { _, Y }) -> have_same_factors(X, Y) end)
            of
                { false, _ } ->
                    [Head | combine_terms(Poly)];
                { { Coef2, _ }, Rest } ->
                    combine_terms([{ Coef1 + Coef2, Factors } | Rest])
            end
    end;

combine_terms(_) -> [].



find_and_remove(Elem, [Head | List], IsEqual) ->
    case IsEqual(Elem, Head) of
        true -> { Head, List };
        _ -> { Something, Rest } = find_and_remove(Elem, List, IsEqual),
             { Something, [Head | Rest] }
    end;

find_and_remove(_, [], _) ->
    { false, [] }.



is_subset([Elem | Left], Right) ->
    lists:member(Elem, Right) and is_subset(Left, Right);

is_subset([], _) -> true.



have_same_factors(Left, Right) ->
    is_subset(Left, Right) and is_subset(Right, Left).



combine_factors([Head = { Var, ThisPower } | Factors]) ->
    if
        ThisPower =:= 0 -> combine_factors(Factors);
        true ->
            case find_and_remove(
                   Head,
                   Factors,
                   fun ({ Left, _ }, { Right, _ }) -> Left =:= Right end) of
                { false, _ } ->
                    [Head | combine_factors(Factors)];
                { { _, OtherPower }, Rest } ->
                    combine_factors([{ Var, ThisPower + OtherPower } | Rest])
            end
    end;

combine_factors([]) -> [].



distribute([{ Coef1, Factors1 } | Left], Right) ->
    lists:map(
      fun ({ Coef2, Factors2 }) ->
              { Coef1 * Coef2, combine_factors(
                                 lists:append(Factors1, Factors2)
                                ) } end, Right) ++ distribute(Left, Right);

distribute([], _) -> [].



print([Term = { Coef, Factors } | Poly]) ->
    case Term of
        { 1, [] } -> io:fwrite("1");
        { _, [] } -> io:fwrite("~w", [Coef]);
        { 1, _ } -> print_factors(Factors);
        { _, _ } -> io:fwrite("~w ", [Coef]), print_factors(Factors)
    end,
    io:fwrite(case Poly of [] -> "~n"; _ -> " + " end),
    print(Poly);

print([]) -> ok.

print_factors([{ Var, Power } | Factors]) ->
    if
        Power =:= 1 -> io:fwrite("~w", [Var]);
        true -> io:fwrite("~w^~w", [Var, Power])
    end,
    case Factors of
        [] -> ok;
        _ -> io:fwrite(" ")
    end,
    print_factors(Factors);

print_factors([]) -> ok.
