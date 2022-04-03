-module(test).
-export([test/0]).

test() ->
    Left = [{ 5, [{ x, 1 }, { y, 2 }, { z, 1 }] }, { 3, [{ y, 1 }] }],
    Right = [{ 3, [{ x, 2 }] },
             { 3, [{ y, 1 }] },
             { 1, [{ z, 1 }, { x, 1 }, { y, 2 }] }],

    io:fwrite("Starting server...~n"),

    Pid = spawn(server, start, []),

    io:fwrite(
      "Server started.~nUsing polynomials:~n  ~s~n  ~s~nRequesting sumation, subtraction and multipliation...~n",
      [poly:to_string(Left), poly:to_string(Right)]
     ),

    SumResult = client:request_sum(Left, Right),
    SubtractionResult = client:request_subtract(Left, Right),
    ProductResult = client:request_multiply(Left, Right),

    io:fwrite(
      "Got answers:~n  Sum: ~s~n  Difference: ~s~n  Product: ~s~nClosing server...~n",
      [pretiffy(SumResult), pretiffy(SubtractionResult), pretiffy(ProductResult)]
     ),

    exit(Pid, ok),

    io:fwrite("Server successfully closed.~n").

pretiffy({ _, Poly }) ->
    poly:to_string(Poly).
