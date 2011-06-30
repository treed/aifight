-module(gameboard).
-export([start/1, init/1, look/1, place/2, move/2, quit/0, test/0, test_spawn/0]).
-record(board, {x_size, y_size, grid}).

start(Size) ->
    register(?MODULE, Pid=spawn(?MODULE, init, [Size])),
    Pid.

init(Size) ->
    loop(create_board(Size)).

loop(Board) ->
    receive
        {From, Ref, place, Piece, Location} ->
            New_Board = place_piece(Board, Piece, Location),
            From ! {Ref, ok},
            loop(New_Board);
        {From, Ref, move, Origin, Destination} ->
            try move_piece(Board, Origin, Destination) of
                New_Board ->
                    From ! {Ref, ok},
                    loop(New_Board)
            catch
                _:_ ->
                    From ! {Ref, error}
            end;
        {From, Ref, look, Location} ->
            From ! {Ref, get_piece(Board, Location)},
            loop(Board);
        {From, Ref, quit} ->
            From ! {Ref, ok};
        Whatever ->
            io:format("Got unexpected message: ~p~n", [Whatever]),
            loop(Board)
    end.

place(Piece, Location) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, place, Piece, Location},
    receive
        {Ref, ok} -> ok
    end.

move(Origin, Destination) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, move, Origin, Destination},
    receive
        {Ref, Response} -> Response
    after 1000 ->
        timeout
    end.

look(Location) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, look, Location},
    receive
        {Ref, Piece} -> Piece
    end.

quit() ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, quit},
    receive
        {Ref, ok} -> ok
    end.

create_board({X_Size, Y_Size}) ->
    #board{x_size=X_Size, y_size=Y_Size, grid=create_grid({X_Size, Y_Size})}.

create_grid({X_Size, Y_Size}) ->
    lists:foldl(fun(Key, Dict) -> dict:store(Key, empty, Dict) end, dict:new(), [{X, Y} || X <- lists:seq(1,X_Size), Y <- lists:seq(1,Y_Size)]).

place_piece(_Board = #board{}, _Piece, {X, _Y}) when X < 1 ->
    throw(invalid_position);
place_piece(_Board = #board{}, _Piece, {_X, Y}) when Y < 1 ->
    throw(invalid_position);
place_piece(Board = #board{}, _Piece, {X, _Y}) when X > Board#board.x_size ->
    throw(invalid_position);
place_piece(Board = #board{}, _Piece, {_X, Y}) when Y > Board#board.y_size ->
    throw(invalid_position);
place_piece(Board, Piece, {X, Y}) ->
    case dict:fetch({X, Y}, Board#board.grid) of
        empty -> Board#board{grid=dict:store({X, Y}, Piece, Board#board.grid)};
        _ -> throw(position_not_empty)
    end.

get_piece(_Board = #board{}, {X, _Y}) when X < 1 ->
    throw(invalid_position);
get_piece(_Board = #board{}, {_X, Y}) when Y < 1 ->
    throw(invalid_position);
get_piece(Board = #board{}, {X, _Y}) when X > Board#board.x_size ->
    throw(invalid_position);
get_piece(Board = #board{}, {_X, Y}) when Y > Board#board.y_size ->
    throw(invalid_position);
get_piece(Board, Location) ->
    dict:fetch(Location, Board#board.grid).

move_piece(Board, Origin, Destination) ->
    Piece = get_piece(Board, Origin),
    New_Board = place_piece(Board, Piece, Destination),
    remove_piece(New_Board, Origin).

remove_piece(Board, {X, Y}) ->
    Board#board{grid=dict:store({X, Y}, empty, Board#board.grid)}.

test_spawn() ->
    start({5,5}),
    place(hooty, {1,1}),
    move({1,1},{1,2}),
    quit(),
    ok.

test() ->
    Empty_Board = create_board({5,5}),
    empty = get_piece(Empty_Board,{1,1}),
    empty = get_piece(Empty_Board,{1,2}),
    Board = place_piece(Empty_Board, hooty, {1,1}),
    hooty = get_piece(Board, {1,1}),
    empty = get_piece(Board, {1,2}),
    Moved_Board = move_piece(Board, {1,1}, {1,2}),
    empty = get_piece(Moved_Board, {1,1}),
    hooty = get_piece(Moved_Board, {1,2}),
    ok.
