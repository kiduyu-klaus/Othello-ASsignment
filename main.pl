% 3.2 Library software
:- use_module( [library( lists ),
		io,
		fill] ).

% 3.4 Board representation 

% succeeds when its argument is the character representing a black piece in the representation.
is_black( x ).

% succeeds when its argument is the character representing a white piece in the representation.
is_white( y ).

% succeeds when its argument is the empty square character in the representation.
is_empty( ' ' ).

% succeeds when its argument is either the black character or the white character.
% for black piece
is_piece( X ) :-
	is_black( X ).

% succeeds when its argument is either the black character or the white character.
% for white piece
is_piece( Y ) :-
	is_white( Y ).

% succeeds when both its arguments are player representation characters, but they are different.
other_player( X, Y ) :-
	is_piece( X ),
	is_piece( Y ),
	\+ X = Y.

% succeeds when its first argument is a row number (between 1 and 8) and its second is a representation of a board state.
row( Num, Board, row( Num, X, Y, Z ) ) :-
	nth1( Num, Board, [X, Y, Z] ).

% succeeds when its first argument is a column number (between 1 and 8) and its second is a representation of a board state.
column( Num, [R1,R2,R3], col( Num, X, Y, Z ) ) :-
	nth1( Num, R1, X ),
	nth1( Num, R2, Y ),
	nth1( Num, R3, Z ).

% succeeds when its first two arguments are numbers between 1 and 8, and its third is a representation of a board state.
square( A, B, Board, squ( A, B, Piece )) :-
	nth1( A, Board, Column ),
	nth1( B, Column, Piece ).

% succeeds when its first two arguments are coordinates on the board (which is specified in argument 3), and the square they name is empty.
empty_square( A, B, Board ) :-
	is_empty( Empty ),
	square( A, B, Board, squ( A, B, Empty )).

% succeeds when its argument represents the initial state of the board.

initial_board( [[E,E,E],
		[E,E,E],
		[E,E,E]] ) :-
	is_empty( E ).

report_illegal() :-
	is_empty( ' ' ). 

% succeeds when its argument unifies with a representation of the board with distinct variables in the places where the pieces would normally go.
empty_board( [[_,_,_],[_,_,_],[_,_,_]] ).


% Spotting a winner
%row
and_the_winner_is( Board, Winner ) :-
    %check if its a piece
	is_piece( Winner ),
	row( _, Board, row( _, Winner, Winner, Winner )).

% Spotting a winner
%column
and_the_winner_is( Board, Winner ) :-
	is_piece( Winner ),
	column( _, Board, col( _, Winner, Winner, Winner )).

and_the_winner_is( Board, Winner ) :-
	is_piece( Winner ),
	diagonal( _, Board, dia( _, Winner, Winner, Winner )).


% count pieces
% succeeds when its first argument is a board representation and its second and third arguments are the number of black and white pieces, respectively.
count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

% Spotting a winner
%diagonal

%diagonals
diagonal( t2b, Board, dia( t2b, A, B, C )) :-
	row( 1, Board, row( 1, A, _, _ )),
	row( 2, Board, row( 2, _, B, _ )),
	row( 3, Board, row( 3, _, _, C )).
diagonal( b2t, Board, dia( b2t, A, B, C )) :-
	row( 1, Board, row( 1, _, _, C )),
	row( 2, Board, row( 2, _, B, _ )),
	row( 3, Board, row( 3, A, _, _ )).


% 3.6 Running a game for 2 human players

play :-
	welcome,
	initial_board( Board ),
	%display_board( Board ),
	is_black( Cross ),
	play( Cross, Board ).
%
play( Player, Board ) :-
	is_black( Player ),
	get_legal_move( Player, X, Y, Board ),
	fill_square( X, Y, Player, Board, NewBoard ),
	is_white( NextPlayer ),
	display_board( NewBoard ),
	play( NextPlayer, NewBoard ),
	!.
%
play( Player, Board ) :-
	is_white( Player ),
	choose_move( Player, X, Y, Board ),
	report_move( Player, X, Y ),
	fill_square( X, Y, Player, Board, NewBoard ),
	is_black( NextPlayer ),
	display_board( NewBoard ),
	play( NextPlayer, NewBoard ),
	!.

enclosing_piece( X, Y, Player, Board, _, _, _ ):-
	enclosing_piece( X, Y, Player, Board, _, _, _ ).
	
% 5 Implementing the heuristics
choose_move( _Player, X, Y, Board ) :-
    empty_square( X, Y, Board ).

%A Useful Predicate

line(N, Board, [A,B,C]):-
  row(N, Board, row(N, A,B,C));
  column(N, Board, col(N, A,B,C));
  diagonal(N, Board, dia(N, A,B,C)).

utility(Board, Value):-
  findall([A,B,C], line(_, Board, [A,B,C]), Lines),
  %lines(Board, Lines),
  writer(Lines, x, 0, Xtotal),
  writer(Lines, o, 0, Ototal),
  write(Xtotal), nl,
  write(Ototal), nl,
  Value is Xtotal - Ototal.

writer([],_, Sum, Total):-
  Total is Sum.
writer([Head|Tail],X, Sum, Total):-
  count(Head, X, Out),
  sum(Out, Result),
  NewSum is Sum + Result,
  writer(Tail,X, NewSum, Total).

sum(0, Result):-
  Result is 0.
sum(1, Result):-
  Result is 0.
sum(2,Result):-
  Result is 1.
sum(3,Result):-
  Result is 10.

