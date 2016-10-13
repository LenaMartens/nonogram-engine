%13,487 inferences, 0.000 CPU in 0.070 seconds (0% CPU, Infinite Lips)
%455,486,695 inferences, 145.594 CPU in 324.428 seconds (45% CPU, 3128477 Lips)
width(20).
row(1,[7]).
row(2,[1,4]).
row(3,[4]).
row(4,[5,1]).
row(5,[2,5,1]).
row(6,[1,5,2]).
row(7,[2,1,1]).
row(8,[3,6,2]).
row(9,[4,6,3]).
row(10,[3,6,3]).
row(11,[1,11]).
row(12,[5,6,1,2]).
row(13,[1,1,1,2,1,2,3]).
row(14,[5,1,7]).
row(15,[6,1,4]).
row(16,[16]).
row(17,[14]).
row(18,[3,1,1,1,1,2]).
row(19,[13]).
row(20,[12]).


col(1,[1,4]).
col(2,[3,1,3]).
col(3,[4,8]).
col(4,[8,7]).
col(5,[1,9]).
col(6,[3,2]).
col(7,[1,1,1,5]).
col(8,[4,6,2,2]).
col(9,[1,4,5,5]).
col(10,[17,2]).
col(11,[1,4,5,5]).
col(12,[1,2,5,2,2]).
col(13,[1,4,5]).
col(14,[2,1,4,2]).
col(15,[2,1,12]).
col(16,[1,8,6]).
col(17,[1,5,3]).
col(18,[4]).
col(19,[2]).
col(20,[2]).

fixed(22,22).
:-  dynamic  solved_row/2. 
try(RowBoard) :- width(X),
		numlist(1,X,Numl),
		retractall(solved_row(_,_)),
		save_rows_in_db(Numl),
		generate_pos_sol(Numl,[],RowBoard).
		%generate_possible_cols(Numl,[],Colm),
		
		
		%generate_possible_rows(Numl,[],Rows),
		%construct_row_board(Rows,RowBoard),
		%construct_col_board(Colm,ColBoard),
		%RowBoard =ColBoard.

save_rows_in_db([]).
save_rows_in_db([X|Rest]) :- pos_row(X,T),
							assert(solved_row(X,T)),
							save_rows_in_db(Rest).
		
generate_pos_sol([],Sol,Sol).		
generate_pos_sol([X|Rest],Sol,Out) :- pos_col(X,T),
								
							  member(Candidate,T),
						append(Sol,[Candidate],NewSol),
						
						construct_col_board(NewSol,Board),
						open('twentyfivebaby.out',append,Stream),
						write(Stream,X),nl(Stream),
						close(Stream),
						still_valid(Board,X,1),
						
						generate_pos_sol(Rest,NewSol,Out).
still_valid([],Length,N).						
still_valid(Board,Length,N) :- take(Length,Board,Row),
							valid_incomplete_row(N,Row),
							
							trim(Board,Length,NewBoard),
							NewN is N + 1,
							still_valid(NewBoard,Length,NewN).

total_pos_col([],L,P) :- L = P.
total_pos_col([X|Rest],L,Product) :- numlist(1,X,Numl),
		pos_col(X,Colm),length(Colm,Len), NewProduct is Len* Product, total_pos_col(Rest,L,NewProduct).
total_pos_row([],L,P) :- L = P.
total_pos_row([X|Rest],L,Product) :- numlist(1,X,Numl),
		pos_row(X,Colm),length(Colm,Len), NewProduct is Len* Product, total_pos_row(Rest,L,NewProduct).
		
		
generate_possible_cols([],L,L).
generate_possible_cols([X|Rest],L,Out) :- pos_col(X,T),
										member(Candidate,T),
										append(L,[Candidate],NewL),
										generate_possible_cols(Rest,NewL,Out).

generate_possible_rows([],L,L).
generate_possible_rows([X|Rest],L,Out) :- pos_row(X,T),
										member(Candidate,T),
										append(L,[Candidate],NewL),
										generate_possible_rows(Rest,NewL,Out).


construct_row_board(ListOfLists,List) :- flatten(ListOfLists,List).

construct_col_board([X],X).
construct_col_board([X,Y|Rest],Out) :- interleave(X,Y,[],Inter), construct_col_board([Inter|Rest],Out).

interleave([],[],C,Out) :- C = Out.
interleave(A,[X|B],C,Out) :- length(B,LB),
						   length(A,LA),
						   Tak is (LA / (LB+1)),
							take(Tak,A,Front),
								trim(A,Tak,Rest),
								append(Front,[X],NewC),
								append(C,[NewC],NewerC),
								flatten(NewerC,PerfectC),
								interleave(Rest,B,PerfectC,Out).

take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

trim(L,N,S) :-    % to trim N elements from a list
  length(P,N) ,   % - generate an unbound prefix list of the desired length
  append(P,S,L) . % - and use append/3 to get the desired suffix.

passes_fixed([],L).
passes_fixed([C|I],L) :- take(C,L,Check), last(Check,Last), Last = 1, passes_fixed(I,L).

pos_row(X,L) :- setof(Y,X^(configuration_row(X,Y)),Inter),
				findall(T,fixed(X,T),I),
				include(passes_fixed(I), Inter, L).
configuration_row(X,L) :- row(X,Z),
						base_valid([],Z,YAY),
						reverse(YAY,L).
pos_col(X,L) :- setof(Y,X^(configuration_col(X,Y)),Inter),
					findall(T,fixed(T,X),I),
				include(passes_fixed(I), Inter, L).
configuration_col(X,L) :- col(X,Z),
						base_valid([],Z,YAY),
						reverse(YAY,L).


valid_incomplete_row(X,Sol) :- solved_row(X,L),
							include(prefix(Sol),L,Z),
							length(Z,Found),
							Found > 0 .
						
base_valid(Sol,[],NewSol) :- length(Sol,X),
						width(Y),
						Z is Y - X ,
						pad(Sol,Z,NewSol).
base_valid(Sol,Con,NewSol) :- length(Sol,X),
						width(X),!,
						length(Con,Y),
						Y is 0,
						NewSol = Sol.
base_valid(Sol,[Y|Con],N) :- X is 1,
							   add_whole_group(Sol,Y,NewSol),
							   base_valid(NewSol,Con,N).
base_valid(Sol,[Y|Con],NewSol) :- X is 0,
						base_valid([X|Sol],[Y|Con],NewSol).




add_whole_group(Sol,0,New) :- length(Sol,Len),
						     width(X),
							 X > Len,
							New = [0|Sol].
add_whole_group(Sol,0,New) :- length(Sol,Len),
						     width(X),
							 X < Len,!,
							false.
add_whole_group(Sol,0,New) :- length(Sol,Len),
							  width(Len),
							New = Sol.

add_whole_group(Sol,Y,New) :- Y > 0,
							Y2 is Y - 1,
						add_whole_group([1|Sol],Y2,New).
						
pad(Sol,0,NewSol) :- NewSol = Sol.
pad(Sol,Z,NewSol) :- Z > 0,
					FU is Z-1,
				pad([0|Sol],FU,NewSol).
