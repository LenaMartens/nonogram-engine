width(25).
row(1,[7,3,1,1,7]).
row(2,[1,1,2,2,1,1]).
row(3,[1,3,1,3,1,1,3,1]).
row(4,[1,3,1,1,6,1,3,1]).
row(5,[1,3,1,5,2,1,3,1]).
row(6,[1,1,2,1,1]).
row(7,[7,1,1,1,1,1,7]).
row(8,[3,3]).
row(9,[1,2,3,1,1,3,1,1,2]).
row(10,[1,1,3,2,1,1]).
row(11,[4,1,4,2,1,2]).
row(12,[1,1,1,1,1,4,1,3]).
row(13,[2,1,1,1,2,5]).
row(14,[3,2,2,6,3,1]).
row(15,[1,9,1,1,2,1]).
row(16,[2,1,2,2,3,1]).
row(17,[3,1,1,1,1,5,1]).
row(18,[1,2,2,5]).
row(19,[7,1,2,1,1,1,3]).
row(20,[1,1,2,1,2,2,1]).
row(21,[1,3,1,4,5,1]).
row(22,[1,3,1,3,10,2]).
row(23,[1,3,1,1,6,6]).
row(24,[1,1,2,1,1,2]).
row(25,[7,2,1,2,5]).

col(1,[7,2,1,1,7]).
col(2,[1,1,2,2,1,1]).
col(3,[1,3,1,3,1,3,1,3,1]).
col(4,[1,3,1,1,5,1,3,1]).
col(5,[1,3,1,1,4,1,3,1]).
col(6,[1,1,1,2,1,1]).
col(7,[7,1,1,1,1,1,7]).
col(8,[1,1,3]).
col(9,[2,1,2,1,8,2,1]).
col(10,[2,2,1,2,1,1,1,2]).
col(11,[1,7,3,2,1]).
col(12,[1,2,3,1,1,1,1,1]).
col(13,[4,1,1,2,6]).
col(14,[3,3,1,1,1,3,1]).
col(15,[1,2,5,2,2]).
col(16,[2,2,1,1,1,1,1,2,1]).
col(17,[1,3,3,2,1,8,1]).
col(18,[6,2,1]).
col(19,[7,1,4,1,1,3]).
col(20,[1,1,1,1,4]).
col(21,[1,3,1,3,7,1]).
col(22,[1,3,1,1,1,2,1,1,4]).
col(23,[1,3,1,4,3,3]).
col(24,[1,1,2,2,2,6,1]).
col(25,[7,1,3,2,1,1]).

fixed(4,4).
fixed(4,5).
fixed(4,13).
fixed(4,14).
fixed(4,22).
fixed(9,7).
fixed(9,8).
fixed(9,11).
fixed(9,15).
fixed(9,16).
fixed(9,19).
fixed(17,7).
fixed(17,12).
fixed(17,17).
fixed(17,21).
fixed(22,4).
fixed(22,5).
fixed(22,10).
fixed(22,11).
fixed(22,16).
fixed(22,21).
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
