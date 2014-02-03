/* Sudoku Solver 2013 */
/* Rodrigo Alves */

/* This runs all the simple tests. If it 
works correctly, you should see three identical 
and completed sudoku tables, and finally the 
word false (as test0c will fail.) */
test :-
	test0, nl,
	test0a, nl,
	test0b, nl,
	test0c.

/* This is a completely solved solution. */
test0 :-
	L = [
             [9,6,3,1,7,4,2,5,8],
             [1,7,8,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,9,6],
             [4,9,6,8,5,2,3,1,7],
             [7,3,5,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This has a solution (the one in test0) which 
should be found very quickly. */
test0a :-
	L = [
             [9,_,3,1,7,4,2,5,8],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,_,6],
	     [4,9,6,8,5,2,3,1,7],
             [7,3,_,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This has a solution (the one in test0) and 
may take a few seconds to find. */
test0b :-
	L = [
             [9,_,3,1,7,4,2,5,_],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,_,9,_,3,1],
             [_,2,1,4,3,_,5,_,6],
             [4,9,_,8,_,2,3,1,_],
             [_,3,_,9,6,_,8,2,_],
             [5,8,9,7,1,3,4,6,2],
             [_,1,7,2,_,6,_,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This one obviously has no solution (column 2 has 
two nines in it.) and it may take a few seconds 
to deduce this. */
test0c :-
	L = [
             [_,9,3,1,7,4,2,5,8],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,_,6],
	     [4,9,6,8,5,2,3,1,7],
             [7,3,_,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

test0d :-
	L = [
             [9,_,3,1,_,4,2,5,_],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,_,9,_,3,1],
             [_,2,1,4,3,_,5,_,6],
             [4,9,_,8,_,2,3,1,_],
             [_,3,_,9,6,_,8,2,_],
             [5,8,9,7,1,3,4,6,2],
             [_,1,7,2,_,6,_,8,5],
             [6,4,2,5,_,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

% print suduko table
printsudoku([]).
printsudoku([H|T]) :-
	write(H),nl,
	printsudoku(T).


% Expects a list of lists 9 by 9 grid.
sudoku(M) :-
	transpose(M, RotatedM),
	check(8, M, RotatedM).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          HELPERS                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Make sure the whole matrix follows the sudoku's rule.
check(-1, M, _) :- checkblocks(M). % Time to check the blocks
check(ROW, M, RM) :-  
	ROW >= 0,
	rowcheck(ROW, RM), % column check
	rowcheck(ROW, M),
	R is ROW - 1, 
	check(R, M, RM).

% Make sure a row has no repetitions.
rowcheck(ROW, M) :- row(ROW, M, LRow), sudokulist(LRow).

% Make sure all blocks have no repetitions.
checkblocks(M) :- % Done this way for performance.
	blockcheck(0, 0, M),
	blockcheck(0, 1, M),
	blockcheck(0, 2, M),
	blockcheck(1, 0, M),
	blockcheck(1, 1, M),
	blockcheck(1, 2, M),
	blockcheck(2, 0, M),
	blockcheck(2, 1, M),
	blockcheck(2, 2, M).

blockcheck(ROW, COL, Matrix) :-
	block(ROW, COL, Matrix, LBlock), sudokulist(LBlock).
	
% Return a list with elements from a specific row in Matrix M.
row(0, [H|_], H).
row(ROW, [_|T], List) :- ROW > 0, R is ROW-1, row(R, T, List).


% Search for the block from top to bottom, by jumping from 3 to 3 rows.
% A block is a 3x3 submatrix. So, the list has length 9.
% Return a list with the elements of the block.
% ROW and COL are the coordinates of the BLOCK as if it was a single element.
block(0, COL, [H1, H2, H3 | _], List) :- 
	extractblock(COL, H1, H2, H3, List).
block(ROW, COL, [_, _, _ | T], List) :- 
	ROW > 0, R is ROW-1, block(R, COL, T, List).


% Given the column of the block and the 3 rows, extract the elements
% of the block and put it into List.
extractblock(0, [H1, H2, H3|_], 
		[H4, H5, H6|_], 
		[H7, H8, H9|_], 
		[H1, H2, H3, H4, H5, H6, H7, H8, H9]). 
extractblock(COL, [_,_,_|T1], [_,_,_|T2], [_,_,_|T3], List) :- 
	COL > 0, C is COL-1, extractblock(C, T1, T2, T3, List).


% Transpose a function, so that its rows become its columns and vice-versa.
% Useful to extract the list of columns.
transpose([], []).
transpose([H|T], Result) :- transposehelper(H, [H|T], Result).

transposehelper([], _, []).
transposehelper([_|Rs], Ms, [Ts|Tss]) :-
        decapitate(Ms, Ts, Ms1), transposehelper(Rs, Ms1, Tss).

% Extract the heads of the lists.
decapitate([], [], []).
decapitate([[Head|Os]|Rest], [Head|Fs], [Os|Oss]) :- decapitate(Rest, Fs, Oss).

% Make sure a list contain exactly the elements from 1 to 9.
sudokulist(List) :- % Done this way for better performance.
	member(1, List), 
	member(2, List),
	member(3, List),
	member(4, List),
	member(5, List),
	member(6, List),
	member(7, List),
	member(8, List),
	member(9, List).		

% Check if X is a member of the list
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

