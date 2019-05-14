% Author: Wenyen Wei <wenyenw@student.unimelb.edu.au>
% Login Id: 949624

% Purpose: The objective of this project is to practice and assess 
% my understanding of logic programming and Prolog.
% This project is to implement code to solve maths puzzles.

% The program: A maths puzzle is a square grid of squares, each to
% be filled in with a single digit 1–9 (zero is not permitted)
% satisfying constraints: 
% 1. each row and each column contains no repeated digits.
% 2. all squares on the diagonal line from upper left to lower 
% right contain the same value.
% 3. the heading of reach row and column (leftmost square 
% in a row and topmost square in a column) holds either 
% the sum or the product of all the digits in that row or column.


:- ensure_loaded(library(clpfd)).

% check all diagnose elements are the same
check_diag(_, [], _).
check_diag(1, [R|Rs], E) :- nth0(1, R, E), check_diag(2, Rs, E).
check_diag(Ind, [R|Rs], E) :- 
	nth0(Ind, R, E), IndNext is Ind+1, check_diag(IndNext, Rs, E).

% entry point for check_diag/3
check_diag([R|Rs]) :- nth0(1, R, E), check_diag(2, Rs, E).

% sum all element in the row
sum_row([], 0).
sum_row([R0|R], S) :- sum_row(R, SR), S is R0 + SR.

% check if sum of the row equals the first element
check_row_sum([]).
check_row_sum([R0|R0s]) :- sum_row(R0s, SR), R0 is SR.

% product all element in the row
prod_row([], 1).
prod_row([R0|R], S) :- prod_row(R, PR), S is R0 * PR.

% check if product of the row equals the first element
check_row_prod([]).
check_row_prod([R0|R0s]) :- prod_row(R0s, PR), R0 is PR.

% check if the row match either of sum or product condition
check_row_valid(R) :- check_row_sum(R).
check_row_valid(R) :- check_row_prod(R).

% check all rows match either of sum or product condition
check_rows_valid([]).
check_rows_valid([R|Rs]) :- check_row_valid(R), check_rows_valid(Rs).

% check if there is repeat digits for rows in the matrix
check_repeat([]).
check_repeat([[_|R1]|R]) :- all_distinct(R1), check_repeat(R).

% check if digits between 0-9 in the row
check_digits_row([]).
check_digits_row([R0|R]) :- between(1,9,R0), check_digits_row(R).

% check if digits between 0-9 for rows in the matrix
check_digits([]).
check_digits([[_|R1]|R]) :- check_digits_row(R1), check_digits(R).


% check if all element in row is ground
check_ground_row([]).
check_ground_row([R0|R]) :- ground(R0), check_ground_row(R).

% check if all element is ground for rows in the matrix
check_ground([]).
check_ground([[_|R1]|R]) :- check_ground_row(R1), check_ground(R).

% check lengths of all lows in the list are the same
check_length([], _).
check_length([R0|Rs], L) :- length(R0, L), check_length(Rs, L).

puzzle_solution([R0|Rs]) :- 
	length(R0, L), check_length(Rs, L),
	transpose([R0|Rs], [_|Ts]),
	check_repeat(Rs), check_repeat(Ts),
	check_digits(Rs),
	check_diag(Rs),
	check_rows_valid(Rs), check_rows_valid(Ts),
	flatten([R0|Rs],FL),
	maplist(ground,FL).
