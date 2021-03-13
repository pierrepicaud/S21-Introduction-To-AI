% (*) Find the last element of a list.
%    Example:
%    ?- my_last(X,[a,b,c,d]).
%    X = d

:-use_module(library(lists)).

% basecase, if given empty list, then math the empty list, otherwise,...
% my_last([], []).

% basecase, if list has only one item then return that item.
my_last(X, X):- length(X, 1).

% recursion, if tail's length is not 1, recursively pops the head
my_last(Result, [_|Result]):- atom_length(Result, 1).

% recursively pops the head?
my_last(Result, [_|T]):-
    my_last(Result, T).

% it works but it gives X = [4], instead of X = 4.
