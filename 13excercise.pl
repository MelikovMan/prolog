father(bastard,marisa).
mother(alice,marisa).
father(bastard,tom).
mother(alice,tom).
father(el,bastard).
male(bastard).
male(tom).
female(X):- \+(male(X)).
parent(X,Y):-father(X,Y);mother(X,Y).
is_mother(X):-mother(X,_).
is_father(X):-father(X,_).
is_son(X):- male(X),mother(_,X);father(_,X).
share_parents(X,Y):-(   mother(Z,X);father(Z,X)),(   mother(Z,Y);father(Z,Y)).
grandfather(X,Y):-father(X,Z),father(Z,Y).
is_sister(X,Y):-share_parents(X,Y),!,female(X).
