hanoi(N):-move(N,left,middle,right).
move(0,_,_,_):-!.
move(N,A,B,C):-M is N-1, move(M,A,C,B), notify(A,B),move(M,C,B,A).
notify(A,B):-write([moved,disk,from,A,to,B]),nl.
