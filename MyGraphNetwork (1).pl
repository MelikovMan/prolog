:- dynamic edge/6.
:- dynamic car/3.
edge(a,b,2).
edge(b,a,2).
edge(b,c,5).
edge(c,b,5).
edge(c,d,10).
edge(c,e,12).
edge(e,c,12).
edge(d,e,13).
edge(c,f,1).
edge(f,e,6).
edge(f,a,15).
busy_coef(1,5).
busy_coef(2,4).
busy_coef(3,3).
busy_coef(4,2).
busy_coef(5,1.5).
member(X,[H|T]):- X = H; member(X,T).
pop([H|T],H,T).
pop2([H1,H2|T],H1,H2,T).
append(ListLeft,[],ListLeft).
append([],ListRight,ListRight).
append([ListLeftH|ListLeftT],ListRight,[ListResultH|ListResultT]):-
    ListResultH = ListLeftH, 
    append(ListLeftT,ListRight,ListResultT).

reverse([],R,R).
reverse([H|T],T2,R):-reverse(T,[H|T2],R).
reverse(L,R):-reverse(L,[],R).

find_min_list([E],E,0).

find_min_list_index([H|T],X,I):-find_min_list_index(T,H,X,I,0,1).
find_min_list_index([],CurMin,CurMin,Index,Index,_).
find_min_list_index([H|T],CurMin,Min,Index,MinIndex,CurIndex):- NextIndex is CurIndex+1, (   H < CurMin ->  
    NewMin is H, find_min_list_index(T,NewMin,Min,Index,CurIndex,NextIndex)
    ;find_min_list_index(T,CurMin,Min,Index,MinIndex,NextIndex)).

pop_firsts([],[],[]).
pop_firsts([NestedListHead|NestedListTail],[H|T],[PoppedListHead|PoppedListTail]):-pop(NestedListHead,H,PoppedListHead)
    ,pop_firsts(NestedListTail,T,PoppedListTail).

extract_index([H|T],I,X):- I =< 0 ->  X = H; I1 is I-1, extract_index(T,I1,X).

path(Start,End,Path):-
    path(Start,End,[],Path).
path(Now, End, Acc, Path) :-
    edge(Now, End),
    \+ member(Now, Acc),
    \+ member(End, Acc),
    reverse([End,Now|Acc],Path).
path(Now, End, Acc, Path) :-
    edge(Now, Mid),
    \+ member(Mid, Acc),
    path(Mid, End, [Now|Acc], Path).

path_weights(Start,End,Path):-
    path_weights(Start,End,[],Path, 0).
path_weights(Now, End, Acc, Path, WeightAcc) :-
    edge(Now, End, Weight),
    \+ member(Now, Acc),
    \+ member(End, Acc),
    Weights is WeightAcc+Weight,
    reverse([End,Now|Acc],Tail),
    Path = [Weights|Tail].
path_weights(Now, End, Acc, Path, WeightAcc) :-
    edge(Now, Mid, Weight),
    \+ member(Mid, Acc),
    WeightsA is WeightAcc+Weight,
    path_weights(Mid,End,[Now|Acc],Path,WeightsA).

path_weights_busy(Start,End,Path,CarSpeed):-
    path_weights_busy(Start,End,[],Path, 0, CarSpeed,0).
path_weights_busy(Now, End, Acc, Path, WeightAcc,CarSpeed,TimeAcc) :-
    edge(Now, End, Weight, Busy, MaxSpeed),
    \+ member(Now, Acc),
    \+ member(End, Acc),
    (  CarSpeed > MaxSpeed ->  Speed is MaxSpeed; Speed is CarSpeed ),
    Time is Weight/Speed,
    retract(edge(Now, End, Weight, Busy,MaxSpeed)),
    B1 is Busy+Time/2,
    asserta(edge(Now, End, Weight, B1,MaxSpeed)),
    Weights is WeightAcc+Weight,
    FinalTime is TimeAcc + Time, 
    reverse([End,Now|Acc],Tail),
    Path = [FinalTime,Weights|Tail].
path_weights_busy(Now, End, Acc, Path, WeightAcc,CarSpeed,TimeAcc) :-
    edge(Now, Mid, Weight,Busy,MaxSpeed),
    \+ member(Mid, Acc),
    (  CarSpeed > MaxSpeed ->  Speed is MaxSpeed; Speed is CarSpeed ),
    Time is Weight/Speed,
    retract(edge(Now, Mid, Weight, Busy,MaxSpeed)),
    B1 is Busy+Time/2,
    asserta(edge(Now, Mid, Weight, B1,MaxSpeed)),
    WeightsA is WeightAcc+Weight,
    TimeA is TimeAcc+Time,
    path_weights_busy(Mid,End,[Now|Acc],Path,WeightsA,CarSpeed,TimeA).

path_weights_busy_no_mutate(Start,End,Path,CarId):-
    path_weights_busy_no_mutate(Start,End,[],Path, 0, CarId,0).
path_weights_busy_no_mutate(Now, End, Acc, Path, WeightAcc,CarId,TimeAcc) :-
    edge(Now, End, Weight, Busy, MaxSpeed, MaxCarClass),
    \+ member(Now, Acc),
    \+ member(End, Acc),
    car(CarId,CarSpeed,CarClass),
    \+ (CarClass > MaxCarClass),
    (  CarSpeed > MaxSpeed ->  Speed is MaxSpeed; Speed is CarSpeed ),
    Time is Weight/Speed + Busy,
    Weights is WeightAcc+Weight,
    FinalTime is TimeAcc + Time, 
    reverse([End,Now|Acc],Tail),
    Path = [FinalTime,Weights|Tail].
path_weights_busy_no_mutate(Now, End, Acc, Path, WeightAcc,CarId,TimeAcc) :-
    edge(Now, Mid, Weight,Busy,MaxSpeed, MaxCarClass),
    \+ member(Mid, Acc),
    car(CarId,CarSpeed,CarClass),
    \+ (CarClass > MaxCarClass),
    (  CarSpeed > MaxSpeed ->  Speed is MaxSpeed; Speed is CarSpeed ),
    Time is Weight/Speed+Busy,
    WeightsA is WeightAcc+Weight,
    TimeA is TimeAcc+Time,
    path_weights_busy_no_mutate(Mid,End,[Now|Acc],Path,WeightsA,CarId,TimeA).

mutate_edges([],_).
mutate_edges([_],_).
mutate_edges([Edge1,Edge2|T],CarId):-
    edge(Edge1,Edge2,Weight,Busy,MaxSpeed, MaxCarClass),
    car(CarId,CarSpeed,CarClass),
     (  CarSpeed > MaxSpeed ->  Speed is MaxSpeed; Speed is CarSpeed ),
    Time is Weight/Speed + Busy,
    retract(edge(Edge1, Edge2, Weight, Busy,MaxSpeed,MaxCarClass)),
    busy_coef(CarClass,Coef),
    B1 is Busy+Time/Coef,
    asserta(edge(Edge1, Edge2, Weight, B1,MaxSpeed,MaxCarClass)),
    mutate_edges([Edge2|T],CarId).

find_shortest_path(Start,End,ResultPath,Weight):-
    findall(Path,path_weights(Start,End,Path),Paths),
    pop_firsts(Paths,Weights,_),
    find_min_list_index(Weights,Weight,Index),
    extract_index(Paths,Index,Final),
    pop(Final,Weight,ResultPath).

find_shortest_path_busy(Start,End,ResultPath,Weight,Time,CarId):-
    findall(Path,path_weights_busy_no_mutate(Start,End,Path,CarId),Paths),
    pop_firsts(Paths,Weights,_),
    find_min_list_index(Weights,Time,Index),
    extract_index(Paths,Index,Final),
    pop2(Final,Time,Weight,ResultPath),
    mutate_edges(ResultPath,CarId).

network(Paths,Weights,Times):-

    asserta(edge(a,b,2,0,50,3)),
    asserta(edge(b,a,2,0,50,2)),
	asserta(edge(b,c,5,0,30,4)),
    asserta(edge(c,b,5,0,30,4)),
	asserta(edge(c,d,10,0,60,4)),
	asserta(edge(c,e,12,0,80,5)),
	asserta(edge(e,c,12,0,80,5)),
	asserta(edge(d,e,13,0,60,4)),
	asserta(edge(c,f,1,0,60,4)),
	asserta(edge(f,a,15,0,120,5)),
    asserta(edge(f,e,6,0,70,5)),
    asserta(edge(a,f,15,0,120,5)),   
    asserta(edge(b,d,4,0,40,5)),  
    asserta(edge(d,b,4,0,40,5)),
    asserta(edge(e,g,8,0,80,5)),  
    asserta(edge(g,e,8,0,80,5)),
    asserta(edge(d,h,12,0,70,4)),
    asserta(edge(h,d,12,0,70,4)),
    asserta(edge(h,i,3,0,40,2)),
    asserta(edge(i,h,3,0,40,2)),
    asserta(edge(g,i,7,0,60,5)),
    asserta(edge(i,g,7,0,60,5)),
    asserta(edge(i,e,5,0,60,4)),
    asserta(edge(e,h,5,0,60,4)),
    asserta(edge(f,g,8,0,80,5)),
    asserta(edge(g,f,8,0,80,5)),
	asserta(car(2,80,2)),
    asserta(car(1,70,1)),
    asserta(car(3,50,4)),   
    asserta(car(4,30,4)),
    asserta(car(5,120,4)),
    asserta(car(6,70,1)),   
    find_shortest_path_busy(a,e,P2,W2,T2,2),
    %path_weights_busy_no_mutate(a,e,P,2),
    find_shortest_path_busy(b,i,P1,W1,T1,1),
    find_shortest_path_busy(f,g,P3,W3,T3,3),
    find_shortest_path_busy(a,g,P4,W4,T4,4),
    find_shortest_path_busy(e,f,P5,W5,T5,5),
    find_shortest_path_busy(c,i,P6,W6,T6,6),
    retractall(edge(_,_,_,_,_,_)),
    retractall(car(_,_,_)),
    %pop2(NP1,T1,W1,P1),
    %pop2(NP2,T2,W2,P2),
    %pop2(NP3,T3,W3,P3),
    %pop2(NP4,T4,W4,P4),
    %pop2(NP5,T5,W5,P5),
    Paths=[P1,P2,P3,P4,P5,P6],
    Times=[T1,T2,T3,T4,T5,T6],
    Weights = [W1,W2,W3,W4,W5,W6].