% LABORATORIO DE LENGUAJES DE PROGRAMACION
% GRUPO: 21.
% PROYECTO 2.

% generadorSopa :- Tiene exito si los archivos pedidos por I/O estandar son validos
% 				   y los predicados que utiliza se cumplen
 generadorSopa :-
	write('Tamano? '),
	read(Length),
	nl,
	write('Alfabeto? '),
	read(Alphabet),
	nl,
	write('Archivo de palabras aceptadas? '),
	read(AcceptedWords),
	nl,
	write('Archivo de palabras rechazadas? '),
	read(RejectedWords),
	nl,
	sopaLetra(Length, Alphabet, AcceptedWords, RejectedWords, Sopa),
	nl,
	mostrarSopa(Sopa),
	nl,
	ofrecerMas,
	!.


% sopaLetra :- Tiene exito si existen sopas de letras que no se han creado
 sopaLetra(Length, Alphabet, AcceptedWords, RejectedWords, Sopa) :- 
	cargarListaArchivo(AcceptedWords, AcceptedWordsList, Alphabet),
	cargarListaArchivo(RejectedWords, RejectedWordsList, Alphabet),
	creadorMatriz(Length, Alphabet, Sopa, Length),
	filtrarResultados(Sopa, AcceptedWordsList, RejectedWordsList).


% mostrarSopa :- Se cumple si se puede mostrar las letras de las listas de una lista
 mostrarSopa([]) :- !.

 mostrarSopa([H|T]) :-
	mostrarSopaAux(H),
	nl,
	!,
	mostrarSopa(T).


% mostrarSopaAux :- Se cumple si se puede mostrar en pantalla las letras de una lista
 mostrarSopaAux([]).

 mostrarSopaAux([H|T]) :-
	display(H),
	tab(2),
	mostrarSopaAux(T).


% cargarListaArchivo :- Tiene exito si se pueden abrir los archivos de entrada
% 						y sus caracteres pertenecen al alfabeto dado
 cargarListaArchivo(File, List, Alphabet) :-
	see(File),
	read(List),
	seen,
	letrasPertenecenALista(List, Alphabet),
	seen.


% letrasPertenecenALista :- Se cumple si las letras de una lista de palabras
% 							pertenecen a otra
 letrasPertenecenALista([],[_|_]).

 letrasPertenecenALista([Word|Words], Alphabet) :-
	atom_chars(Word,LetterArray),
	miembroMultiple(LetterArray, Alphabet),
	!,
	letrasPertenecenALista(Words, Alphabet).


% miembroMultiple :- Se cumple si los elementos de una lista estan en otra
 miembroMultiple([],_) :- !.

 miembroMultiple([H|T],List) :-
	member(H,List),
	!,
	miembroMultiple(T,List).


% creadorMatriz :- Tiene exito si se crean listas de listas del tamano y alfabeto dados
 creadorMatriz(0, _, [], _) :- !.

 creadorMatriz(Count, Alphabet, [H|T], Length) :-
	Count > 0,
	!,	
	listaDe(Length, Alphabet, H),
	TailCount is Count - 1,
	creadorMatriz(TailCount, Alphabet, T, Length).


% listaDe :- Tiene exito si existen listas del tamano y del alfabeto dados
 listaDe(0, _, []) :- !.

 listaDe(Length, Alphabet, [H|T]) :-
	Length > 0,
	!,
	member(H, Alphabet),
	TailLength is Length - 1,
	listaDe(TailLength, Alphabet, T).


% filtrarResultados :- Tiene exito si tienen exito los ultimos 2 predicados definidos
 filtrarResultados(Sopa, AcceptedWordsList, RejectedWordsList) :-
	sopaContienePalabras(Sopa, AcceptedWordsList), 				%Contiene todas las palabras aceptadas
	sopaNoContienePalabras(Sopa, RejectedWordsList), 			%No contiene horizontalmente las rechazadas
	trasponer(Sopa, SopaTraspuesta),
	sopaNoContienePalabras(SopaTraspuesta, RejectedWordsList),  %No contiene verticalmente las rechazadas
	length(Sopa, Length),
	diagonal(Sopa, Length, DiagonalesSopa),
	sopaNoContienePalabras(DiagonalesSopa, RejectedWordsList).  %No contiene diagonalmente las rechazadas


% sopaNoContienePalabras :- Recibe una sopa y una lista de palabras y tiene exito 
% 							si ninguna contiene las palabras de la lista
 sopaNoContienePalabras(_, []) :- !.

 sopaNoContienePalabras(Sopa, [FirstWord|Words]) :-
	 not(sopaContienePalabra(Sopa, FirstWord)),
	 sopaNoContienePalabras(Sopa, Words).


% sopaContienePalabras :- Recibe una lista de listas y una lista de palabras 
% 						  y tiene exito si las listas entre todas, contienen las 
%						  palabras de la lista
 sopaContienePalabras(_, []) :- !.

 sopaContienePalabras(Sopa, [FirstWord|Words]) :-
	 sopaContienePalabra(Sopa, FirstWord),
	 sopaContienePalabras(Sopa, Words).

 sopaContienePalabras(Sopa, [FirstWord|Words]) :-
	trasponer(Sopa, SopaTraspuesta),
	sopaContienePalabra(SopaTraspuesta, FirstWord),
	sopaContienePalabras(Sopa, Words).

 sopaContienePalabras(Sopa, [FirstWord|Words]) :-
	length(Sopa, Length),
	diagonal(Sopa, Length, Diagonales),
	sopaContienePalabra(Diagonales, FirstWord),
	sopaContienePalabras(Sopa, Words).


% sopaContienePalabra :- Recibe una sopa y una palabra y tiene exito si
% 						 la palabra se puede encontrar en una de las listas.
 sopaContienePalabra([H|_], Word) :-
	atom_chars(Word, LongWord),
	subPalabra(LongWord, H),
	!.

 sopaContienePalabra([H|_], Word) :-
	atom_chars(Word, LongWord),
	reverse(H, H2),
	subPalabra(LongWord, H2),
	!.

 sopaContienePalabra([_|T], Word) :-
	sopaContienePalabra(T, Word).


% subPalabraMultiple :- Tiene exito si elemento de lista es subpalabra de la palabra
 subPalabraMultiple([H|T], LongWord) :-
	atom_codes(H, Word1),
	atom_codes(LongWord, Word2),
	subPalabra(Word1, Word2);
	subPalabraMultiple(T, LongWord).


% subPalabra :- Tiene exito si el primer string es subpalabra del segundo
 subPalabra(Sub,Str) :- prefijoDe(Sub,Str).
 subPalabra(Sub,[_|Str]) :- subPalabra(Sub,Str).


% prefijoDe :- Tiene exito si 
 prefijoDe(Pre, Str) :- append(Pre, _, Str).


% invertirSopa :- Tiene exito si invierte todas las filas de la sopa.
 invertirSopa([], []).
 invertirSopa([H|T], [S|Ts]):-
	reverse(H, S),
	invertirSopa(T, Ts).


% diagonal :- Tiene exito si encuentra las diagonales, empezando de izquierda
% 			  a derecha en la sopa, y luego la invierte para buscar las
%			  diagonales de derecha a izquierda.
 diagonal(Sopa, Length, S):-
	diagonal1(Sopa, Length, M),
	diagonal2(Sopa, 1, Length, Ms),
	append(M, Ms, D),
	invertirSopa(Sopa, V),
	diagonal1(V, Length, N),
	diagonal2(V, 1, Length, Ns),
	append(N, Ns, Ds),
	append(D, Ds, S),
	!.


% diagonal1 :- Tiene exito si encuentra las diagonales de la diagonal principal
%	           hacia abajo
 diagonal1([], _, []).
 diagonal1([H|T], Length, [S|Ts]) :-
	diagonalAux([H|T], 0, Length, S),
	LengthN is Length -1,
	diagonal1(T, LengthN, Ts).


% diagonal2 :- Tiene exito si encuentra las diagonales de la diagonal principal
%   		   hacia arriba.
diagonal2(_, Counter, Length, []):- Counter = Length.
diagonal2(Sopa, Counter, Length, [S|Ts]) :-
	diagonalAux(Sopa, Counter, Length, S),
	CounterN is Counter + 1,
	diagonal2(Sopa, CounterN, Length, Ts).


% diagonalAux :- Tiene exito si encuentra la diagonal que empieza en la posicion
%                Counter de la primera lista suministrada
diagonalAux(_, Counter, Length, []):- Counter = Length.
diagonalAux([H|T], Counter, Length, [S|Ts]) :-
	nth0(Counter, H, S),
	CounterN is Counter + 1,
	diagonalAux(T, CounterN, Length, Ts).


% trasponer :- Tiene exito si traspone una sopa para buscar las palabras
% 			   horizontales
 trasponer([], []).
 trasponer([H|T], S) :-
    trasponerAux(H, [H|T], S).


% trasponerAux :- Tiene exito si llama a trasponerAux2 suministrandole al principio
%				  la sopa completa y luego las listas con las filas que devuelve
%				  dicha funcion hasta que se acaben los elementos en la lista de
%				  control (primer argunmento) y estas tienen exito
 trasponerAux([], _, []).
 trasponerAux([_|Rs], Ms, [S|Tss]) :-
        trasponerAux2(Ms, S, Ms1),
        trasponerAux(Rs, Ms1, Tss).


% trasponerAux2 :- Tiene exito si encuentra las primeras letras de cada fila.
%				   Guarda una lista con dichas letras, y en otra lista guarda
%				   las filas excluyendo la primera letra
 trasponerAux2([], [], []).
 trasponerAux2([[H|Os]|Rest], [H|T], [Os|Oss]) :-
        trasponerAux2(Rest, T, Oss).


% ofrecerMas :- Falla si el usuario escribe 'mas' por salida estandar
 ofrecerMas :-
	write('Quieres mas?  '),
	read(UserAnswer),
	UserAnswer \= mas.