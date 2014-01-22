/*
   Proyecto 2 - Checkers
   Grupo 9
   Autores: Marcos Campos  10-10108
            Andrea Salcedo 10-10666
*/

% Tablero inicializado 

tableroInicial(X) :- X =
[[empty,black,empty,black,empty,black,empty,black],
 [black,empty,black,empty,black,empty,black,empty],
 [empty,black,empty,black,empty,black,empty,black],

 [empty,empty,empty,empty,empty,empty,empty,empty],
 [empty,empty,empty,empty,empty,white,empty,empty],

 [white,empty,white,empty,empty,empty,white,empty],

 %[empty,white,empty,white,empty,white,empty,white],
 %[white,empty,white,empty,white,empty,white,empty]].
 [empty,black,empty,white,empty,white,empty,white],
 [empty,empty,empty,empty,empty,empty,empty,empty]].
 
/* 
  [[empty,empty,empty,empty,empty,empty,empty,empty],
   [empty,empty,empty,empty,empty,empty,empty,black],
   [black,empty,black,empty,black,empty,empty,empty],
   [empty,black,empty,black,empty,black,empty,black],
   [white,empty,white,empty,white,empty,white,empty],
   [empty,white,empty,white,empty,white,empty,white],
   [empty,empty,empty,empty,empty,empty,empty,empty],
   [empty,empty,empty,empty,empty,empty,empty,empty]].
 */
 
% Inicializa el tablero. establece que es un turno normal y que el jugador actual es jugador1. 
 inicializar :- tableroInicial(X), assert(tablero(X)),
                assert(turnoNormal),
                assert(jugadorActual(jugador1)).
 
 % Convierte los nombres de las fichas a los simbolos pertinentes para imprimirlos en el tablero.
 convertir(empty, '  ').
 convertir(black, '< ').
 convertir(blackKing, '<<').
 convertir(white, '> ').
 convertir(whiteKing, '>>').
 
 % Dice cuales fichas no son vacias.
 notEmpty(white).
 notEmpty(black).
 notEmpty(whiteKing).
 notEmpty(blackKing).
 
 % Imprime el tablero actual.
 imprimirTablero :- write('    1    2    3    4    5    6    7    8'),
                    nl,
                    tablero(X),
                    imprimirFila(1,X),!.
                
% Imprime las filas del tablero actual.                
imprimirFila(9,_).                
imprimirFila(Cont,[H|T]) :- write(Cont), write(' '),
                            imprimirColumna(H),
                            ContNuevo is Cont + 1,
                            nl,
                            imprimirFila(ContNuevo,T).

% Imprime las columnas del tablero actual.                            
imprimirColumna([]).                            
imprimirColumna([H|T]) :- convertir(H,X),
                          write('|'),write(X),write('| '),
                          imprimirColumna(T).
                          
% Asocia las fichas al jugador.
asociarJugador(jugador1,black).
asociarJugador(jugador1,blackKing).
asociarJugador(jugador2,white).
asociarJugador(jugador2,whiteKing).

% Busca una ficha en el tablero, dado su posicion.
buscarPieza(X,Y,Piece) :- tablero(T),
                          buscarFila(X,Y,T,Piece),!.
                          
% Busca la fila donde esta un ficha, dada su posicion.
buscarFila(X,1,[H|_],Piece) :- buscarColumna(X,H,Piece).
buscarFila(X,Y,[_|T],Piece) :- YNuevo is Y - 1,
                               buscarFila(X,YNuevo,T,Piece).

% Busca la columna donde esta un ficha, dada su posicion.                               
buscarColumna(1,[H|_],H).
buscarColumna(X,[_|T],Piece) :- XNuevo is X - 1,
                                buscarColumna(XNuevo,T,Piece).

% Verifica si la posicion dada esta en el rango del tablero.
posicionValida(X,Y) :- X >= 1, X =< 8,
                       Y >= 1, Y =< 8.

% Establece las fichas contrarias de una ficha dada. La ficha vacia no tiene contraria.                       
piezaContraria(black,white).
piezaContraria(black,whiteKing).
piezaContraria(blackKing,white).
piezaContraria(blackKing,whiteKing).
piezaContraria(empty,_) :- !,fail.
piezaContraria(X,Y) :- piezaContraria(Y,X),!.

% Establece cuales fichas son reyes.
isKing(whiteKing).
isKing(blackKing).

% Establece cuales fichas no son reyes.
isNotKing(white).
isNotKing(black).

% Dado las coordenadas Y de un movimiento, devuelve las posibles posiciones que se puede mover verticalmente un peon.
direccionVertical(Y1,Y2,TypePiece,YNuevo1,YNuevo2) :- 
                      ((TypePiece = black; TypePiece = whiteKing; TypePiece = blackKing), Y2 > Y1, YNuevo1 is Y1 + 1, YNuevo2 is Y1 + 2);
                      ((TypePiece = white; TypePiece = blackKing; TypePiece = whiteKing), Y2 < Y1, YNuevo1 is Y1 - 1, YNuevo2 is Y1 - 2).
                 
% Dado la coordenada X de un movimiento, devuelve las posibles posiciones que se puede mover horizontalmente un peon.
direccionHorizontal(X,XDiag1,XDiag2) :- XDiag1 is X + 1, XDiag2 is X + 2.
direccionHorizontal(X,XDiag1,XDiag2) :- XDiag1 is X - 1, XDiag2 is X - 2. 


diagonalRey(Piece,X1,Y1,X2,Y2,PosFin) :- posicionValida(X2,Y2),
                                         buscarPieza(X1,Y1,PieceAnt),
                                         piezaContraria(Piece,PieceAnt),
                                         buscarPieza(X2,Y2,PieceAct),
                                         PieceAct = empty,
                                         PosFin = posRey(X1,Y1,X2,Y2),!.

                                  
diagonalRey(_,X1,Y1,X2,Y2,PosFin) :- posicionValida(X2,Y2),
                                     buscarPieza(X1,Y1,PieceAnt),
                                     PieceAnt = empty,
                                     buscarPieza(X2,Y2,PieceAct),
                                     PieceAct = empty,
                                     PosFin = posRey(X1,Y1,X2,Y2).
                                    
diagonalRey(Piece,X1,Y1,X2,Y2,PosFin) :- X1 > X2, Y1 > Y2,
                                         XDiag is X2 - 1, YDiag is Y2 - 1,
                                         posicionValida(XDiag,YDiag),
                                         diagonalRey(Piece,X2,Y2,XDiag,YDiag,PosFin),!.
                                  
diagonalRey(Piece,X1,Y1,X2,Y2,PosFin) :- X1 < X2, Y1 > Y2,
                                         XDiag is X2 + 1, YDiag is Y2 - 1,
                                         posicionValida(XDiag,YDiag),
                                         diagonalRey(Piece,X2,Y2,XDiag,YDiag,PosFin),!.
                                  
diagonalRey(Piece,X1,Y1,X2,Y2,PosFin) :- X1 > X2, Y1 < Y2,
                                         XDiag is X2 - 1, YDiag is Y2 + 1,
                                         posicionValida(XDiag,YDiag),
                                         diagonalRey(Piece,X2,Y2,XDiag,YDiag,PosFin),!.

diagonalRey(Piece,X1,Y1,X2,Y2,PosFin) :- X1 < X2, Y1 < Y2,
                                         XDiag is X2 + 1, YDiag is Y2 + 1,
                                         posicionValida(XDiag,YDiag),
                                         diagonalRey(Piece,X2,Y2,XDiag,YDiag,PosFin),!.

                                  
diagonalesRey(X1,Y1,X2,Y2,X3,Y3) :- XInter is X1 + 1, YInter is Y1 + 1,
                                    buscarPieza(XInter,YInter,Piece),
                                    Piece = empty,
                                    X2 is X1 + 2, Y2 is Y1 + 2,
                                    X3 is X1 + 3, Y3 is Y1 + 3.
                                           
diagonalesRey(X1,Y1,X2,Y2,X3,Y3) :- XInter is X1 + 1, YInter is Y1 - 1,
                                    buscarPieza(XInter,YInter,Piece),
                                    Piece = empty,
                                    X2 is X1 + 2, Y2 is Y1 - 2,
                                    X3 is X1 + 3, Y3 is Y1 - 3.
                                                                                     
diagonalesRey(X1,Y1,X2,Y2,X3,Y3) :- XInter is X1 - 1, YInter is Y1 + 1,
                                    buscarPieza(XInter,YInter,Piece),
                                    Piece = empty,
                                    X2 is X1 - 2, Y2 is Y1 + 2,
                                    X3 is X1 - 3, Y3 is Y1 + 3.
                                                                                     
diagonalesRey(X1,Y1,X2,Y2,X3,Y3) :- XInter is X1 - 1, YInter is Y1 - 1,
                                    buscarPieza(XInter,YInter,Piece),
                                    Piece = empty,
                                    X2 is X1 - 2, Y2 is Y1 - 2,
                                    X3 is X1 - 3, Y3 is Y1 - 3.
                                          
                                          
% Ver si la pieza se puede mover al lugar indicado. Ya fue verificado que es el jugador correcto.                                
jugadaValida(Piece,X1,Y1,X2,Y2) :-
                     
                     posicionValida(X2,Y2),
                     direccionVertical(Y1,Y2,Piece,YNuevo1,YNuevo2),
                     direccionHorizontal(X1,XDiag1,XDiag2),
                     ((XDiag1 =:= X2, YNuevo1 =:= Y2, turnoNormal,
                           buscarPieza(XDiag1,YNuevo1,PieceDiag1), PieceDiag1 = empty);
                     (XDiag2 =:= X2, YNuevo2 =:= Y2, ((turnoNormal,!); %TURNO NORMAL, NO ESTA FIJA SU POSICION TODAVIA. PRIMERA VEZ QUE COME........
                       (turnoEspecial(Xspecial, Yspecial), Xspecial = X1, Yspecial = Y1)),
                           buscarPieza(XDiag2,YNuevo2,PieceDiag2), PieceDiag2 = empty,   
                           buscarPieza(XDiag1,YNuevo1,PieceDiag1), piezaContraria(Piece,Contraria1), PieceDiag1 = Contraria1)),
                     !.
                     
jugadaValida(Piece,X1,Y1,X2,Y2) :- (Piece = blackKing; Piece = whiteKing),
                                   diagonalesRey(X1,Y1,XRey2,YRey2,XRey3,YRey3),
                                   diagonalRey(Piece,XRey2,YRey2,XRey3,YRey3,PosFin),
                                   PosFin = posRey(XAnt,YAnt,X2,Y2),
                                   buscarPieza(XAnt,YAnt,PieceAnt),                                    
                                   (PieceAnt = empty; ((turnoNormal,!); 
                                   (turnoEspecial(Xspecial, Yspecial), Xspecial = X1, Yspecial = Y1))).
                     
                     
cambiarPieza(X,Y,TypePiece) :- tablero(T),
                               retract(tablero(_)),
                               cambiarFila(X,Y,TypePiece,T,TNuevo),
                               assert(tablero(TNuevo)).
                      
cambiarFila(X,1,TypePiece,[H|T],[TNuevo|T]) :- cambiarColumna(X,TypePiece,H,TNuevo).                     
cambiarFila(X,Y,TypePiece,[H|T],[H|TNuevo]) :- YNuevo is Y - 1,
                                           cambiarFila(X,YNuevo,TypePiece,T,TNuevo).
                      
cambiarColumna(1,TypePiece,[_|T], [TypePiece|T]).                      
cambiarColumna(X,TypePiece,[H|T],[H|TNuevo]) :- XNuevo is X - 1,
                                                cambiarColumna(XNuevo,TypePiece,T,TNuevo).
                                                
                       
                       
cambiarJugador :- jugadorActual(X),
                  retract(jugadorActual(_)),
                  ((X = jugador1, assert(jugadorActual(jugador2)));
                  (X = jugador2, assert(jugadorActual(jugador1)))).
                  
                  


jugada(X1,Y1,X2,Y2) :- buscarPieza(X1,Y1,Piece),
                       jugadorActual(Jugador),
                       asociarJugador(Jugador,Piece),
                       jugadaValida(Piece,X1,Y1,X2,Y2),
                       cambiarPieza(X1,Y1,empty),
                       cambiarPieza(X2,Y2,Piece),
                       (turnoNormal; (retract(turnoEspecial(_,_)), assert(turnoNormal))),
                       direccionVertical(Y1,Y2,Piece,YNuevo1,YNuevo2),
                       ((YNuevo1 =:= Y2,
                       ((Y2 =:= 1, Piece = white, convertirRey(X2,Y2,white));
                       (Y2 =:= 8, Piece = black, cambiarPieza(X2,Y2,blackKing)); Y2 =\= 1; Y2 =\= 8),
                       cambiarJugador, turno,!); 
                       
                       ((YNuevo2 =:= Y2,  
                       (X1 < X2, XIntermed is X1 + 1; X1 > X2, XIntermed is X1 - 1),
                       cambiarPieza(XIntermed,YNuevo1,empty),jugarDeNuevo(X2,Y2,Piece),!)); 
                       
                       (isKing(Piece), 
                       direccionHorizontal(X2,XDiag,_),
                       direccionVertical(Y2,Y1,Piece,YNuevo,_),
                       ((X1 < XDiag, XDiag < X2, Y1 < YNuevo, YNuevo < Y2);
                       (X1 < XDiag, XDiag < X2, Y1 > YNuevo, YNuevo > Y2);
                       (X1 > XDiag, XDiag > X2, Y1 < YNuevo, YNuevo < Y2);
                       (X1 > XDiag, XDiag > X2, Y1 > YNuevo, YNuevo > Y2)),
                       buscarPieza(XDiag,YNuevo,PieceAnt),
                       ((PieceAnt = empty, cambiarJugador, turno, !);
                       (cambiarPieza(XDiag,YNuevo,empty), jugarDeNuevo(X2,Y2,Piece),!)))).
                      
jugada(_,_,_,_) :- write('Jugada invalida, juega de nuevo.'),
                   nl,
                   turno. 


                   
jugarDeNuevo(X,1,white) :- convertirRey(X,1,white),
                           cambiarJugador,
                           turno, !.      
                           
jugarDeNuevo(X,8,black) :- convertirRey(X,8,black),
                           cambiarJugador,
                           turno, !.
                           
                      
jugarDeNuevo(X,Y,Piece) :- XLeft is X - 2,
                           XRight is X + 2,
                           YUp is Y - 2,
                           YDown is Y + 2,
                           (jugadaValida(Piece,X,Y,XLeft,YUp); 
                            jugadaValida(Piece,X,Y,XRight,YUp);
                            jugadaValida(Piece,X,Y,XLeft,YDown);
                            jugadaValida(Piece,X,Y,XRight,YDown)),
                            write('Te comiste una ficha! Puedes jugar de nuevo!'),
                            nl,
                            assert(turnoEspecial(X,Y)),
                            retract(turnoNormal),
                            turno,
                            !.
                            
jugarDeNuevo(X,Y,Piece) :- isKing(Piece),
                           diagonalesRey(X,Y,X2,Y2,X3,Y3),
                           diagonalRey(Piece,X2,Y2,X3,Y3,PosFin),                           
                           PosFin = posRey(XAnt,YAnt,_,_),
                           buscarPieza(XAnt,YAnt,PieceAnt),
                           notEmpty(PieceAnt),
                           write('Te comiste una ficha! Puedes jugar de nuevo!'),
                           nl,
                           assert(turnoEspecial(X,Y)),
                           retract(turnoNormal),
                           turno,
                           !.
                            
jugarDeNuevo(_,_,_) :- cambiarJugador, turno.

turno :- jugadorActual(X),
         X = jugador2,
         tipoJuego(computadora),
         imprimirTablero,
         verificarTablero,
         write('Juega '), write(X),
         jugarMaquina,!.

turno :- imprimirTablero,
         verificarTablero,
         jugadorActual(X),
         write('Juega '), write(X),!.

turno :- cambiarJugador,
         tablero(T),
         flatten(T,F),
         findall(_, member(black,F), LBlack),
         findall(_, member(blackKing,F), LBK),
         findall(_, member(white,F), LWhite),
         findall(_, member(whiteKing,F), LWK),
         append(LBlack,LBK,TotalBlack),
         append(LWhite,LWK,TotalWhite),
         length(TotalBlack,LenBlack),
         length(TotalWhite,LenWhite),
         ganador(LenBlack,LenWhite),
         retract(tablero(_)),!.
 
ganador(L1,L2) :- L1 > L2, write('Ha ganado el jugador 1!'). 
ganador(L1,L2) :- L1 < L2, write('Ha ganado el jugador 2!').      
ganador(L1,L2) :- L1 =:= L2, write('Empate!').           
         
jugar :- inicializar,
         preguntar,
         write('Comenzo el juego'),
         nl,
         turno.

preguntar :- write('Desea jugar contra la maquina (S/N)? '),
             read(Input),        
             ((Input = 'S', assert(tipoJuego(computadora))); 
             (Input = 'N', assert(tipoJuego(humano)))),!.
             
preguntar :- preguntar.
         
convertirRey(X,Y,black) :- cambiarPieza(X,Y,blackKing).
convertirRey(X,Y,white) :- cambiarPieza(X,Y,whiteKing).




verificarTablero :- existeFicha(jugador1),
                    existeFicha(jugador2).
                    buscarMovimientos(_).

buscarMovimientos(Jugada) :- tablero(T),
                             buscarMovFila(1,1,Jugada,T).                    
                    
buscarMovFila(X,Y,Jugada,[H|_]) :- buscarMovColumna(X,Y,Jugada,H). 
buscarMovFila(X,Y,Jugada,[_|T]) :- YNuevo is Y + 1,                                
                                   buscarMovFila(X,YNuevo,Jugada,T).

buscarMovColumna(X,Y,Jugada,[H|_]) :- notEmpty(H),
                                      direccionHorizontal(X,XNuevo1,XNuevo2),
                                      direccionHorizontal(Y,YNuevo1,YNuevo2),
                                      buscarPieza(X,Y,Piece),
                                      ((jugadaValida(Piece,X,Y,XNuevo1,YNuevo1), Jugada = movimiento(Piece,X,Y,XNuevo1,YNuevo1));
                                      (jugadaValida(Piece,X,Y,XNuevo1,YNuevo2), Jugada = movimiento(Piece,X,Y,XNuevo1,YNuevo2));
                                      (jugadaValida(Piece,X,Y,XNuevo2,YNuevo1), Jugada = movimiento(Piece,X,Y,XNuevo2,YNuevo1));
                                      (jugadaValida(Piece,X,Y,XNuevo2,YNuevo2), Jugada = movimiento(Piece,X,Y,XNuevo2,YNuevo2))).

buscarMovColumna(X,Y,Jugada,[H|_]) :- isKing(H), buscarPieza(X,Y,Piece),
                                      diagonalesRey(X,Y,X2,Y2,X3,Y3),
                                      diagonalRey(Piece,X2,Y2,X3,Y3,PosFin),
                                      PosFin = posRey(_,_,XNuevo,YNuevo),
                                      Jugada = movimiento(Piece,X,Y,XNuevo,YNuevo).

                                             
buscarMovColumna(X,Y,Jugada,[_|T]) :- XNuevo is X + 1,
                                      buscarMovColumna(XNuevo,Y,Jugada,T).
                        
existeFicha(Jugador) :- tablero(T),
                        existeFichaFila(T,Jugador).

existeFichaFila([H|_],Jugador) :- asociarJugador(Jugador,Piece),
                                  member(Piece,H).
existeFichaFila([_|T],Jugador) :- existeFichaFila(T,Jugador).


jugarMaquina :- findall(M,buscarMovimientos(M),Lista),
                dividirMovHelper(Lista,_,ListaJug2),
                random_member(Move,ListaJug2),
                Move = movimiento(Piece,X1,Y1,X2,Y2),
                nl, nl, write('La computadora movio: '), write(Move),nl,
                jugada(X1,Y1,X2,Y2),!.
                


dividirMovimientos(L,ListaJug1,ListaJug2) :- dividirMovHelper(L,[],[],ListaJug1,ListaJug2).

dividirMovHelper([],ListaJug1,ListaJug2,ListaJug1,ListaJug2).
dividirMovHelper([H|T],Acc1,Acc2,ListaJug1,ListaJug2) :- H = movimiento(Piece,_,_,_,_),
                                                         asociarJugador(jugador1,Piece),
                                                         Acc1Nuevo = [H|Acc1],
                                                         dividirMovHelper(T,Acc1Nuevo,Acc2,ListaJug1,ListaJug2),!.
dividirMovHelper([H|T],Acc1,Acc2,ListaJug1,ListaJug2) :- H = movimiento(Piece,_,_,_,_),
                                                         asociarJugador(jugador2,Piece),
                                                         Acc2Nuevo = [H|Acc2],
                                                         dividirMovHelper(T,Acc1,Acc2Nuevo,ListaJug1,ListaJug2),!.


?- jugar.
?- jugada(2,7,3,8).




                      









