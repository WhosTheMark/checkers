

% Tablero inicializado

tableroInicial(X) :- X =
[[empty,black,empty,black,empty,black,empty,black],
 [black,empty,black,empty,black,empty,black,empty],
 [empty,black,empty,black,empty,black,empty,black],

 [empty,empty,empty,empty,empty,empty,empty,empty],
 [empty,empty,empty,empty,empty,empty,empty,empty],

 [white,empty,white,empty,white,empty,white,empty],
 [empty,white,empty,white,empty,white,empty,white],
 [white,empty,white,empty,white,empty,white,empty]].

% Inicializar tablero dinamicamente 
 inicializar :- tableroInicial(X), assert(tablero(X)).
 
 convertir(empty, '  ').
 convertir(black, '< ').
 convertir(blackKing, '<<').
 convertir(white, '> ').
 convertir(whiteKing, '>>').
 
 imprimirTablero :- write('    1    2    3    4    5    6    7    8'),
                    nl,
                    tablero(X),
                    imprimirFila(1,X),!.
                
                
imprimirFila(9,_).                
imprimirFila(Cont,[H|T]) :- write(Cont), write(' '),
                            imprimirColumna(H),
                            ContNuevo is Cont + 1,
                            nl,
                            imprimirFila(ContNuevo,T).
                            
imprimirColumna([]).                            
imprimirColumna([H|T]) :- convertir(H,X),
                          write('|'),write(X),write('| '),
                          imprimirColumna(T).
                          

asociarJugador(jugador1,black).
asociarJugador(jugador1,blackKing).
asociarJugador(jugador2,white).
asociarJugador(jugador2,whiteKing).

buscarPieza(X,Y,Piece) :- tablero(T),
                          buscarFila(X,Y,T,Piece),!.
                          
buscarFila(X,1,[H|_],Piece) :- buscarColumna(X,H,Piece).
buscarFila(X,Y,[_|T],Piece) :- YNuevo is Y - 1,
                               buscarFila(X,YNuevo,T,Piece).
                               
buscarColumna(1,[H|_],H).
buscarColumna(X,[_|T],Piece) :- XNuevo is X - 1,
                                buscarColumna(XNuevo,T,Piece).

posicionValida(X,Y) :- X >= 1, X =< 8,
                       Y >= 1, Y =< 8.
                       
piezaContraria(black,white).
piezaContraria(black,whiteKing).
piezaContraria(blackKing,white).
piezaContraria(blackKing,whiteKing).
piezaContraria(X,Y) :- piezaContraria(Y,X),!.

isKing(whiteKing).
isKing(blackKing).

isNotKing(white).
isNotKing(black).

direccion(Y1,Y2,TypePiece,YNuevo1,YNuevo2) :- 
                      ((TypePiece = black; TypePiece = whiteKing) -> Y2 > Y1, YNuevo1 is Y1 + 1, YNuevo2 is Y1 + 2);
                      ((TypePiece = white; TypePiece = blackKing) -> Y2 < Y1, YNuevo1 is Y1 - 1, YNuevo2 is Y1 - 2).
                      
% Ver si la pieza se puede mover al lugar indicado. Ya fue verificado que es el jugador correcto.                                
jugadaValida(X1,Y1,X2,Y2) :-
                     buscarPieza(X1,Y1,Piece),
                     posicionValida(X2,Y2),
                     direccion(Y1,Y2,Piece,YNuevo1,YNuevo2),
                     ((XDiagL1 is X1 - 1, XDiagL1 =:= X2, YNuevo1 =:= Y2, 
                           buscarPieza(XDiagL1,YNuevo1,PieceDiagL1), PieceDiagL1 = empty);
                     (XDiagR1 is X1 + 1, XDiagR1 =:= X2, YNuevo1 =:= Y2, 
                           buscarPieza(XDiagR1,YNuevo1,PieceDiagR1), PieceDiagR1 = empty);
                     (XDiagL2 is X1 - 2, XDiagL2 =:= X2, YNuevo2 =:= Y2, 
                           buscarPieza(XDiagL2,YNuevo2,PieceDiagL2), PieceDiagL2 = empty, XDiagL1 is X1 - 1,  
                           buscarPieza(XDiagL1,YNuevo1,PieceDiagL1), piezaContraria(Piece,Contraria1), PieceDiagL1 = Contraria1);
                     (XDiagR2 is X1 + 2, XDiagR2 =:= X2, YNuevo2 =:= Y2, 
                           buscarPieza(XDiagR2,YNuevo2,PieceDiagR2), PieceDiagR2 = empty, XDiagR1 is X1 + 1,  
                           buscarPieza(XDiagR1,YNuevo1,PieceDiagR1), piezaContraria(Piece,Contraria2), PieceDiagR1 = Contraria2)),
                     !.
                     
                     
cambiarPieza(X,Y,TypePiece) :- tablero(T),
                               retract(tablero(_)),
                               cambiarFila(X,Y,TypePiece,T,TNuevo),
                               assert(tablero(TNuevo)),!.
                      
cambiarFila(X,1,TypePiece,[H|T],[TNuevo|T]) :- cambiarColumna(X,TypePiece,H,TNuevo).                     
cambiarFila(X,Y,TypePiece,[H|T],[H|TNuevo]) :- YNuevo is Y - 1,
                                           cambiarFila(X,YNuevo,TypePiece,T,TNuevo).
                      
cambiarColumna(1,TypePiece,[_|T], [TypePiece|T]).                      
cambiarColumna(X,TypePiece,[H|T],[H|TNuevo]) :- XNuevo is X - 1,
                                                cambiarColumna(XNuevo,TypePiece,T,TNuevo).
                      
                      














