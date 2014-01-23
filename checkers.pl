/*
   Proyecto 2 - Checkers
   Laboratorio de Lenguajes 1 - Grupo 9
   Autores: Marcos Campos  10-10108
            Andrea Salcedo 10-10666
*/

% Tablero inicializado.
tableroInicial(X) :- X =
/*
[[empty,black,empty,black,empty,black,empty,black],
 [black,empty,black,empty,black,empty,black,empty],
 [empty,black,empty,black,empty,black,empty,black],
 [empty,empty,empty,empty,empty,empty,empty,empty],
 [empty,empty,empty,empty,empty,empty,empty,empty],
 [white,empty,white,empty,white,empty,white,empty],
 [empty,white,empty,white,empty,white,empty,white],
 [white,empty,white,empty,white,empty,white,empty]].*/

[[empty,black,empty,black,empty,whiteKing,empty,black],
 [empty,empty,black,empty,empty,empty,empty,empty],
 [empty,black,empty,empty,empty,empty,empty,empty],
 [empty,empty,empty,empty,empty,empty,empty,empty],
 [empty,empty,empty,empty,empty,empty,empty,empty],
 [empty,empty,empty,empty,black,empty,empty,empty],
 [empty,empty,empty,empty,empty,blackKing,empty,empty],
 [blackKing,empty,blackKing,empty,empty,empty,white,empty]]. 
 
 
 
/* Inicializa el tablero. Establece que es un turno normal 
   y que el jugador actual es jugador1. */ 
inicializar :- 
      tableroInicial(X), assert(tablero(X)),
      assert(turnoNormal),
      assert(jugadorActual(jugador1)).
 
/* Convierte los nombres de las fichas a los simbolos 
   pertinentes para imprimirlos en el tablero. */
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
imprimirTablero :- 
      write('    1    2    3    4    5    6    7    8'), nl,
      tablero(X), imprimirFila(1,X),!.
                
% Imprime las filas del tablero actual.                
imprimirFila(9,_).                
imprimirFila(Cont,[H|T]) :- 
      write(Cont), write(' '), imprimirColumna(H),
      ContNuevo is Cont + 1, nl, imprimirFila(ContNuevo,T).

% Imprime las columnas del tablero actual.                            
imprimirColumna([]).                            
imprimirColumna([H|T]) :- 
      convertir(H,X),write('|'),write(X),write('| '), imprimirColumna(T).
                          
% Asocia las fichas al jugador.
asociarJugador(jugador1,black).
asociarJugador(jugador1,blackKing).
asociarJugador(jugador2,white).
asociarJugador(jugador2,whiteKing).

% Busca una ficha en el tablero, dado su posicion.
buscarPieza(X,Y,Piece) :- tablero(T), buscarFila(X,Y,T,Piece), !.
                          
% Busca la fila donde esta un ficha, dada su posicion.
buscarFila(X,1,[H|_],Piece) :- buscarColumna(X,H,Piece).
buscarFila(X,Y,[_|T],Piece) :- YNuevo is Y - 1, buscarFila(X,YNuevo,T,Piece).

% Busca la columna donde esta un ficha, dada su posicion.                               
buscarColumna(1,[H|_],H).
buscarColumna(X,[_|T],Piece) :- XNuevo is X - 1, buscarColumna(XNuevo,T,Piece).

% Verifica si la posicion dada esta en el rango del tablero.
posicionValida(X,Y) :- X >= 1, X =< 8, Y >= 1, Y =< 8.

/* Establece las fichas contrarias de una ficha dada. 
   El vacio no tiene ficha contraria. */                       
piezaContraria(black,white).
piezaContraria(black,whiteKing).
piezaContraria(blackKing,white).
piezaContraria(blackKing,whiteKing).
piezaContraria(white,black).
piezaContraria(whiteKing,black).
piezaContraria(white,blackKing).
piezaContraria(whiteKing,blackKing).

% Establece cuales fichas son reyes.
isKing(whiteKing).
isKing(blackKing).

% Establece cuales fichas no son reyes.
isNotKing(white).
isNotKing(black).

/* Dado las coordenadas Y de un movimiento, devuelve las 
   posibles posiciones que se puede mover verticalmente un peon. */
direccionVertical(Y1,Y2,TypePiece,YNuevo1,YNuevo2) :- 
      ((TypePiece = black; TypePiece = whiteKing; TypePiece = blackKing),
        Y2 > Y1, YNuevo1 is Y1 + 1, YNuevo2 is Y1 + 2);
      ((TypePiece = white; TypePiece = blackKing; TypePiece = whiteKing), 
        Y2 < Y1, YNuevo1 is Y1 - 1, YNuevo2 is Y1 - 2).

/* Dado la coordenada X de un movimiento, devuelve las 
   posibles posiciones que se puede mover horizontalmente un peon. */
direccionHorizontal(X,XDiag1,XDiag2) :- XDiag1 is X + 1, XDiag2 is X + 2.
direccionHorizontal(X,XDiag1,XDiag2) :- XDiag1 is X - 1, XDiag2 is X - 2. 

/* Verifica las posiciones diagonales de un rey. 
   Caso base donde el rey come una ficha. */ 
diagonalRey(Piece,X1,Y1,X2,Y2,PosFin) :- 
      posicionValida(X2,Y2),
      buscarPieza(X1,Y1,PieceAnt),
      piezaContraria(Piece,PieceAnt),
      buscarPieza(X2,Y2,PieceAct), 
      PieceAct = empty,
      PosFin = posRey(X1,Y1,X2,Y2),!.

% Verifica las posiciones diagonales de un rey cuando no come una ficha.
diagonalRey(_,X1,Y1,X2,Y2,PosFin) :- 
      posicionValida(X2,Y2),
      buscarPieza(X1,Y1,PieceAnt),
      PieceAnt = empty,
      buscarPieza(X2,Y2,PieceAct),
      PieceAct = empty,
      PosFin = posRey(X1,Y1,X2,Y2).

% Verifica las posiciones diagonales de un rey, paso recursivo.      
diagonalRey(Piece,X1,Y1,X2,Y2,PosFin) :- 
      X1 > X2, Y1 > Y2,
      XDiag is X2 - 1, YDiag is Y2 - 1,
      posicionValida(XDiag,YDiag),
      diagonalRey(Piece,X2,Y2,XDiag,YDiag,PosFin).

% Verifica las posiciones diagonales de un rey, paso recursivo.      
diagonalRey(Piece,X1,Y1,X2,Y2,PosFin) :- 
      X1 < X2, Y1 > Y2,
      XDiag is X2 + 1, YDiag is Y2 - 1,
      posicionValida(XDiag,YDiag),
      diagonalRey(Piece,X2,Y2,XDiag,YDiag,PosFin).

% Verifica las posiciones diagonales de un rey, paso recursivo.      
diagonalRey(Piece,X1,Y1,X2,Y2,PosFin) :- 
      X1 > X2, Y1 < Y2,
      XDiag is X2 - 1, YDiag is Y2 + 1,
      posicionValida(XDiag,YDiag),
      diagonalRey(Piece,X2,Y2,XDiag,YDiag,PosFin).

% Verifica las posiciones diagonales de un rey, paso recursivo.      
diagonalRey(Piece,X1,Y1,X2,Y2,PosFin) :- 
      X1 < X2, Y1 < Y2,
      XDiag is X2 + 1, YDiag is Y2 + 1,
      posicionValida(XDiag,YDiag),
      diagonalRey(Piece,X2,Y2,XDiag,YDiag,PosFin).

% Devuelve las posiciones diagonales de un rey.                                 
diagonalesRey(X1,Y1,X2,Y2,X3,Y3) :- 
      XInter is X1 + 1, YInter is Y1 + 1,
      posicionValida(XInter,YInter),
      buscarPieza(XInter,YInter,Piece),
      Piece = empty,
      X2 is X1 + 1, Y2 is Y1 + 1,
      X3 is X1 + 2, Y3 is Y1 + 2.

% Devuelve las posiciones diagonales de un rey.               
diagonalesRey(X1,Y1,X2,Y2,X3,Y3) :- 
      XInter is X1 + 1, YInter is Y1 - 1,
      posicionValida(XInter,YInter),
      buscarPieza(XInter,YInter,Piece),
      Piece = empty,
      X2 is X1 + 1, Y2 is Y1 - 1,
      X3 is X1 + 2, Y3 is Y1 - 2.
                                               
% Devuelve las posiciones diagonales de un rey.       
diagonalesRey(X1,Y1,X2,Y2,X3,Y3) :- 
      XInter is X1 - 1, YInter is Y1 + 1,
      posicionValida(XInter,YInter),
      buscarPieza(XInter,YInter,Piece),
      Piece = empty,
      X2 is X1 - 1, Y2 is Y1 + 1,
      X3 is X1 - 2, Y3 is Y1 + 2.
                                               
% Devuelve las posiciones diagonales de un rey.          
diagonalesRey(X1,Y1,X2,Y2,X3,Y3) :- 
      XInter is X1 - 1, YInter is Y1 - 1,
      posicionValida(XInter,YInter),
      buscarPieza(XInter,YInter,Piece),
      Piece = empty,
      X2 is X1 - 1, Y2 is Y1 - 1,
      X3 is X1 - 2, Y3 is Y1 - 2.
                                                    
/* Verifica si una ficha puede hacer el movimiento dado.
   Este caso considera un movimiento de un peon. */
jugadaValida(Piece,X1,Y1,X2,Y2) :-               
      posicionValida(X2,Y2),
      direccionVertical(Y1,Y2,Piece,YNuevo1,YNuevo2),
      direccionHorizontal(X1,XDiag1,XDiag2),
      ((XDiag1 =:= X2, YNuevo1 =:= Y2, turnoNormal,
        buscarPieza(XDiag1,YNuevo1,PieceDiag1), PieceDiag1 = empty);
      (XDiag2 =:= X2, YNuevo2 =:= Y2, ((turnoNormal,!);
       (turnoEspecial(Xspecial, Yspecial), Xspecial = X1, Yspecial = Y1)),
        buscarPieza(XDiag2,YNuevo2,PieceDiag2), PieceDiag2 = empty,   
        buscarPieza(XDiag1,YNuevo1,PieceDiag1), 
        piezaContraria(Piece,Contraria1), PieceDiag1 = Contraria1)), !.
            
/* Verifica si una ficha puede hacer el movimiento dado.
   Este caso considera un movimiento de un rey. */
jugadaValida(Piece,X1,Y1,X2,Y2) :- 
      (Piece = blackKing; Piece = whiteKing), posicionValida(X2,Y2),      
      diagonalesRey(X1,Y1,XRey2,YRey2,XRey3,YRey3),
      posicionValida(XRey2,YRey2),
      diagonalRey(Piece,XRey2,YRey2,XRey3,YRey3,PosFin),
      PosFin = posRey(XAnt,YAnt,X2,Y2),
      posicionValida(XAnt,YAnt),
      buscarPieza(XAnt,YAnt,PieceAnt),                                    
      (PieceAnt = empty; ((turnoNormal,!); 
      (turnoEspecial(Xspecial, Yspecial), Xspecial = X1, Yspecial = Y1))).

% Cambia el tablero, dado una ficha y su posicion. 
cambiarPieza(X,Y,TypePiece) :- 
      tablero(T), retract(tablero(_)),
      cambiarFila(X,Y,TypePiece,T,TNuevo),
      assert(tablero(TNuevo)).
                      
% Busca la fila del tablero a cambiar.
cambiarFila(X,1,TypePiece,[H|T],[TNuevo|T]) :- cambiarColumna(X,TypePiece,H,TNuevo).
cambiarFila(X,Y,TypePiece,[H|T],[H|TNuevo]) :- YNuevo is Y - 1,
                                               cambiarFila(X,YNuevo,TypePiece,T,TNuevo).
                      
% Cambia la ficha en la columna dada.
cambiarColumna(1,TypePiece,[_|T], [TypePiece|T]).                      
cambiarColumna(X,TypePiece,[H|T],[H|TNuevo]) :- XNuevo is X - 1,
                                                cambiarColumna(XNuevo,TypePiece,T,TNuevo).   
                                                               
% Cambia el jugador actual.
cambiarJugador :- 
      jugadorActual(X), retract(jugadorActual(_)),
      ((X = jugador1, assert(jugadorActual(jugador2)));
       (X = jugador2, assert(jugadorActual(jugador1)))).
       
/* Verifica si una jugada es valida. Si lo es, cambia la posicion de la 
   ficha. Ademas, si se come una ficha, entonces esta es eliminada del
   tablero y se agrega el predicado turnoEspecial. Si hay un turnoEspecial
   entonces el jugador actual debe jugar de nuevo y comer. */
jugada(X1,Y1,X2,Y2) :- 
      buscarPieza(X1,Y1,Piece),
      jugadorActual(Jugador),
      asociarJugador(Jugador,Piece),
      jugadaValida(Piece,X1,Y1,X2,Y2),
      cambiarPieza(X1,Y1,empty),
      cambiarPieza(X2,Y2,Piece),
      (turnoNormal; (retract(turnoEspecial(_,_)), assert(turnoNormal))),
      direccionVertical(Y1,Y2,Piece,YNuevo1,YNuevo2),
      
      % La ficha no comio, convierte la ficha en un rey si llego al final del tablero.
      ((YNuevo1 =:= Y2, ((Y2 =:= 1, Piece = white, convertirRey(X2,Y2,white));
       (Y2 =:= 8, Piece = black, cambiarPieza(X2,Y2,blackKing)); 
       Y2 =\= 1; Y2 =\= 8),cambiarJugador,turno,!); 
      
      % La ficha comio, elimina la ficha comida y el jugador juega de nuevo.
      ((YNuevo2 =:= Y2, (X1 < X2, XIntermed is X1 + 1; X1 > X2, XIntermed is X1 - 1),
      cambiarPieza(XIntermed,YNuevo1,empty),jugarDeNuevo(X2,Y2,Piece),!)); 
      
      /* La ficha es un rey. Pudo haber comido o no. Si come una ficha,
         entonces juega de nuevo. */
      (isKing(Piece), 
      direccionHorizontal(X2,XDiag,_), direccionVertical(Y2,Y1,Piece,YNuevo,_),
      ((X1 < XDiag, XDiag < X2, Y1 < YNuevo, YNuevo < Y2);
      (X1 < XDiag, XDiag < X2, Y1 > YNuevo, YNuevo > Y2);
      (X1 > XDiag, XDiag > X2, Y1 < YNuevo, YNuevo < Y2);
      (X1 > XDiag, XDiag > X2, Y1 > YNuevo, YNuevo > Y2)),
      buscarPieza(XDiag,YNuevo,PieceAnt),
      ((PieceAnt = empty, cambiarJugador, turno, !);
      (cambiarPieza(XDiag,YNuevo,empty), jugarDeNuevo(X2,Y2,Piece),!)))).
     
% La jugada dada es invalida y el jugador debe jugar de nuevo.
jugada(_,_,_,_) :- write('Jugada invalida, juega de nuevo.'), nl, turno. 

% Convierte una ficha blanca a un rey blanco.
jugarDeNuevo(X,1,white) :- convertirRey(X,1,white), cambiarJugador, turno, !.      
                           
% Convierte una ficha negra a un rey negro.
jugarDeNuevo(X,8,black) :- convertirRey(X,8,black), cambiarJugador, turno, !.
                           
% Verifica si el jugador puede comer de nuevo con la ficha que ya movio.                      
jugarDeNuevo(X,Y,Piece) :- 
      XLeft is X - 2, XRight is X + 2, YUp is Y - 2, YDown is Y + 2,
      (jugadaValida(Piece,X,Y,XLeft,YUp); jugadaValida(Piece,X,Y,XRight,YUp);
      jugadaValida(Piece,X,Y,XLeft,YDown); jugadaValida(Piece,X,Y,XRight,YDown)),
      write('Te comiste una ficha! Puedes jugar de nuevo!'), nl,
      assert(turnoEspecial(X,Y)), retract(turnoNormal), turno, !.
      
% Verifica si el jugador puede comer de nuevo con el rey que ya movio.
jugarDeNuevo(X,Y,Piece) :- 
      isKing(Piece), diagonalesRey(X,Y,X2,Y2,X3,Y3),
      diagonalRey(Piece,X2,Y2,X3,Y3,PosFin), PosFin = posRey(XAnt,YAnt,_,_),
      buscarPieza(XAnt,YAnt,PieceAnt), notEmpty(PieceAnt),
      write('Te comiste una ficha! Puedes jugar de nuevo!'), nl,
      assert(turnoEspecial(X,Y)), retract(turnoNormal), turno, !.
                            
% Cambia de turno.
jugarDeNuevo(_,_,_) :- cambiarJugador, turno.

% Turno cuando se juega contra la maquina.
turno :- jugadorActual(X), X = jugador2, tipoJuego(computadora),
         verificarTablero,imprimirTablero,         
         write('Juega '), write(X),
         jugarMaquina, !.

/* Imprime el tablero, verifica si aun hay movimientos validos o fichas y
   le pide al usuario que juegue. */
turno :- imprimirTablero, verificarTablero,
         jugadorActual(X), write('Juega '), write(X),!.

% Cuando no hay movimientos o fichas, termina el juego y calcula el ganador.
turno :- cambiarJugador, tablero(T), flatten(T,F),
         findall(_, member(black,F), LBlack),
         findall(_, member(blackKing,F), LBK),
         findall(_, member(white,F), LWhite),
         findall(_, member(whiteKing,F), LWK),
         append(LBlack,LBK,TotalBlack),
         append(LWhite,LWK,TotalWhite),
         length(TotalBlack,LenBlack),
         length(TotalWhite,LenWhite),
         nl, ganador(LenBlack,LenWhite),
         retract(tablero(_)),!.
 
/* Verifica quien es el gandaro. Cuenta el numero de fichas disponibles
   de cada jugador. */
ganador(L1,L2) :- L1 > L2, write('Ha ganado el jugador 1!'). 
ganador(L1,L2) :- L1 < L2, write('Ha ganado el jugador 2!').      
ganador(L1,L2) :- L1 =:= L2, write('Empate!').           
 
/* Empieza el juego. Inicializa el tablero y le pregunta al usuario
   si desea jugar contra la maquina o no. */
jugar :- inicializar, preguntar, write('Comenzo el juego'), nl, turno.

% Pregunta si el usuario desea jugar contra la maquina.
preguntar :- write('Desea jugar contra la maquina (s/n)? '), read(Input), 
             ((Input = s, assert(tipoJuego(computadora))); 
             (Input = n, assert(tipoJuego(humano)))),!.
             
preguntar :- preguntar.
         
% Convierte un peon a un rey.
convertirRey(X,Y,black) :- cambiarPieza(X,Y,blackKing).
convertirRey(X,Y,white) :- cambiarPieza(X,Y,whiteKing).

/* Verifica que cada jugador tiene fichas disponibles y que existan
   movimientos validos. */
verificarTablero :- existeFicha(jugador1),
                    existeFicha(jugador2),
                    findall(M,buscarMovimientos(M),Lista),
                    dividirMovimientos(Lista,ListaJug1,ListaJug2),
                    length(ListaJug1,L1), length(ListaJug2,L2),
                    ((turnoNormal, L1 > 0, L2 > 0); 
                    (turnoEspecial(_,_),((L1 >= 0, L2 > 0);(L1 > 0, L2 >= 0)))).

% Busca todos los movimientos posibles en el tablero.
buscarMovimientos(Jugada) :- tablero(T), buscarMovFila(1,1,Jugada,T).                    
                    
% Busca un movimiento en una fila del tablero.
buscarMovFila(X,Y,Jugada,[H|_]) :- buscarMovColumna(X,Y,Jugada,H). 
buscarMovFila(X,Y,Jugada,[_|T]) :- YNuevo is Y + 1, buscarMovFila(X,YNuevo,Jugada,T).

/* Busca un movimiento en una columna del tablero. Verifica si es un
   movimiento valido. Este caso considera los movimientos de los peones. */
buscarMovColumna(X,Y,Jugada,[H|_]) :- 
      notEmpty(H),
      direccionHorizontal(X,XNuevo1,XNuevo2),
      direccionHorizontal(Y,YNuevo1,YNuevo2),
      buscarPieza(X,Y,Piece),
      ((jugadaValida(Piece,X,Y,XNuevo1,YNuevo1), Jugada = movimiento(Piece,X,Y,XNuevo1,YNuevo1));
      (jugadaValida(Piece,X,Y,XNuevo1,YNuevo2), Jugada = movimiento(Piece,X,Y,XNuevo1,YNuevo2));
      (jugadaValida(Piece,X,Y,XNuevo2,YNuevo1), Jugada = movimiento(Piece,X,Y,XNuevo2,YNuevo1));
      (jugadaValida(Piece,X,Y,XNuevo2,YNuevo2), Jugada = movimiento(Piece,X,Y,XNuevo2,YNuevo2))).

% Busca un movimiento valido para un rey.
buscarMovColumna(X,Y,Jugada,[H|_]) :- 
      isKing(H),buscarPieza(X,Y,Piece),
      diagonalesRey(X,Y,X2,Y2,X3,Y3),
      diagonalRey(Piece,X2,Y2,X3,Y3,PosFin),
      PosFin = posRey(_,_,XNuevo,YNuevo),
      Jugada = movimiento(Piece,X,Y,XNuevo,YNuevo).

% Paso recursivo para buscar un movimiento valido en el tablero.                                            
buscarMovColumna(X,Y,Jugada,[_|T]) :- XNuevo is X + 1, buscarMovColumna(XNuevo,Y,Jugada,T).
                        
% Verifica si un jugador dado tiene fichas.
existeFicha(Jugador) :- tablero(T), existeFichaFila(T,Jugador).
existeFichaFila([H|_],Jugador) :- asociarJugador(Jugador,Piece), member(Piece,H),!.
existeFichaFila([_|T],Jugador) :- existeFichaFila(T,Jugador).

/* Busca todos los movimientos posibles que puede efectuar la maquina
   y elige un movimiento random. */
jugarMaquina :- findall(M,buscarMovimientos(M),Lista), dividirMovimientos(Lista,_,ListaJug2),
                random_member(Move,ListaJug2), Move = movimiento(_,X1,Y1,X2,Y2),
                nl, nl, write('La computadora movio: '), 
                write(Move), nl, nl, jugada(X1,Y1,X2,Y2),!.
                

% Divide todos los movimientos posibles de los dos jugadores en dos listas.
dividirMovimientos(L,ListaJug1,ListaJug2) :- dividirMovHelper(L,[],[],ListaJug1,ListaJug2).

% Predicado auxiliar para dividir los movimientos de los jugadores.
dividirMovHelper([],ListaJug1,ListaJug2,ListaJug1,ListaJug2).

/* Si el movimiento actual le pertenece al jugador 1 entonces se agrega a
   la primera lista. */
dividirMovHelper([H|T],Acc1,Acc2,ListaJug1,ListaJug2) :- 
      H = movimiento(Piece,_,_,_,_),
      asociarJugador(jugador1,Piece),
      Acc1Nuevo = [H|Acc1],
      dividirMovHelper(T,Acc1Nuevo,Acc2,ListaJug1,ListaJug2),!.

/* Si el movimiento actual le pertenece al jugador 2 entonces se agrega a
   la segunda lista. */
dividirMovHelper([H|T],Acc1,Acc2,ListaJug1,ListaJug2) :- 
      H = movimiento(Piece,_,_,_,_),
      asociarJugador(jugador2,Piece),
      Acc2Nuevo = [H|Acc2],
      dividirMovHelper(T,Acc1,Acc2Nuevo,ListaJug1,ListaJug2),!.


                  