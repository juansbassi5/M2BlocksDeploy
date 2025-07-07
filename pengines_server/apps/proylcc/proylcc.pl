:- module(proylcc,
    [ randomBlock/2,
      shoot/6,
      score/1 
    ]).

:- discontiguous merge_cluster/8.
:- discontiguous empty_cell/1.


score(_).
/*
    Constante para la celda vacia
*/
empty_cell('-').  

/*--------------------------------------------------------------------
  AUX: posiciones y utilidades sobre la grilla
--------------------------------------------------------------------*/

/*
    index(+Fila,+Col,+NCols,-Index)
    Calcula las coordenadas Fila y Columna en un indice para acceder a 
    elementos de la lista que representa la grilla.
    Row: Numero de la fila 
    Col: Numero de la columna
    NCols: Numero de columnas de la grilla
    Index: Indice correspondiente a la celda (Fila,Col) en la grilla.
*/
index(Row,Col,NCols,Index) :- Index is (Row-1)*NCols + Col.

/*
    dims(+Grid,+NCols,-NRows)
    Divide la longitud de la lista por el numero de columnas.
    Grid: Lista que representa la grilla
    NCols: Numero de columnas de la grilla
    NRows: Numero calculado de filas de la grilla
*/
dims(Grid,NCols,NRows) :- length(Grid,L), NRows is L//NCols.

/*
    nth1_update(+Index,+List,+Value,-NewList)
    Actualiza el elemento en la posicion Index de la lista con 
    Value, generando una nueva lista NewList.
    Index: Posicion del elemento a actualizar
    List: Lista original
    Value: Valor a insertar en la posicion Index
    NewList: Nueva lista con el valor actualizado
*/
nth1_update(1,[_|Xs],V,[V|Xs]).
nth1_update(N,[X|Xs],V,[X|Ys]) :- N>1, N1 is N-1, nth1_update(N1,Xs,V,Ys).

/*
    get_cell(+Index,+Grid,-Val)
    Obtiene el valor de la celda en la posicion Index de la grilla.
    Index: Posicion de la celda 
    Grid: Lista que representa la grilla
    Val: Valor almacenado en la celda en la posicion Index
*/
get_cell(Index,Grid,Val) :- nth1(Index,Grid,Val).

%% adjacent(+Index,+NCols,+NRows,-IndexAdj)
/*
    adjacent(+Index,+NCols,+NRows,-IndexAdj)
    Encuentra los indices de las celdas vecinas (arriba, abajo, izquierda, derecha)
    de la celda en la posicion Index.
    Index: Posicion de la celda
    NCols: Numero de columnas de la grilla
    NRows: Numero de filas de la grilla
    IndexAdj: Indice de una celda vecina
*/
adjacent(Index,NCols,NRows,I2) :-
    (   IdxUp is Index-NCols, IdxUp >= 1, I2 = IdxUp
    ;   IdxDown is Index+NCols, IdxDown =< NCols*NRows, I2 = IdxDown
    ;   IdxLeft is Index-1, Index mod NCols =\= 1, I2 = IdxLeft
    ;   IdxRight is Index+1, Index mod NCols =\= 0, I2 = IdxRight
    ).

/*--------------------------------------------------------------------
  1. Colocar el bloque en la columna 
--------------------------------------------------------------------*/

/*
    drop_block(+Block,+Col,+Grid,+NCols,-NewGrid,-Row)
    Coloca un bloque en la columna Col de la grilla Grid.
    Block: Valor del bloque a colocar
    Col: Numero de la columna donde se coloca el bloque
    Grid: Lista que representa la grilla
    NCols: Numero de columnas de la grilla
    NewGrid: Nueva grilla con el bloque colocado
    Row: Fila donde se coloca el bloque
*/
drop_block(Block,Col,Grid,NCols,NewGrid,Row) :-
    dims(Grid,NCols,NRows),
    place_from_top(Block,Col,1,NRows,Grid,NCols,NewGrid,Row).

/*
    place_from_top(+Block,+Col,+Row,+NRows,+Grid,+NCols,-NewGrid,-RowPlaced)
    Coloca un bloque en la columna Col, comenzando desde la fila Row hasta NRows.
    Block: Valor del bloque a colocar
    Col: Numero de la columna donde se coloca el bloque
    Row: Fila inicial para intentar colocar el bloque
    NRows: Numero de filas de la grilla
    Grid: Lista que representa la grilla
    NCols: Numero de columnas de la grilla
    NewGrid: Nueva grilla con el bloque colocado
    RowPlaced: Fila donde se colocó el bloque
*/
place_from_top(Block,Col,Row,NRows,Grid,NCols,NewGrid,Row) :-
    Row =< NRows,
    index(Row,Col,NCols,Index),
    empty_cell(Empty),
    get_cell(Index,Grid,Empty), !,
    nth1_update(Index,Grid,Block,NewGrid).
place_from_top(Block,Col,Row,NRows,Grid,NCols,NewGrid,RowPlaced) :-
    Row < NRows,
    Row1 is Row+1,
    place_from_top(Block,Col,Row1,NRows,Grid,NCols,NewGrid,RowPlaced).
place_from_top(_,_,Row,NRows,_,_,_,_) :-
    Row > NRows,
    throw(error(column_full)).

/*--------------------------------------------------------------------
  2. Detectar grupos y fusionar
--------------------------------------------------------------------*/
/*
    cluster_same(+Index,+Grid,+NCols,+NRows,+Val,-Cluster)
    Encuentra un grupo de celdas adyacentes con el mismo valor Val, comenzando desde Index.
    Index: Posicion inicial en la grilla
    Grid: Lista que representa la grilla
    NCols: Numero de columnas de la grilla
    NRows: Numero de filas de la grilla
    Val: Valor a buscar en las celdas adyacentes
    Cluster: Lista de indices de las celdas que forman el grupo
*/
cluster_same(Index,Grid,NCols,NRows,Val,Cluster) :-
    get_cell(Index,Grid,Val),
    bfs_same([Index],[],Grid,NCols,NRows,Val,Cluster).

/*
    bfs_same(+Front,+Visited,+Grid,+NCols,+NRows,+Val,-Cluster)
    Realiza una búsqueda en anchura para encontrar todas las celdas adyacentes con el mismo valor.
    Front: Lista de indices a explorar
    Visited: Lista de indices ya visitados
    Grid: Lista que representa la grilla
    NCols: Numero de columnas de la grilla
    NRows: Numero de filas de la grilla
    Val: Valor a buscar en las celdas adyacentes
    Cluster: Lista de indices de las celdas que forman el grupo encontrado
*/
bfs_same([],Visited,_,_,_,_,Visited).
bfs_same([I|Rest],Visited,Grid,NCols,NRows,Val,Cluster) :-
    (   member(I,Visited)
    ->  bfs_same(Rest,Visited,Grid,NCols,NRows,Val,Cluster)
    ;   findall(V,(adjacent(I,NCols,NRows,V), get_cell(V,Grid,Val)),Vs),
        append(Rest,Vs,Front),
        bfs_same(Front,[I|Visited],Grid,NCols,NRows,Val,Cluster)
    ).

/*
    merge_cluster(+Cluster,+Index,+Grid,+Val,+NCols,-GridFinal,-MergedVal,-IdxResult)
    Fusiona un grupo de celdas adyacentes con el mismo valor.
    Cluster: Lista de indices de las celdas que forman el grupo
    Index: Posicion del primer elemento del grupo en la grilla
    Grid: Lista que representa la grilla
    Val: Valor de las celdas a fusionar
    NCols: Numero de columnas de la grilla
    GridFinal: Nueva grilla con el grupo fusionado
    MergedVal: Valor resultante de la fusión
    IdxResult: Indice del bloque fusionado en la nueva grilla
*/
merge_cluster(Cluster,Index,Grid,Val,NCols,GridFinal,MergedVal,IdxResult) :-
    length(Cluster,Size), Size>=2, !,
    merged_value(Val,Size,MergedVal),
    empty_cells(Cluster,Grid,Temp1),
    nth1_update(Index,Temp1,MergedVal,Temp2),
    bubble_up(Index,Temp2,NCols,Temp3,IdxAfter),
    compact_grid_up(Temp3,NCols,GridFinal),
    col_of_idx(IdxAfter,NCols,Col),
    find_block_in_column(GridFinal,NCols,Col,MergedVal,IdxResult).
merge_cluster(_,Index,Grid,Val,_,Grid,Val,Index).
merged_value(V,Size,Out) :- Exp is Size-1, Out is V * 2^Exp.

/*
    empty_cells(+Indices,+Grid,-GOut)
    Actualiza las celdas en las posiciones especificadas por Indices a un valor vacío.
    Indices: Lista de indices de las celdas a vaciar
    Grid: Lista que representa la grilla
    GOut: Nueva grilla con las celdas vacías
*/
empty_cells([],G,G).
empty_cells([I|Is],Grid,GOut) :-
    empty_cell(Empty),
    nth1_update(I,Grid,Empty,G1),
    empty_cells(Is,G1,GOut).

/*
    bubble_up(+Index,+Grid,+NCols,-GridOut,-IdxOut)
    Mueve un bloque hacia arriba en la grilla si es posible, hasta que no pueda
    subir más.
    Index: Posicion del bloque a mover
    Grid: Lista que representa la grilla
    NCols: Numero de columnas de la grilla
    GridOut: Nueva grilla con el bloque movido
    IdxOut: Posicion final del bloque en la grilla
*/
bubble_up(Index,Grid,NCols,GridOut,IdxOut) :-
    IdxUp is Index-NCols,
    empty_cell(Empty),
    (   IdxUp >= 1, get_cell(IdxUp,Grid,Empty)
    ->  get_cell(Index,Grid,Val),
        nth1_update(Index,Grid,Empty,T1),
        nth1_update(IdxUp,T1,Val,T2),
        bubble_up(IdxUp,T2,NCols,GridOut,IdxOut)
    ;   GridOut = Grid, IdxOut = Index
    ).

/*--------------------------------------------------------------------
  3. Compactar columnas 
--------------------------------------------------------------------*/
/*
    compact_grid_up(+Grid,+NCols,-GridOut)
    Compacta las columnas de la grilla, moviendo los bloques hacia arriba
    y rellenando con celdas vacías al final de cada columna.
    Grid: Lista que representa la grilla
    NCols: Numero de columnas de la grilla
    GridOut: Nueva grilla compactada
*/
compact_grid_up(Grid,NCols,GridOut) :-
    dims(Grid,NCols,NRows),
    compact_columns(1,NCols,NRows,Grid,NCols,GridOut).
compact_columns(Col,MaxCol,_,Grid,_,Grid) :- Col > MaxCol, !.
compact_columns(Col,MaxCol,NRows,Grid,NCols,GridOut) :-
    column_indices(Col,NCols,NRows,Idxs),
    maplist({Grid}/[I,V]>>nth1(I,Grid,V),Idxs,Vals),
    empty_cell(Empty),
    include(\=(Empty),Vals,NonEmpty),
    length(NonEmpty,LNon),
    LEmpty is NRows-LNon,
    length(EmptyList,LEmpty), maplist(=(Empty),EmptyList),
    append(NonEmpty,EmptyList,NewVals),
    update_indices_with_vals(Idxs,NewVals,Grid,Grid1),
    Col1 is Col+1,
    compact_columns(Col1,MaxCol,NRows,Grid1,NCols,GridOut).

/*
    update_indices_with_vals(+Indices,+Values,+Grid,-GOut)
    Actualiza los indices especificados en la grilla con los valores correspondientes.
    Indices: Lista de indices a actualizar
    Values: Lista de valores a insertar en los indices
    Grid: Lista que representa la grilla
    GOut: Nueva grilla con los valores actualizados
*/
update_indices_with_vals([],[],G,G).
update_indices_with_vals([I|Is],[V|Vs],Grid,GOut) :-
    nth1_update(I,Grid,V,G1),
    update_indices_with_vals(Is,Vs,G1,GOut).

/*
    column_indices(+Col,+NCols,+NRows,-Idxs)
    Obtiene los indices de todas las celdas en la columna Col de la grilla.
    Col: Numero de la columna
    NCols: Numero de columnas de la grilla  
    NRows: Numero de filas de la grilla
    Idxs: Lista de indices de las celdas en la columna Col
*/
column_indices(Col,NCols,NRows,Idxs) :-
    findall(I,(between(1,NRows,R), index(R,Col,NCols,I)),Idxs).

/*
    col_of_idx(+Index,+NCols,-Col)
    Obtiene el numero de columna a partir de un indice dado.
    Index: Posicion del elemento en la grilla
    NCols: Numero de columnas de la grilla
    Col: Numero de la columna correspondiente al indice
*/
col_of_idx(Index,NCols,Col) :- Col is ((Index-1) mod NCols) + 1.

/*
    find_block_in_column(+Grid,+NCols,+Col,+Val,-Index)
    Busca un bloque con el valor Val en la columna Col de la grilla Grid.
    Grid: Lista que representa la grilla
    NCols: Numero de columnas de la grilla
    Col: Numero de la columna donde buscar
    Val: Valor del bloque a buscar
    Index: Indice del bloque encontrado en la grilla
*/
find_block_in_column(Grid,NCols,Col,Val,Index) :-
    dims(Grid,NCols,NRows),
    between(1,NRows,R), index(R,Col,NCols,Index), nth1(Index,Grid,Val), !.

/*--------------------------------------------------------------------
  4. Cascada de fusiones y efectos
--------------------------------------------------------------------*/
/*
    shoot(+Block,+Col,+Grid,+NCols,-Effects, -ForbiddenBlocksOut) % <-- Nuevo argumento
    Coloca un bloque en la columna Col de la grilla Grid y realiza cascadas de fusiones.
    Block: Valor del bloque a colocar
    Col: Numero de la columna donde se coloca el bloque
    Grid: Lista que representa la grilla
    NCols: Numero de columnas de la grilla
    Effects: Lista de efectos resultantes de las fusiones
    ForbiddenBlocksOut: Lista final de bloques prohibidos para lanzar. <-- NUEVO
*/
shoot(Block,Col,Grid,NCols,Effects, ForbiddenBlocksOut) :-
    deleteForbiddenBlocks(Grid, TempCleanGrid),        % <-- Obtiene la grilla con los prohibidos eliminados
    compact_grid_up(TempCleanGrid, NCols, CleanGrid),  % <-- ¡Aplica compact_grid_up aquí!
    drop_block(Block,Col,CleanGrid,NCols,G1,RowPlaced), % <-- ¡USA CleanGrid (ya compactada) AQUÍ!
    index(RowPlaced,Col,NCols,IdxPlaced0),
    get_cell(IdxPlaced0,G1,Val0),
    cascade_fuse(IdxPlaced0, G1, NCols, Val0, GridsAndEffectsAsc),
    grids_to_effects(GridsAndEffectsAsc, Effects),
    forbidden_blocks_accumulated(ForbiddenBlocksOut).
/*
    cascade_fuse(+IdxOrig,+Grid,+NCols,+Val,+GridsAsc)
    Realiza cascadas de fusiones a partir del bloque colocado en la grilla.
    IdxOrig: Índice del bloque colocado originalmente
    Grid: Lista que representa la grilla
    NCols: Número de columnas de la grilla
    Val: Valor del bloque colocado
    GridsAsc: Lista de grillas resultantes de las fusiones
*/
cascade_fuse(IdxOrig, Grid, NCols, _Val, GridsAndEffectsAsc) :-
    % El acumulador inicial para cascade_fuse_ es la primera grilla con una lista vacía de efectos individuales.
    % Los efectos de score y newBlock (por fusión) se añadirán a medida que ocurran.
    cascade_fuse_(Grid, NCols, IdxOrig, [effect(Grid, [])], RevGridsAndEffects),
    reverse(RevGridsAndEffects, GridsAndEffectsAsc).

cascade_fuse_(Grid, NCols, IdxOrig, AccEffects, FinalAccEffects) :-
    dims(Grid, NCols, NRows),
    empty_cell(Empty),
    findall((I, Cluster),
        (
            nth1(I, Grid, Val), Val \= Empty,
            cluster_same(I, Grid, NCols, NRows, Val, Cluster),
            length(Cluster, Size), Size >= 2
        ),
        ClustersWithIdx
    ),
    (
        ClustersWithIdx = []
    ->
        FinalAccEffects = AccEffects
    ;
        % Buscar un clúster que incluya el índice original
        ( member((_, Cluster0), ClustersWithIdx), member(IdxOrig, Cluster0)
        -> I = IdxOrig, Cluster = Cluster0
        ;  ClustersWithIdx = [(I, Cluster)|_]
        ),
        nth1(I, Grid, Val),
        merge_cluster(Cluster, I, Grid, Val, NCols, Grid2, Val2, NewIdxOrig),
        
        % Primero, actualizamos los bloques prohibidos
        update_forbidden_blocks_accumulated(Val2),

        % Luego, eliminamos los bloques prohibidos de la *grilla actual* (Grid2)
        % y compactamos inmediatamente.
        deleteForbiddenBlocks(Grid2, Grid3),            % Elimina los prohibidos de Grid2
        compact_grid_up(Grid3, NCols, CompactedGrid),   % Compacta Grid3

        length(Cluster, RealClusterSize),
        CurrentStepIndividualEffects = [newBlock(Val2), newBlock(Val2, RealClusterSize), score(Val2)],

        % Continuamos la cascada con la grilla ya compactada y limpiada
        cascade_fuse_(CompactedGrid, NCols, NewIdxOrig, [effect(CompactedGrid, CurrentStepIndividualEffects)|AccEffects], FinalAccEffects)
    ).

    deleteForbiddenBlocks(GridIn, GridOut) :-
        forbidden_blocks_accumulated(Forbidden),
        maplist(replaceForbidden(Forbidden), GridIn, GridOut).

    replaceForbidden(Forbidden, X, '-') :-
        number(X),
        member(X, Forbidden), !.
        replaceForbidden(_, X, X).

/*--------------------------------------------------------------------
  5. Convertir grillas → effects
--------------------------------------------------------------------*/

/*
    grids_to_effects(+GridsAndIndividualEffects,+Effects)
    Convierte una lista de grillas (con sus efectos individuales) en una lista final de efectos.
    GridsAndIndividualEffects: Lista de `effect(Grid, IndividualEffects)` resultantes de las fusiones
    Effects: Lista de efectos finales, combinando `newBlock` y `score`
*/
grids_to_effects([], []).

% Caso base para la última grilla en la secuencia
grids_to_effects([effect(G, IndividualEffects)], [effect(G, IndividualEffects)]).

% Procesa pares de grillas.
% NOTA: new_block_between ya NO es necesario aquí para detectar el bloque *generado por fusión*.
% Ese ya lo registramos en cascade_fuse_.
% Sin embargo, new_block_between podría ser útil si un bloque "cae" en un espacio vacío después de una fusión,
% pero por lo que entiendo, quieres el que *resulta* de la fusión.
% Para simplificar y cumplir tu requisito exacto, podemos ignorar new_block_between aquí.
grids_to_effects([effect(PrevGrid, PrevIndividualEffects), effect(NextGrid, NextIndividualEffects)|Rest], [effect(PrevGrid, PrevIndividualEffects)|Tail]) :-
    % Simplemente pasamos los efectos individuales que ya fueron registrados.
    % La detección de newBlock (por fusión) y score ya ocurrió en cascade_fuse_.
    % new_block_between podría ser útil para otros tipos de "nuevos bloques",
    % pero para tu caso de "cuando se fusione", ya lo tenemos.
    grids_to_effects([effect(NextGrid, NextIndividualEffects)|Rest], Tail).


/*
    new_block_between(+G1,+G2,-Val)
    (Este predicado ya no sería necesario para tu requisito específico de 'newBlock por fusión',
    pero lo dejo por si tiene otros usos en tu lógica general).
    Determina el valor del nuevo bloque entre dos grillas G1 y G2.
    G1: Grilla anterior
    G2: Grilla siguiente
    Val: Valor del nuevo bloque, o none si no hay bloque nuevo
*/
new_block_between(G1,G2,Val) :-
    empty_cell(Empty),
    findall(V,(nth1(I,G2,V), nth1(I,G1,Empty), number(V)),Vs),
    (   Vs = [Val|_] -> true ; Val = none).
/*--------------------------------------------------------------------
  6. randomBlock (con cambios para bloques prohibidos)
--------------------------------------------------------------------*/

% Declaramos un hecho dinámico para almacenar la lista de bloques prohibidos.
% Inicializamos como una lista vacía.
:- dynamic(forbidden_blocks_accumulated/1).

/*
    forbidden_blocks_accumulated(+Forbidden)
    Almacena los bloques prohibidos acumulados en la base de datos.
    Forbidden: Lista de bloques prohibidos
*/
forbidden_blocks_accumulated([]).

/*
    randomBlock(+Block)
    Genera un bloque aleatorio basado en el máximo valor en la grilla,
    excluyendo los bloques prohibidos.    
    Grid: Lista que representa la grilla
    Block: Valor del bloque aleatorio generado
*/
/*
randomBlock(Grid, 2) :-
    forall(member(X, Grid), empty_cell(X)).
randomBlock(Grid, Block) :-
    max_num(Grid, Max),
    MaxBlock is max(2, Max // 2),        
    power_of_2(MaxBlock, AllPotencias),
    % Filtramos las potencias para excluir los bloques prohibidos
    forbidden_blocks_accumulated(Forbidden),
    exclude(member_of_forbidden(Forbidden), AllPotencias, AvailablePotencias),
    (   AvailablePotencias = [] % Si no hay bloques disponibles, podemos manejar un caso de error o default
    ->  random_member(Block, AllPotencias) % En este caso, genera cualquiera si no hay opciones válidas
    ;   random_member(Block, AvailablePotencias)
    ).*/

randomBlock(Grid, 2) :-
    forall(member(X, Grid), empty_cell(X)).

randomBlock(Grid, Block) :-
    max_num(Grid, MaxGrilla),
    rango_valido(MaxGrilla, MinVal, MaxVal),
    power_of_2(MaxVal, AllPotencias),
    include(in_rango(MinVal, MaxVal), AllPotencias, RangoPermitido),
    forbidden_blocks_accumulated(Forbidden),
    exclude(member_of_forbidden(Forbidden), RangoPermitido, Disponibles),
    ( Disponibles = [] ->
        random_member(Block, RangoPermitido)  % fallback si no hay válidos
    ;
        random_member(Block, Disponibles)
    ).


empty_cell('-').

/*
    member_of_forbidden(+Forbidden,+Element)
    Verifica si un elemento está en la lista de bloques prohibidos.
    Forbidden: Lista de bloques prohibidos
    Element: Elemento a verificar
*/
member_of_forbidden(ForbiddenList, Element) :-
    member(Element, ForbiddenList).

/*
    max_num(+Grid,-Max)
    Encuentra el máximo número en la grilla, o 0 si no hay números.
    Grid: Lista que representa la grilla
    Max: Máximo número encontrado en la grilla
*/
max_num(Grid, Max) :-
    include(number, Grid, Numbers),
    (   Numbers = [] -> Max = 0
    ;   max_list(Numbers, Max)
    ).

/*
    power_of_2(+Max, -Lista)
    Genera una lista de potencias de 2 hasta el máximo especificado.
    Max: Máximo valor para las potencias de 2
    Lista: Lista de potencias de 2 generadas
*/
power_of_2(Max, List) :-
    power_of_2_aux(1, Max, [], List).

/*
    power_of_2_aux(+N, +Max, +Acc, -List)
    Genera potencias de 2 recursivamente hasta que el valor supere Max.
    N: Exponente actual
    Max: Máximo valor permitido
    Acc: Acumulador para las potencias generadas
    List: Lista final de potencias de 2
*/
power_of_2_aux(N, Max, Acc, List) :-
    Valor is 2^N,
    (   Valor =< Max
    ->  power_of_2_aux(N + 1, Max, [Valor|Acc], List)
    ;   reverse(Acc, List)
    ).
    
/*
    log2(+X, -L)
    Calcula el logaritmo base 2 de X.
    X: Valor del cual calcular el logaritmo
    L: Resultado del logaritmo base 2
*/
log2(X, L) :- L is log(X) / log(2).

/*
    update_forbidden_blocks_accumulated(+MergedVal)
    Actualiza la lista de bloques prohibidos acumulados si MergedVal es mayor o igual a 1024.
    MergedVal: Valor resultante de una fusión
*/
update_forbidden_blocks_accumulated(MergedVal) :-
    forbidden_blocks_accumulated(CurrentForbidden),
    (   MergedVal =:= 1024
    ->  Block = 2,
        add_block_to_forbidden(Block, CurrentForbidden)
    ;   MergedVal =:= 2048
    ->  Block = 4,
        add_block_to_forbidden(Block, CurrentForbidden)
    ;   MergedVal =:= 4096
    ->  Block = 8,
        add_block_to_forbidden(Block, CurrentForbidden)
    ;   MergedVal >= 16384
    ->  log2(MergedVal, ExpF),
        Exponent is round(ExpF),
        MaxIndex is Exponent - 10,
        findall(P, (between(1, MaxIndex, I), P is 2^I), BlocksToProhibit),
        append(BlocksToProhibit, CurrentForbidden, TempNewForbidden),
        list_to_set(TempNewForbidden, UniqueForbidden),
        retractall(forbidden_blocks_accumulated(_)),
        assertz(forbidden_blocks_accumulated(UniqueForbidden))
    ;   true  % Si MergedVal < 1024 o es 8192, no hacer nada
    ).

add_block_to_forbidden(Block, CurrentForbidden) :-
    ( member(Block, CurrentForbidden) ->
        true  % Ya está, no hacer nada
    ;   append([Block], CurrentForbidden, Temp),
        list_to_set(Temp, Unique),
        retractall(forbidden_blocks_accumulated(_)),
        assertz(forbidden_blocks_accumulated(Unique))
    ).


rango_valido(MaxGrilla, Min, Max) :-
    ( MaxGrilla < 16     -> Min = 2,  Max = 4
    ; MaxGrilla < 32     -> Min = 2,  Max = 8
    ; MaxGrilla < 64     -> Min = 2,  Max = 16
    ; MaxGrilla < 128    -> Min = 2,  Max = 32
    ; MaxGrilla < 1024   -> Min = 2,  Max = 64
    ; MaxGrilla < 2048   -> Min = 4,  Max = 128
    ; MaxGrilla < 4096   -> Min = 8,  Max = 256
    ; MaxGrilla < 16384  -> Min = 16, Max = 512
    ;                       Min = 32, Max = 1024
    ).

in_rango(Min, Max, X) :-
    X >= Min,
    X =< Max.

/**
 * Bloque Maximo Shooteable
 * 
 * */

% max_shootable_block(+Grid, -Max)
max_shootable_block(Grid, Max) :-
    max_num(Grid, MaxGrilla),
    rango_valido(MaxGrilla, MinVal, MaxVal),
    power_of_2(MaxVal, AllPotencias),
    include(in_rango(MinVal, MaxVal), AllPotencias, RangoPermitido),
    forbidden_blocks_accumulated(Forbidden),
    exclude(member_of_forbidden(Forbidden), RangoPermitido, Disponibles),
    max_list(Disponibles, Max).


/*--------------------------------------------------------------------
  7. Booster Hint Jugada
--------------------------------------------------------------------*/
predict_combo(Block, Col, Grid, NCols, ComboSize) :-
    % Paso 1: Limpiar y compactar la grilla antes de simular
    deleteForbiddenBlocks(Grid, TempCleanGrid),
    compact_grid_up(TempCleanGrid, NCols, CleanGrid),

    % Paso 2: Simular la jugada como en shoot/6, pero sin modificar el estado global
    drop_block(Block, Col, CleanGrid, NCols, G1, RowPlaced),
    index(RowPlaced, Col, NCols, IdxPlaced),
    get_cell(IdxPlaced, G1, Val0),

    % Paso 3: Ejecutar la cascada de fusiones, acumulando grillas intermedias
    cascade_fuse(IdxPlaced, G1, NCols, Val0, Effects),

    % Paso 4: Extraer todos los clústeres de cada fusión (excepto la primera) y quedarnos con el mayor
    extract_combo_clusters(Effects, ClusterSizes),
    (   ClusterSizes = [] -> ComboSize = 0
    ;   max_list(ClusterSizes, ComboSize)
    ).

extract_combo_clusters(Effects, ClusterSizes) :-
    Effects = [_|Rest],
    findall(Size,
        (
            member(effect(_, Efxs), Rest),
            member(newBlock(_, Size), Efxs)
        ),
        ClusterSizes
    ).


unmerge_value(Val, Size) :-
    Val > 0,
    log2(Val, LogVal),
    floor(LogVal) =:= LogVal,  % Verifica que Val es potencia de 2
    Base = 2,
    between(1, 20, Size),
    V is Val // (Base ** (Size - 1)),
    integer(V),
    Val =:= V * Base ** (Size - 1).

