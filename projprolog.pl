% -----------------------------------------------
%           Tomas Teixeira nr. 104165          
% -----------------------------------------------

% -----------------------------------------------
% predicados comuns
% -----------------------------------------------
ilha(Linha, (Linha, Coluna)) :-
    Linha >= 0,
    Coluna >= 0.

% -----------------------------------------------
% 2.1 extrai_linhas_ilha (N_L, Linhas, Ilha)
% -----------------------------------------------
/*extrai_ilhas_linha(N_L, Linha, Ilhas), em que N_L e um inteiro positivo,
correspondente ao numero de uma linha e Linha e uma lista correspondente a uma linha
de um puzzle, significa que Ilhas e a lista ordenada (ilhas da esquerda para a direita)
cujos elementos sao as ilhas da linha Linha. */


extrai_ilhas_linha(N_L, Linha, Ilhas) :-
    extrai_ilhas_linha(N_L, Linha, Ilhas, 1).

extrai_ilhas_linha(_, [], [], _).

extrai_ilhas_linha(N_L, [E|R], [ilha(E,(N_L, N)) | Ilhas], N) :-
    E > 0,
    N1 is N + 1,
    extrai_ilhas_linha(N_L, R, Ilhas, N1).

extrai_ilhas_linha(N_L, [E|R], Ilhas, N) :-
    E = 0,
    N1 is N + 1,
    extrai_ilhas_linha(N_L, R, Ilhas, N1).

% -----------------------------------------------
% 2.2. ilhas(Puz, Ilhas)
% -----------------------------------------------
/* ilhas(Puz, Ilhas), em que Puz e um puzzle, significa que Ilhas e a lista ordenada
(ilhas da esquerda para a direita e de cima para baixo) cujos elementos sao as ilhas de
Puz. */


ilhas(Puz, Ilhas) :-
    ilhas(Puz, IlhasTmp, 1),
    append(IlhasTmp, Ilhas).

ilhas([], [], _).

ilhas([Linha|Puzzle], [IlhasLinha|Ilhas], NrLinha) :-
    NrLinha > 0,
    extrai_ilhas_linha(NrLinha, Linha, IlhasLinha),
    ProximaLinha is NrLinha + 1,
    ilhas(Puzzle, Ilhas, ProximaLinha).

% -----------------------------------------------
% 2.3. vizinhas(Ilhas, Ilha, Vizinhas)
% -----------------------------------------------
/* vizinhas(Ilhas, Ilha, Vizinhas), em que Ilhas e a lista de ilhas de um puzzle
e Ilha e uma dessas ilhas, significa que Vizinhas e a lista ordenada (ilhas de cima para
baixo e da esquerda para a direita ) cujos elementos sao as ilhas vizinhas de Ilha. */


ilha_adjacente_vertical(ilha(_, (Linha1, Coluna)), ilha(_, (Linha2, Coluna))) :-
    Linha1 \= Linha2.

ilha_adjacente_horizontal(ilha(_, (Linha, Coluna1)), ilha(_, (Linha, Coluna2))) :-
    Coluna1 \= Coluna2.

ilha_adjacente(Ilha1, Ilha2) :-
    ilha_adjacente_vertical(Ilha1, Ilha2) ; ilha_adjacente_horizontal(Ilha1, Ilha2).



ilha_norte(ilha(_, (Linha1, Coluna)), ilha(_, (Linha2, Coluna))) :-
    Linha2 < Linha1.

ilha_sul(ilha(_, (Linha1, Coluna)), ilha(_, (Linha2, Coluna))) :-
    Linha2 > Linha1.

ilha_este(ilha(_, (Linha, Coluna1)), ilha(_, (Linha, Coluna2))) :-
    Coluna2 > Coluna1.

ilha_oeste(ilha(_, (Linha, Coluna1)), ilha(_, (Linha, Coluna2))) :-
    Coluna2 < Coluna1.

%Uma ilha e vizinha se cumprir o golo GoloSerVizinha 
ilha_vizinha(GoloSerVizinha, [], Ilha, Vizinha, Vizinha) :-
    call(GoloSerVizinha, Ilha, Vizinha).

ilha_vizinha(GoloSerVizinha, [I|Ilhas], Ilha, I2, Vizinha) :-
    not(call(GoloSerVizinha, Ilha, I)),
    ilha_vizinha(GoloSerVizinha, Ilhas, Ilha, I2, Vizinha).

ilha_vizinha(GoloSerVizinha, [I|Ilhas], Ilha, I2, Vizinha) :-
    call(GoloSerVizinha, Ilha, I),
    ( not(call(GoloSerVizinha, Ilha, I2)); call(GoloSerVizinha, I, I2) ),
    ilha_vizinha(GoloSerVizinha, Ilhas, Ilha, I, Vizinha).

ilha_vizinha(GoloSerVizinha, [I|Ilhas], Ilha, I2, Vizinha) :-
    call(GoloSerVizinha, I2, I),
    ilha_vizinha(GoloSerVizinha, Ilhas, Ilha, I2, Vizinha).

ilha_vizinha(GoloSerVizinha, [I|Ilhas], Ilha, Vizinha) :-
    ilha_vizinha(GoloSerVizinha, Ilhas, Ilha, I, Vizinha).

vizinhas(Ilhas, _, [], Index) :-
    length(Ilhas, NrIlhas),
    Index >= NrIlhas.

vizinhas(Ilhas, Ilha, Vizinhas, Index) :-
    not(member(Ilha, Vizinhas)),
    ProximoIndex is Index + 1,
    vizinhas(Ilhas, Ilha, Vizinhas, ProximoIndex).

%Verificar se e ou a ilha norte, ou sul, ou este, ou oeste mais proxima da ilha que temos originalmente
vizinhas(Ilhas, Ilha, [IlhaAtual|Vizinhas], Index) :-
    nth0(Index, Ilhas, IlhaAtual),
    (
        ilha_vizinha((ilha_norte), Ilhas, Ilha, IlhaAtual);
        ilha_vizinha((ilha_sul), Ilhas, Ilha, IlhaAtual);
        ilha_vizinha((ilha_este), Ilhas, Ilha, IlhaAtual);
        ilha_vizinha((ilha_oeste), Ilhas, Ilha, IlhaAtual)
    ),
    ProximoIndex is Index + 1,
    vizinhas(Ilhas, Ilha, Vizinhas, ProximoIndex).

vizinhas(Ilhas, Ilha, Vizinhas, Index) :-
    ProximoIndex is Index + 1,
    vizinhas(Ilhas, Ilha, Vizinhas, ProximoIndex).

vizinhas(Ilhas, Ilha, Vizinhas) :-
    vizinhas(Ilhas, Ilha, Vizinhas, 0).


% -----------------------------------------------
% 2.4. estado(Ilhas, Estado)
% -----------------------------------------------
/* estado(Ilhas, Estado), em que Ilhas e a lista de ilhas de um puzzle, significa que
Estado e a lista ordenada cujos elementos sao as entradas referentes a cada uma das
ilhas de Ilhas. */


estado(Ilhas, [], NrIlhasProcessadas) :-
    length(Ilhas, LenIlhas),
    NrIlhasProcessadas >= LenIlhas.

estado(Ilhas, [[Ilha, Vizinhas, []]|Estado], IndexIlha) :-
    nth0(IndexIlha, Ilhas, Ilha),
    exclude(=(Ilha), Ilhas, IlhasSemPropria),
    vizinhas(IlhasSemPropria, Ilha, Vizinhas),
    ProximoIndexIlha is IndexIlha + 1,
    estado(Ilhas, Estado, ProximoIndexIlha).

estado([], []).

estado(Ilhas, Estado) :-
    estado(Ilhas, Estado, 0).

% -----------------------------------------------
% 2.5. posicoes_entre(Pos1, Pos2, Posicoes)
% -----------------------------------------------
/* posicoes_entre(Pos1, Pos2, Posicoes), em que Pos1 e Pos2 sao posicoes, sig-
nifica que Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2 (excluindo Pos1 e
Pos2). Se Pos1 e Pos2 nao pertencerem a mesma linha ou a mesma coluna, o resultado
e false. */


%verificar que nao falha caso a Pos1 e Pos2 estejam trocadas
posicoes_entre((Linha, ColunaMin), (Linha, ColunaMax), Posicoes) :-
    ColunaMin > ColunaMax,
    NewMax is ColunaMin,
    NewMin is ColunaMax,
    posicoes_entre((Linha, NewMin), (Linha, NewMax), Posicoes).

posicoes_entre((LinhaMin, Coluna), (LinhaMax, Coluna), Posicoes) :-
    LinhaMin > LinhaMax,
    NewMax is LinhaMin,
    NewMin is LinhaMax,
    posicoes_entre((NewMin, Coluna), (NewMax, Coluna), Posicoes).

posicoes_entre((Linha, ColunaMin), (Linha, ColunaMax), Posicoes) :-
    ColunaMin < ColunaMax,
    Coluna is ColunaMin + 1,
    posicoes_entre((Linha, ColunaMin), (Linha, ColunaMax), Posicoes, Coluna).

posicoes_entre((LinhaMin, Coluna), (LinhaMax, Coluna), Posicoes) :-
    LinhaMin < LinhaMax,
    Linha is LinhaMin + 1,
    posicoes_entre((LinhaMin, Coluna), (LinhaMax, Coluna), Posicoes, Linha).

posicoes_entre((Linha, _), (Linha, ColunaMax), [], ColunaMax) :- !.

posicoes_entre((_, Coluna), (LinhaMax, Coluna), [], LinhaMax) :- !.

posicoes_entre((Linha, ColunaMin), (Linha, ColunaMax), [(Linha, Coluna)|Posicoes], Coluna) :-
    ProximaColuna is Coluna + 1,
    posicoes_entre((Linha, ColunaMin), (Linha, ColunaMax), Posicoes, ProximaColuna).

posicoes_entre((LinhaMin, Coluna), (LinhaMax, Coluna), [(Linha, Coluna)|Posicoes], Linha) :-
    ProximaLinha is Linha + 1,
    posicoes_entre((LinhaMin, Coluna), (LinhaMax, Coluna), Posicoes, ProximaLinha).

% -----------------------------------------------
% 2.6. cria_ponte(Pos1, Pos2, Ponte)
% -----------------------------------------------
/* cria_ponte(Pos1, Pos2, Ponte), em que Pos1 e Pos2 sao 2 posicoes, significa
que Ponte e uma ponte entre essas 2 posicoes. */


%primeiro, definir o que e uma ponte
ponte((X0,Y0), (X1,Y1)) :-
    X0 >= 0,
    X1 >= 0,
    Y0 >= 0,
    Y1 >= 0.

cria_ponte((Linha, Coluna1), (Linha, Coluna2), Ponte) :-
    Coluna2 < Coluna1,
    cria_ponte((Linha, Coluna2), (Linha, Coluna1), Ponte).

cria_ponte((Linha1, Coluna), (Linha2, Coluna), Ponte) :-
    Linha2 < Linha1,
    cria_ponte((Linha2, Coluna), (Linha1, Coluna), Ponte).

cria_ponte((Linha, Coluna1), (Linha, Coluna2), ponte((Linha, Coluna1), (Linha, Coluna2))) :-
    Coluna1 \= Coluna2.

cria_ponte((Linha1, Coluna), (Linha2, Coluna), ponte((Linha1, Coluna), (Linha2, Coluna))) :-
    Linha1 \= Linha2.

% -----------------------------------------------
% 2.7. caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
% -----------------------------------------------
/* caminho_livre(Pos1, Pos2, Posicoes, I, Vz), em que Pos1 e Pos2 sao po-
sicoes, Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2, I e uma ilha, e Vz
e uma das suas vizinhas, significa que a adicao da ponte ponte(Pos1, Pos2) nao faz
com que I e Vz deixem de ser vizinhas. */


%se as pos coincidirem com as ilhas, o caminho e livre
caminho_livre(Pos1, Pos2, _, ilha(_, Pos3), ilha(_, Pos4)) :-
    Pos1 = Pos3, Pos2 = Pos4;
    Pos1 = Pos4, Pos2 = Pos3.

caminho_livre(_, _, Posicoes, ilha(_, Pos3), ilha(_, Pos4)) :-
    posicoes_entre(Pos3, Pos4, PosEntre),
    intersection(Posicoes, PosEntre, []).

% -----------------------------------------------
% 2.8. actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
% -----------------------------------------------
/* actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada,
Nova_Entrada), em que Pos1 e Pos2 sao as posicoes entre as quais ira ser adi-
cionada uma ponte, Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2,
e Entrada e uma entrada (ver Seccao 2.4), significa que Nova_Entrada e igual a
Entrada, excepto no que diz respeito a lista de ilhas vizinhas; esta deve ser actualizada,
removendo as ilhas que deixaram de ser vizinhas, apos a adicao da ponte. */


actualiza_vizinhas_entrada(_, _, _, [Ilha, [], Pontes], [Ilha, [], Pontes]).

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, [Vizinha|Vizinhas], Pontes], [Ilha, [Vizinha|Vizinhas2], Pontes2]) :-
    caminho_livre(Pos1, Pos2, Posicoes, Ilha, Vizinha),
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, Vizinhas, Pontes], [Ilha, Vizinhas2, Pontes2]).

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, [_|Vizinhas], Pontes], [Ilha, Vizinhas2, Pontes2]) :-
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, Vizinhas, Pontes], [Ilha, Vizinhas2, Pontes2]).

% -----------------------------------------------
% 2.9. actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% -----------------------------------------------
/* actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) ,
em que Estado e um estado (ver Seccao 2.4), Pos1 e Pos2 sao as posicoes entre as
quais foi adicionada uma ponte, significa que Novo_estado e o estado que se obtem de
Estado apos a actualizacao das ilhas vizinhas de cada uma das suas entradas. */


actualiza_vizinhas_apos_pontes([], _, _, []).

actualiza_vizinhas_apos_pontes([Entrada|Estado], Pos1, Pos2, [Nova_entrada|Novo_estado]) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_entrada),
    actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado).


% -----------------------------------------------
% 2.10. ilhas_terminadas(Estado, Ilhas_term)
% -----------------------------------------------
/* ilhas_terminadas(Estado, Ilhas_term), em que Estado e um estado (ver Sec-
cao 2.4), significa que Ilhas_term e a lista de ilhas que ja tem todas as pontes associadas,
designadas por ilhas terminadas. Se a entrada referente a uma ilha for [ilha(N_pontes,
Pos), Vizinhas, Pontes], esta ilha esta terminada se N_pontes for diferente de
'X' (a razao para esta condicao ficara aparente mais a frente) e o comprimento da lista
Pontes for N_pontes . */


ilhas_terminadas([], []).

ilhas_terminadas([[ilha(N_Pontes, Pos), _, Pontes]|Estado], [ilha(N_Pontes, Pos)|Ilhas_term]) :-
    N_Pontes \= 'X',
    length(Pontes, N_Pontes),
    ilhas_terminadas(Estado, Ilhas_term).

ilhas_terminadas([_|Estado], Ilhas_term) :-
    ilhas_terminadas(Estado, Ilhas_term).

% -----------------------------------------------
% 2.11. tira_ilhas_terminadas_entrada( Ilhas_term, Estado, Nova_entrada)
% -----------------------------------------------
/* tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada),
em que Ilhas_term e uma lista de ilhas terminadas e Entrada e uma entrada (ver
Seccao 2.4), significa que Nova_entrada e a entrada resultante de remover as ilhas de
Ilhas_term, da lista de ilhas vizinhas de entrada. */


tira_ilhas_terminadas_entrada([], NovoEstado, NovoEstado).

tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vizinhas, Pontes], [Ilha, NovasVizinhas, Pontes]) :-
    findall(I, (member(I, Vizinhas), not(member(I, Ilhas_term))), NovasVizinhas).

% -----------------------------------------------
% 2.12. tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% -----------------------------------------------
/* tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que
Estado e um estado (ver Seccao 2.4) e Ilhas_term e uma lista de ilhas termi-
nadas, significa que Novo_estado e o estado resultante de aplicar o predicado
tira_ilhas_terminadas_entrada a cada uma das entradas de Estado. */


tira_ilhas_terminadas([], _, []).

% se nao houver ilhas terminadas, o novo estado = estado
tira_ilhas_terminadas(Estado, [], Estado).

tira_ilhas_terminadas([Entrada|Estado], Ilhas_term, [Nova_entrada|Novo_estado]) :-
    tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada),
    tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado).

tira_ilhas_terminadas([_|Estado], Ilhas_term, [_|NovoEstado]) :-
    tira_ilhas_terminadas(Estado, Ilhas_term, NovoEstado).

% -----------------------------------------------
% 2.13. marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% -----------------------------------------------
/* marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,
Nova_entrada), em que Ilhas_term e uma lista de ilhas terminadas e Entrada
e uma entrada (ver Seccao 2.4), significa que Nova_entrada e a entrada obtida de
Entrada da seguinte forma: se a ilha de Entrada pertencer a Ilhas_term, o numero
de pontes desta e substituido por 'X'; em caso contrario Nova_entrada e igual a
Entrada. */


marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N_Pontes, Pos), Vz, P], [ilha('X', Pos), Vz, P]) :-
    member(ilha(N_Pontes,Pos), Ilhas_term).

marca_ilhas_terminadas_entrada(_, Entrada, Entrada).

% -----------------------------------------------
% 2.14. marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% -----------------------------------------------
/* marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que
Estado e um estado (ver Seccao 2.4) e Ilhas_term e uma lista de ilhas termi-
nadas, significa que Novo_estado e o estado resultante de aplicar o predicado
marca_ilhas_terminadas_entrada a cada uma das entradas de Estado. */


marca_ilhas_terminadas([], _, []).

marca_ilhas_terminadas([Estado|Estados], Ilhas_term, [Novo_estado|Novos_estados]) :-
    marca_ilhas_terminadas_entrada(Ilhas_term, Estado, Novo_estado),
    marca_ilhas_terminadas(Estados, Ilhas_term, Novos_estados).

% -----------------------------------------------
% 2.15. trata_ilhas_terminadas(Estado, Novo_estado)
% -----------------------------------------------
/* trata_ilhas_terminadas(Estado, Novo_estado), em que Estado e um estado
(ver Seccao 2.4), significa que Novo_estado e o estado resultante de aplicar os predica-
dos tira_ilhas_terminadas e marca_ilhas_terminadas a Estado. */


trata_ilhas_terminadas(Estado, Novo_estado) :-
    ilhas_terminadas(Estado, Ilhas_term),
    marca_ilhas_terminadas(Estado, Ilhas_term, Aux),
    tira_ilhas_terminadas(Aux, Ilhas_term, Novo_estado).

% -----------------------------------------------
% 2.16. Predicado junta_pontes/5
% -----------------------------------------------
/* junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado), em
que Estado e um estado e Ilha1 e Ilha2 sao 2 ilhas, significa que Novo_estado e
o estado que se obtem de Estado por adicao de Num_pontes pontes entre Ilha1 e
Ilha2. */


cria_pontes(ilha(_, Pos1), ilha(_, Pos2), Num_pontes, Pontes) :-
    length(Pontes, Num_pontes),
    cria_ponte(Pos1, Pos2, P),
    maplist(=(P), Pontes).

%este predicado serve para atualizart tudo antes de correr a proxima iteracao
junta_pontes_([], _, _, _, []).

junta_pontes_([[ilha(NP, Pos), Vz, Pontes]|Estado], Num_pontes, ilha(NP1, Pos1), ilha(NP2, Pos2), [[ilha(NP, Pos), Vz, PontesFinal]|Novo_estado]) :-
    (
        ilha(NP, Pos) = ilha(NP1, Pos1); ilha(NP, Pos) = ilha(NP2, Pos2)
    ),
    cria_pontes(ilha(_, Pos1), ilha(_, Pos2), Num_pontes, NovasPontes),
    append(Pontes, NovasPontes, PontesFinal),
    junta_pontes_(Estado, Num_pontes, ilha(NP1, Pos1), ilha(NP2, Pos2), Novo_estado).

junta_pontes_([Entrada|Estado], Num_pontes, I1, I2, [Entrada|Novo_estado]) :-
    junta_pontes_(Estado, Num_pontes, I1, I2, Novo_estado).

junta_pontes(Estado, Num_pontes, ilha(_, Pos1), ilha(_, Pos2), NovoEstado) :-
    junta_pontes_(Estado, Num_pontes, ilha(_, Pos1), ilha(_, Pos2), NovoEstadoAux),
    actualiza_vizinhas_apos_pontes(NovoEstadoAux, Pos1, Pos2, NovoEstadoAux2),
    trata_ilhas_terminadas(NovoEstadoAux2, NovoEstado).

