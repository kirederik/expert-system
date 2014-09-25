/********************************************************
 *  _____           _                                   *
 * |  __ \         | |                                  *
 * | |__) | __ ___ | | ___   __ _                       *
 * |  ___/ '__/ _ \| |/ _ \ / _` |                      *
 * | |   | | | (_) | | (_) | (_| |                      *
 * |_|   |_|  \___/|_|\___/ \__, |                      *
 *                           __/ |                      *
 *                          |___/                       *
 *                                                      *
 * @author Derik Evangelista, kirederik@gmail.com       *
 * @date 05 Oct 2012                                    *
 *                                                      *
 * Expert System Shell                                  *
 *                                                      *
 *******************************************************/


/*
* O trabalho se desenvolverá em 6 passos:
*
* 1- Iterar sobre as regras da base de regra
* 2- Identificar as regras que podem ser utilizadas para o diagnóstico em questão
* 3- Analisar as premissas e fazer as perguntas pertinentes
* 4- Atualizar a base de fatos com a entrada do usuário
* 5- Repetir até que nenhuma regra possa ser utilizada
* 6- Imprimir Diagnóstico.
*/



/* Base de Regras (Retirado do Enunciado) */
base_de_regras(
               [
                [ [idade, jovem], [especie, humana], [sexo, masculino], -> , 1.0, [chamado, menino] ],
                [ [idade, jovem], [especie, humana], [sexo, feminino], -> , 1.0, [chamado, menina] ],
                [ [idade, adulto], [especie, humana], [sexo, masculino], -> , 1.0, [chamado, homem] ],
                [ [idade, adulto], [especie, humana], [sexo, feminino], -> , 1.0, [chamado, mulher] ],
                [ [idade, jovem], [especie, cao], ->, 1.0, [chamado, filhote] ],
                [ [idade, adulto], [especie, cao], [sexo, feminino], ->, 1.0, [chamado, cadela] ],
                [ [idade, adulto], [especie, cao], ->, 0.5, [chamado, cao] ],
                [ [pernas, 2], ->, 1.0, [especie, humana] ],
                [ [pernas, 4], ->, -1.0, [especie, humana] ],
                [ [pernas, 4], ->, 0.5, [especie, cao] ],
                [ [altura, baixa], ->, 0.5, [especie, cao] ],
                [ [altura, alta], ->, 0.5, [especie, humana] ]
               ]).

/** Predicados de impressao */
pula_linha(nl) :-
    monitoramento, nl.
pula_linha(nl).

pula_linha2(nl) :-
    not(monitoramento),
    nl.
pula_linha2(nl).

escreva_regra(Frase, Variavel) :-
    monitoramento, !,
    write(Frase),
    escreva_regra(Variavel).
escreva_regra(_, _) :-
    not(monitoramento), !.

escreva_regra([]) :-
    monitoramento, !,
    write('].'),
    nl.
escreva_regra([Head | Tail]) :-
    monitoramento, !,
    write(' '),
    write(Head),
    escreva_regra(Tail).
escreva_regra(_) :- not(monitoramento), !.

escreva_regra_justificativa([]) :-
    !,
    write('].'),
    nl,
    pula_linha2(nl).
escreva_regra_justificativa([Head | Tail]) :-
    !,
    write(' '),
    write(Head),
    escreva_regra_justificativa(Tail).

escreva_justificativa(Regra) :-
    write('Eu estou tentando usar a regra:'), nl,
    escreva_regra_justificativa(Regra).

escreva(Frase, Variavel)  :-
    monitoramento, !,
    write(Frase),
    write(Variavel),
    nl.
escreva(_, _) :-
    !,
    not(monitoramento).

escreve_pergunta(Atributo) :-
    monitoramento, !,
    nl,
    write('Me informe sobre '),
    write(Atributo),
    write(' ? ').

escreve_pergunta(Atributo) :-
    not(monitoramento), !,
    write('Me informe sobre '),
    write(Atributo),
    write(' ? ').

escreve_prob(Atr, Valor, Prob) :-
    monitoramento,
    !,
    write('>> A exatidao de '),
    write(Atr),
    write(' = '),
    write(Valor),
    write(' passou a ser '),
    write(Prob),
    nl.

escreve_prob(_, _, _) :-
    not(monitoramento). 

escreve_adeus(Diagnostico) :-
    monitoramento,
    write('>> Abandonando a regra para '),
    write(Diagnostico),
    nl.
escreve_adeus(_) :-
    not(monitoramento).

escreve_hipoteses(BF, Diagnostico) :-
    encontra_unidade(Diagnostico,  BF,Hipotese),
    escreve_resultado(Hipotese).
escreve_hipoteses(_, _) :-
    escreve_resultado([]).
escreve_resultado(Hipotese) :-
    write('As hipoteses sao as seguintes: '),
    write(Hipotese),
    nl, nl.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** Predicados para percorrer base de fatos */

/* Busca de determinado fato está resolvido */
resolvido_atributo([], _) :- !, fail.
resolvido_atributo([ [Valor, Prob] | Tail ],  Valor).
resolvido_atributo([ [_, _] | Tail], C) :-
    resolvido_atributo(Tail, C). 
resolvido_elemento([Elemento | Tail], Diagnostico, [Elemento, Valor]) :-
    !,
    resolvido_atributo(Tail, Valor).

resolvido([], _, _) :- fail.
resolvido([Head | Tail], Elemento, Clausula) :-
    resolvido_elemento(Head, Elemento, Clausula).
resolvido([_ | Tail], El, Cl) :-
    resolvido(Tail, El, Cl).


/* Retorna True se existir determinado atributo na base de fatos */
encontra_atributo([], []).
encontra_atributo([[Atr, Valor] | Tail], [[Atr, Valor] | Tail2]) :-
    encontra_atributo(Tail, Tail2).
encontra_atributo([ [_, _] | Tail], Hipotese) :-
    encontra_atributo(Tail, Hipotese).

encontra_elemento(Diagnostico, [Diagnostico | Tail], Hipotese) :-
    encontra_atributo(Tail, Hipotese).
    
encontra_unidade(Diagnostico, [Head | Tail], Hipotese) :-
    encontra_elemento(Diagnostico, Head, Hipotese).
encontra_unidade(Diagnostico, [Head | Tail], Hipotese) :-
    encontra_unidade(Diagnostico, Tail, Hipotese).

/* Retorna True se existe determinado Elemento na base de fatos */
existe_elemento([], _, _) :- !, fail.
existe_elemento([Elemento | Tail], Regra, [Elemento, _]) :- !.
existe_elemento([_ | Tail], Regra, Clausula) :-
    existe_elemento(Tail, Regra, Clausula).

existe_informacao([], _, _) :- !, fail.
existe_informacao([Head | Tail], Regra, Clausula) :-
    existe_elemento(Head, Regra, Clausula).
existe_informacao([_ | Tail], Regra, Clausula) :-
    existe_informacao(Tail, Regra, Clausula).


/* Checa se o elemento possui o atributo */
procura_atributo([], [Elemento, _], [Atributo, Prob2], [[Atributo, Prob2]]) :-
    escreve_prob(Elemento, Atributo, Prob2).
procura_atributo([[Atributo, Prob] | Tail], [Elemento, V], [Atributo, Prob2], [[Atributo, NovaProb] | Tail]) :-
    !,
    calcula_prob(Prob, Prob2, NovaProb),
    escreve_prob(Elemento, Atributo, NovaProb).

procura_atributo([Head | Tail], L1, L2, [Head | Tail2]) :-
    procura_atributo(Tail, L1, L2, Tail2).
    

/* Checa se a unidade possui o elemento */
procura_elemento([Elemento | Tail], [Elemento, Valor], L2, BF_atualizada) :-
    procura_atributo(Tail, [Elemento, Valor], L2, BF_temp),
    concat_head(Elemento, BF_temp, BF_atualizada).
procura_elemento([Head | Tail], L1, L2, [Head | Tail2]) :-
    procura_elemento(Tail, L1, L2, Tail2).
    
/* Extrai unidade da base de fatos*/
procura_unidade([], [Atributo, Valor], [Resposta, GrauCerteza], [[Atributo, [Resposta, GrauCerteza]]]) :-
    escreve_prob(Atributo, Resposta, GrauCerteza),!.
procura_unidade([Head | Tail], L1, L2, BF_atualizada) :-
    procura_elemento(Head, L1, L2, BF_temp),
    concat_tail(Tail, BF_temp, BF_atualizada).
procura_unidade([Head | Tail], L1, L2, [Head | Tail2]) :-
    procura_unidade(Tail, L1, L2, Tail2).

insere([], Atributo, Valor, Resposta, GrauCerteza, [[Atributo, [Resposta, GrauCerteza]]]) :- 
    escreve_prob(Atributo, Resposta, GrauCerteza),!.
insere(BF, Atributo, Valor, Resposta, GrauCerteza, BF_atualizada) :-
    procura_unidade(BF, [Atributo, Valor], [Resposta, GrauCerteza], BF_atualizada).

insere_fato(Regra, [Atributo, Valor], Resposta, GrauCerteza) :-
    base_de_fatos(BF),
    insere(BF, Atributo, Valor, Resposta, GrauCerteza, BF_atualizada),
    !,
    retractall(base_de_fatos(_)),
    assertz(base_de_fatos(BF_atualizada)).

/* Extrai, para um Atributo, o valor da base de fatos */
extrai_atributo([Valor, GrauCerteza], Valor, [Valor, GrauCerteza]).

extrai_elemento([Elemento | Tail], [Elemento, Valor], Resposta) :- 
    extrai_atributo(Tail, Valor, Resposta).
extrai_elemento([_ | Tail], Clausula, Resposta) :-
    extrai_elemento(Tail, Clausula, Resposta).

extrai_informacao([], _, _) :- fail.
extrai_informacao([Head | Tail], Clausula, Resposta) :-
    extrai_elemento(Head, Clausula, Resposta).
extrai_informacao([_ | Tail], Clausula, Resposta) :-
    extrai_informacao(Tail, Clausula, Resposta).

/* Tenta concluir algo com base nas infos da base de fatos */
procura_no_atributo([], _, _, _, Premissas) :- assertz(invalida_premissa(Premissas)), !, fail.
procura_no_atributo([[Valor, Prob] | Tail], Valor, Prob2, ProbTotal, Premissas) :-
    ProbTotal is Prob.
procura_no_atributo([[_, _] | Tail], Valor, Prob, ProbTotal, Premissas) :-
    procura_no_atributo(Tail, Valor, Prob, ProbTotal, Premissas).

procura_na_unidade([], Premissas, _, _, _) :- assertz(invalida_premissa(Premissa)), !, fail.
procura_na_unidade([Atributo | Tail], Premissas, [Atributo, Valor], Prob, ProbTotal) :-
    procura_no_atributo(Tail, Valor, Prob, ProbTotal, Premissas).

procura_premissas([] ,_, _, _, _) :- !, fail.
procura_premissas([ Unidade | Tail ], Clausula, Premissas, Prob, ProbTotal) :-
    procura_na_unidade(Unidade, Premissas, Clausula, Prob, ProbTotal).
procura_premissas([ _ | Tail], Regra, Premissas, Prob, ProbTotal) :-
    procura_premissas(Tail, Regra, Premissas, Prob, ProbTotal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/* Predicados diversos para extracao de elementos de uma regra */
limiar_de_exatidao(0.2).

extrai_regra([], []) :- !, fail.
extrai_regra([Regra | Tail], Regra).
extrai_regra([Head | Tail], Regra) :-
    extrai_regra(Tail, Regra).

extrai_clausula([], [], [], _).
extrai_clausula(Regra, [], [], Diagnostico) :-
    possivel_concluir(Regra).

extrai_clausula(Regra, [Clausula | Tail], Clausula, _) :-
    not(invalida(Regra)).
extrai_clausula(Regra, [Head | Tail], Clausula, Diagnostico) :-
    extrai_clausula(Regra, Tail, Clausula, Diagnostico).

extrai_clausula_premissa(_, [], [], _) :- !, fail.
extrai_clausula_premissa(Premissa, [Clausula | Tail], Clausula, Tail) :-
    not(invalida_premissa(Premissa)).
extrai_clausula_premissa(Premissa, [Head | Tail], Clausula, R) :-
    extrai_clausula_premissa(Premissa, Tail, Clausula, R).

extrai_conclusao([Premissa, ->, N, Conclusao], Conclusao) :- !.
extrai_conclusao([Head | Tail], Conclusao) :-
    !,
    extrai_conclusao(Tail, Conclusao).

premissas([Premissa, ->, N, Conclusao], [Premissa] ) :- !.
premissas([Premissa | Tail], [Premissa | Tail2]) :-
    !,
    premissas(Tail, Tail2).

extrai_prob([_, ->, N, _], N) :- !.
extrai_prob([Head | Tail], Prob) :-
    !,
    extrai_prob(Tail, Prob).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** Calculos de probabilidade */
absoluto(P, P) :-
    P > 0, !.
absoluto(P, Abs) :-
    Abs is P * -1.

minimo(P1, P2, P1) :-
    P1 < P2, !.
minimo(_, P2, P2).

calcula_prob(P1, P2, P3) :-
    P1 > 0,
    P2 > 0,
    !,
    P3 is P1 + P2 - (P1 * P2).

calcula_prob(P1, P2, P3) :-
    P1 < 0,
    P2 < 0,
    !,
    P2 is P1 + P2 + (P1 * P2).

calcula_prob(P1, P2, P3) :-
    !,
    absoluto(P1, AbsP1),
    absoluto(P2, AbsP2),
    minimo(AbsP1, AbsP2, Min),
    P3 is (P1 + P2) / (1 - Min).    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Predicados Auxiliares */
concat_head(Elemento, [Head | Tail], [ Elemento, Head | Tail]).
concat_tail(L1, L2, [L2 | L1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Fluxo de controle principal */
posso_continuar(Regra, [Atr, Valor],  Valor, GrauCerteza, Diagnostico) :-
    limiar_de_exatidao(L),
    GrauCerteza > L,
    !, fail.
posso_continuar(Regra, _, _, _, Diagnostico) :-
    assertz(invalida(Regra)),
    escreve_adeus(Diagnostico),
    !, fail.

posso_ir_adiante(Diagnostico, Regra, [Atr, Valor]) :-
    not(invalida(Regra)),
    base_de_fatos(BF),
    existe_informacao(BF, [], [Atr, N1]),
    not(resolvido(BF, Diagnostico, [Atr, Valor])),
    escreva('>> Descobrindo sobre ', Atr),
    escreve_adeus(Diagnostico), assertz(invalida(Regra)), !, fail.
posso_ir_adiante(_, Regra, [Atr, Valor]) :-
    not(invalida(Regra)),
    escreva('>> Descobrindo sobre ', Atr),
    base_de_fatos(BF),
    not(existe_informacao(BF, [], [Atr, N1])).               

posso_perguntar(Regra, Clausula) :-
    base_de_fatos(BF),
    not(existe_informacao(BF, Regra, Clausula)).

tenta_concluir(_, [], _, _, 1).
tenta_concluir(Regra, Premissas, Prob, Conclusao, PR) :-
    base_de_fatos(BF),
    extrai_clausula_premissa(Premissas, Premissas, Clausula, Resto),
    procura_premissas(BF, Clausula, Premissas, Prob, ProbTotal),
    tenta_concluir(Regra, Resto, ProbTotal, Conclusao, PR2),
    PR is ProbTotal * PR2.    

possivel_concluir(Regra) :-
    retractall(invalida_premissa(_)),
    premissas(Regra, Premissas),
    extrai_prob(Regra, Prob),
    extrai_conclusao(Regra, [Atr, Valor]),
    tenta_concluir(Regra, Premissas, Prob, [Atr, Valor], ProbTotal),
    limiar_de_exatidao(L),
    ProbTotal > L,
    ProbR is ProbTotal * Prob,
    insere_fato(Regra, [Atr, Valor], Valor, ProbR), !.
possivel_concluir(R).


porque(porque, Regra, Clausula, GrauCerteza, R2) :-
    escreva_justificativa(Regra),
    !,
    questiona(Clausula, R2, GrauCerteza, Regra).
porque(Resposta, Regra, Clausula, GrauCerteza, Resposta) :- 
    write('Qual eh o seu grau de exatidao (um numero entre -1 e 1) ?'),
    read(GrauCerteza),
    nl, !.
    
questiona([Atributo, Variavel], Resposta, GrauCerteza, Regra) :-
    escreve_pergunta(Atributo),
    read(R2),
    porque(R2, Regra, [Atributo, Variavel], GrauCerteza, Resposta).

tem_regra(_, [Atributo, Valor], Resposta, GrauCerteza) :-
    diagnostica(Atributo),
    !.
tem_regra(Regra, Clausula, Resposta, GrauCerteza) :-
    posso_perguntar(Regra, Clausula),
    !,
    questiona(Clausula, Resposta, GrauCerteza, Regra).
tem_regra(Regra, Clausula, Resposta, GrauCerteza) :-
    base_de_fatos(BF),
    extrai_infos(BF, Clausula, [Resposta, GrauCerteza]).

tem_atributo([Diagnostico, _], Diagnostico).
checa_aplicabilidade(Regra, Diagnostico) :-
    extrai_conclusao(Regra, Clausula),
    tem_atributo(Clausula, Diagnostico).

itera_na_regra(Regra, Diagnostico) :-
    escreva_regra('>> Tentando usar a regra ', Regra),
    premissas(Regra, Premissas),
    extrai_clausula(Regra, Premissas, Clausula, Diagnostico),
    posso_ir_adiante(Diagnostico, Regra, Clausula),
    tem_regra(Regra, Clausula, Resposta, GrauCerteza),
    insere_fato(Regra, Clausula, Resposta, GrauCerteza),
    posso_continuar(Regra, Clausula, Resposta, GrauCerteza, Diagnostico).

diagnostica(Diagnostico) :-
    base_de_regras(Regras),
    extrai_regra(Regras, Regra),
    checa_aplicabilidade(Regra, Diagnostico),
    itera_na_regra(Regra, Diagnostico).

diagnostico(Diagnostico) :-
    /* Limpa base de fatos */
    retractall(base_de_fatos(_)),
    retractall(invalida(_)),
    retractall(primeiro),
    /* Inicializa o vetor de fatos */
    assertz(base_de_fatos([])),
    nl,
    escreva('>> Descobrindo sobre ', Diagnostico),
    not(diagnostica(Diagnostico)),
    pula_linha(nl),
    base_de_fatos(BF),
    escreve_hipoteses(BF, Diagnostico).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
